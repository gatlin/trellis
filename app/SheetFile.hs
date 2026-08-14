{- |
Module: SheetFile
Description: Parses and renders the plain-text sheet file format - one
declaration per line, in the same shape a cell's own typed text
already has.

@
# a trellis sheet
0,0=10
1,0=\@0,0+1
2,0=!5s date
OUT 3,0=\/tmp\/status.fifo
@
-}
module SheetFile (
  FileEntry (..),
  parseSheetFile,
  serialize,
) where

import Cli (parseCoordPath)
import Data.Char (isSpace)
import Data.List (isPrefixOf, nub, sort, stripPrefix)
import qualified Data.Map.Strict as Map
import Formula (blank, renderExpr)
import SheetState (LiveBinding (..), SheetState (..))

-- | One line's worth of a sheet file, once parsed.
data FileEntry
  = {- | A cell's own typed text - could be a formula or a "!"-prefixed
    live spec, undecided here; telling them apart needs
    'Live.parseLiveSpec' and 'Parser.parseExpr', and declaring a live
    spec needs an 'Trellis.Orc.Group' to run it under, neither available
    to this pure module - see "Main" for where that actually happens.
    -}
    CellEntry (Int, Int) String
  | OutEntry (Int, Int) FilePath
  deriving (Eq, Show)

{- | Parses a sheet file's lines. Blank lines and "#" comments are
skipped, matching "Keymap"'s config file; anything else that doesn't
parse is a warning, not a hard failure - one bad line shouldn't cost
you the rest of the sheet, matching 'Keymap.loadKeyMap's own
unrecognized-setting handling.
-}
parseSheetFile :: String -> ([String], [FileEntry])
parseSheetFile text = foldr step ([], []) (zip [1 :: Int ..] (lines text))
 where
  step (lineNo, raw) acc@(warnings, entries)
    | null ln || "#" `isPrefixOf` ln = acc
    | otherwise = case parseLine ln of
        Just entry -> (warnings, entry : entries)
        Nothing ->
          ( ("line " ++ show lineNo ++ ": malformed: " ++ raw)
              : warnings
          , entries
          )
   where
    ln = trim raw

{- | An @OUT@-prefixed line, or an ordinary cell line - both are just
'Cli.parseCoordPath', which already keeps everything after the
*first* @=@ as the value, so a formula\/path containing its own
@=@ still round-trips.
-}
parseLine :: String -> Maybe FileEntry
parseLine ln
  | Just rest <- stripPrefix "OUT " ln =
      uncurry OutEntry <$> parseCoordPath rest
  | otherwise = uncurry CellEntry <$> parseCoordPath ln

trim :: String -> String
trim = f . f
 where
  f = reverse . dropWhile isSpace

{- | The inverse: every populated cell (formula or live spec) and every
out-binding, one line each, sorted so the file diffs sensibly between
saves.
-}
serialize :: SheetState -> String
serialize st =
  unlines $
    [showCoord pos ++ "=" ++ cellText pos | pos <- cellPositions]
      ++ [ "OUT " ++ showCoord pos ++ "=" ++ path
         | (pos, path) <- Map.toList (outBindings st)
         ]
 where
  cellPositions =
    sort (nub (Map.keys (cells st) ++ Map.keys (subscriptions st)))
  -- \| Live spec text if subscribed, its rendered formula otherwise -
  -- the same check 'Update.Subscriptions.existingText' does, kept
  -- local here rather than imported (see the module doc above).
  cellText pos = case Map.lookup pos (subscriptions st) of
    Just b -> liveSpecText b
    Nothing -> renderExpr (Map.findWithDefault blank pos (cells st))
  showCoord (x, y) = show x ++ "," ++ show y
