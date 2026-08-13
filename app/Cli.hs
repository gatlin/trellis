{- |
Module: Cli
Description: Parses "--in"/"--out" command-line flags for pre-populating
cells with a live subscription, or publishing a cell's value out.
-}
module Cli (
  CliOptions (..),
  parseArgs,
  parseCoordPath,
  parseCoord,
) where

{- | Everything gathered from argv: which cells to pre-bind to a
@!tail@ subscription ("--in"), and which cells to publish out to a
pipe\/file whenever their value changes ("--out").
-}
data CliOptions = CliOptions
  { cliIns :: [((Int, Int), FilePath)]
  , cliOuts :: [((Int, Int), FilePath)]
  }
  deriving (Eq, Show)

{- | Parses argv; "--in"\/"--out" each consume the next token as
@COL,ROW=PATH@. Anything else - missing value, bad coordinate, unknown
flag - is one human-readable 'Left', not a crash.
-}
parseArgs :: [String] -> Either String CliOptions
parseArgs = go (CliOptions [] [])
 where
  go opts [] = Right opts
  go opts ("--in" : val : rest) = case parseCoordPath val of
    Just cp -> go opts{cliIns = cliIns opts ++ [cp]} rest
    Nothing -> Left ("--in: malformed COL,ROW=PATH: " ++ val)
  go _ ["--in"] = Left "--in: missing COL,ROW=PATH argument"
  go opts ("--out" : val : rest) = case parseCoordPath val of
    Just cp -> go opts{cliOuts = cliOuts opts ++ [cp]} rest
    Nothing -> Left ("--out: malformed COL,ROW=PATH: " ++ val)
  go _ ["--out"] = Left "--out: missing COL,ROW=PATH argument"
  go _ (arg : _) = Left ("unrecognized argument: " ++ arg)

-- | @"0,0=/tmp/pipe"@ -> @Just ((0,0), "\/tmp\/pipe")@.
parseCoordPath :: String -> Maybe ((Int, Int), FilePath)
parseCoordPath s = case break (== '=') s of
  (coordStr, '=' : path) | not (null path) -> do
    coord <- parseCoord coordStr
    Just (coord, path)
  _ -> Nothing

{- | @"0,0"@ -> @Just (0,0)@ - same @(col,row)@ order 'Formula.RefF' and
@\@x,y@ formula syntax already use.
-}
parseCoord :: String -> Maybe (Int, Int)
parseCoord s = case break (== ',') s of
  (colStr, ',' : rowStr) -> case (reads colStr, reads rowStr) of
    ([(col, "")], [(row, "")]) -> Just (col, row)
    _ -> Nothing
  _ -> Nothing
