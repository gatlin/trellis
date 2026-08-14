{- |
Module: Keymap
Description: Configurable keybindings, with documented defaults.

Every key "Update" looks for comes from here, not a literal 'Tb2.Tb2Key'
scattered through the code - one place that knows what a keystroke means,
changeable without touching code. [^1] [^2]
-}
module Keymap (
  BaseKey (..),
  Binding (..),
  MouseBinding (..),
  KeyMap (..),
  defaultKeyMap,
  loadKeyMap,
  matches,
  matchesMouse,
) where

import Control.Monad (foldM)
import Data.Bits ((.&.))
import Data.Char (isSpace)
import Data.List (isPrefixOf, stripPrefix)
import System.Directory (
  XdgDirectory (XdgConfig),
  createDirectoryIfMissing,
  doesFileExist,
  getXdgDirectory,
 )
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import qualified Termbox2 as Tb2

-- | The part of a binding that isn't about whether Alt is held.
data BaseKey
  = -- | A named key: an arrow, Enter, Delete, and so on.
    Key Tb2.Tb2Key
  | {- | An ordinary typed character, case-sensitive - @Char 'k'@ and
    @Char 'K'@ differ, like vim's @j@ vs @J@.
    -}
    Char Char
  deriving (Eq, Show)

-- | A 'BaseKey', optionally held down together with Alt.
data Binding = Plain BaseKey | WithAlt BaseKey
  deriving (Eq, Show)

-- | Does an incoming event match a configured binding?
matches :: Binding -> Tb2.Tb2Event -> Bool
matches binding evt = base baseKey evt && altHeld == eventHasAlt
 where
  (baseKey, altHeld) = case binding of
    Plain b -> (b, False)
    WithAlt b -> (b, True)
  eventHasAlt = Tb2._mod evt .&. Tb2.modAlt /= 0
  base (Key k) e = Tb2._type e == Tb2.eventKey && Tb2._key e == k
  base (Char c) e =
    Tb2._type e == Tb2.eventKey
      && Tb2._key e == 0
      && Tb2._ch e == fromIntegral (fromEnum c)

-- | A mouse button, optionally required to be held together with Ctrl.
data MouseBinding = MouseBinding
  { mouseKey :: Tb2.Tb2Key
  , mouseCtrl :: Bool
  }
  deriving (Eq, Show)

-- | Does an incoming mouse event match a configured 'MouseBinding'?
matchesMouse :: MouseBinding -> Tb2.Tb2Event -> Bool
matchesMouse (MouseBinding key ctrl) evt =
  Tb2._type evt == Tb2.eventMouse
    && Tb2._key evt == key
    && ctrl == (Tb2._mod evt .&. Tb2.modCtrl /= 0)

data KeyMap = KeyMap
  { moveUp, moveDown, moveLeft, moveRight :: Binding
  , scrollUp, scrollDown :: MouseBinding
  , selectButton :: MouseBinding
  -- ^ Which mouse gesture selects\/drags a cell.
  , panButton :: MouseBinding
  -- ^ Which mouse gesture, held and dragged, pans instead of moving the cursor.
  , fillButton :: MouseBinding
  {- ^ Which mouse gesture, held and dragged, fills the dragged range with
  the source cell's formula, references adjusted to match.
  -}
  , confirm :: Binding
  -- ^ Starts an edit when navigating; commits it when editing.
  , cancel :: Binding
  -- ^ Discards an in-progress edit, or clears a selection\/open chart.
  , clearCell :: Binding
  -- ^ Clears the focused cell's content directly, without editing.
  , barChartKey, lineChartKey, heatmapKey :: Binding
  {- ^ Toggle a bar\/line\/heatmap chart over the current selection - the
  same key again closes it; a different one switches, if the selection
  qualifies (see 'SheetState.classifyChartRange').
  -}
  , pivotKey :: Binding
  {- ^ Toggles a pivot-table summary (count\/sum\/average per distinct
  category) over a selected two-column range - the same key again closes
  it; unlike the three chart keys there's only one pivot "type", so
  there's no "switch" behavior to mirror (see
  'SheetState.classifyPivotRange').
  -}
  , saveKey :: Binding
  -- ^ Writes the sheet back to the file it was loaded from, if any.
  , selectUp, selectDown, selectLeft, selectRight :: Binding
  {- ^ Extend a selection from the keyboard - alongside Shift+Arrow, which
  isn't itself configurable (see "Update.Events.isShiftKey"). Some
  terminals\/multiplexers\/SSH hops don't relay Shift held on an arrow key
  but do relay Alt, so this is a second, independent path to the same
  gesture, not a replacement.
  -}
  }

{- | Arrows navigate, the wheel zooms, left-click selects\/drags,
middle-click-drag pans, right-click-drag fills a range with the source
cell's formula (adjusted), @b@\/@l@\/@h@ toggle a bar\/line\/heatmap
chart over the selection, @p@ toggles a pivot-table summary over it,
Enter starts\/commits an edit, Escape cancels, Delete clears a cell.
Kept in sync with 'defaultConfigText' by hand.
-}
defaultKeyMap :: KeyMap
defaultKeyMap =
  KeyMap
    { moveUp = Plain (Key Tb2.keyArrowUp)
    , moveDown = Plain (Key Tb2.keyArrowDown)
    , moveLeft = Plain (Key Tb2.keyArrowLeft)
    , moveRight = Plain (Key Tb2.keyArrowRight)
    , scrollUp = MouseBinding Tb2.keyMouseWheelUp False
    , scrollDown = MouseBinding Tb2.keyMouseWheelDown False
    , selectButton = MouseBinding Tb2.keyMouseLeft False
    , panButton = MouseBinding Tb2.keyMouseMiddle False
    , fillButton = MouseBinding Tb2.keyMouseRight False
    , confirm = Plain (Key Tb2.keyCtrlEnter)
    , cancel = Plain (Key Tb2.keyCtrlEsc)
    , clearCell = Plain (Key Tb2.keyDelete)
    , barChartKey = Plain (Char 'b')
    , lineChartKey = Plain (Char 'l')
    , heatmapKey = Plain (Char 'h')
    , pivotKey = Plain (Char 'p')
    , saveKey = Plain (Key Tb2.keyCtrlS)
    , selectUp = WithAlt (Key Tb2.keyArrowUp)
    , selectDown = WithAlt (Key Tb2.keyArrowDown)
    , selectLeft = WithAlt (Key Tb2.keyArrowLeft)
    , selectRight = WithAlt (Key Tb2.keyArrowRight)
    }

-- | The named keys a config file can refer to, beyond a bare character.
namedKeys :: [(String, Tb2.Tb2Key)]
namedKeys =
  [ ("ArrowUp", Tb2.keyArrowUp)
  , ("ArrowDown", Tb2.keyArrowDown)
  , ("ArrowLeft", Tb2.keyArrowLeft)
  , ("ArrowRight", Tb2.keyArrowRight)
  , ("WheelUp", Tb2.keyMouseWheelUp)
  , ("WheelDown", Tb2.keyMouseWheelDown)
  , ("MouseLeft", Tb2.keyMouseLeft)
  , ("MouseRight", Tb2.keyMouseRight)
  , ("MouseMiddle", Tb2.keyMouseMiddle)
  , ("Enter", Tb2.keyCtrlEnter)
  , ("Escape", Tb2.keyCtrlEsc)
  , ("Delete", Tb2.keyDelete)
  , ("Tab", Tb2.keyCtrlTab)
  , ("Space", Tb2.keySpace)
  , ("Home", Tb2.keyHome)
  , ("End", Tb2.keyEnd)
  , ("PageUp", Tb2.keyPgUp)
  , ("PageDown", Tb2.keyPgDn)
  , ("Ctrl+S", Tb2.keyCtrlS)
  ]

{- | Maps a config setting name to the 'KeyMap' field it updates (full
'Binding' fields only).
-}
bindingSetters :: [(String, Binding -> KeyMap -> KeyMap)]
bindingSetters =
  [ ("moveUp", \k m -> m{moveUp = k})
  , ("moveDown", \k m -> m{moveDown = k})
  , ("moveLeft", \k m -> m{moveLeft = k})
  , ("moveRight", \k m -> m{moveRight = k})
  , ("confirm", \k m -> m{confirm = k})
  , ("cancel", \k m -> m{cancel = k})
  , ("clearCell", \k m -> m{clearCell = k})
  , ("barChartKey", \k m -> m{barChartKey = k})
  , ("lineChartKey", \k m -> m{lineChartKey = k})
  , ("heatmapKey", \k m -> m{heatmapKey = k})
  , ("pivotKey", \k m -> m{pivotKey = k})
  , ("saveKey", \k m -> m{saveKey = k})
  , ("selectUp", \k m -> m{selectUp = k})
  , ("selectDown", \k m -> m{selectDown = k})
  , ("selectLeft", \k m -> m{selectLeft = k})
  , ("selectRight", \k m -> m{selectRight = k})
  ]

-- | Same, for the mouse-only fields that take a 'MouseBinding'.
mouseKeySetters :: [(String, MouseBinding -> KeyMap -> KeyMap)]
mouseKeySetters =
  [ ("scrollUp", \k m -> m{scrollUp = k})
  , ("scrollDown", \k m -> m{scrollDown = k})
  , ("selectButton", \k m -> m{selectButton = k})
  , ("panButton", \k m -> m{panButton = k})
  , ("fillButton", \k m -> m{fillButton = k})
  ]

{- | A name from 'namedKeys', or - since letters have no named key - a
bare character, tried in that order so a config can't shadow a named key.
-}
parseBase :: String -> Maybe BaseKey
parseBase s = case lookup s namedKeys of
  Just k -> Just (Key k)
  Nothing -> case s of
    [c] -> Just (Char c)
    _ -> Nothing

-- | A base key name, or @Alt+@ followed by one.
parseBinding :: String -> Maybe Binding
parseBinding s = case stripPrefix "Alt+" s of
  Just rest -> WithAlt <$> parseBase rest
  Nothing -> Plain <$> parseBase s

-- | A named key from 'namedKeys', or @Ctrl+@ followed by one.
parseMouseBinding :: String -> Maybe MouseBinding
parseMouseBinding s = case stripPrefix "Ctrl+" s of
  Just rest -> (`MouseBinding` True) <$> lookup rest namedKeys
  Nothing -> (`MouseBinding` False) <$> lookup s namedKeys

trim :: String -> String
trim = f . f
 where
  f = reverse . dropWhile isSpace

{- | A blank line, or one starting with @#@, is a comment; otherwise
@setting = KeyName@, whitespace around each side ignored.
-}
parseLine :: String -> Maybe (String, String)
parseLine raw
  | null ln || "#" `isPrefixOf` ln = Nothing
  | otherwise = case break (== '=') ln of
      (field, '=' : value) -> Just (trim field, trim value)
      _ -> Nothing
 where
  ln = trim raw

configPath :: IO FilePath
configPath = do
  dir <- getXdgDirectory XdgConfig "trellis"
  createDirectoryIfMissing True dir
  pure (dir </> "keybindings")

defaultConfigText :: String
defaultConfigText =
  unlines
    [ "# Trellis keybindings. Edit and restart to apply."
    , "# Blank lines and lines starting with # are ignored."
    , "#"
    , "# A value is either a named key or a single character - e.g. moveUp"
    , "# could be set to ArrowUp, or to k for a vim-style hjkl, or to w for"
    , "# wasd. Prefix a *named key* with Alt+ to require Alt held too, e.g."
    , "# Alt+ArrowUp - but not a plain character (Alt+k): terminals can't"
    , "# reliably tell that apart from Escape-then-k without breaking Escape"
    , "# itself, so it's rejected rather than silently never firing."
    , "# Named keys: ArrowUp, ArrowDown, ArrowLeft, ArrowRight, WheelUp,"
    , "# WheelDown, MouseLeft, MouseRight, MouseMiddle, Enter, Escape,"
    , "# Delete, Tab, Space, Home, End, PageUp, PageDown, Ctrl+S."
    , "#"
    , "# scrollUp/scrollDown zoom the grid in and out. selectButton,"
    , "# panButton, fillButton: mouse-only settings may additionally be"
    , "# prefixed with Ctrl+ (e.g. Ctrl+MouseLeft), unlike a named key that"
    , "# can't - a mouse button and Ctrl always arrive together in one"
    , "# event, so there's no ambiguity to worry about the way there is"
    , "# with Alt and a keyboard Escape. selectButton drags to move the"
    , "# cursor; panButton, held and dragged, pans the viewport instead;"
    , "# fillButton, held and dragged, replicates the source cell's formula"
    , "# across the dragged range, with references adjusted to match. Give"
    , "# each a different gesture or one will always win over the others."
    , "#"
    , "# barChartKey/lineChartKey/heatmapKey toggle a chart over the current"
    , "# selection - press the same one again to close it, or a different"
    , "# one to switch, as long as the selection still qualifies."
    , "#"
    , "# pivotKey toggles a pivot-table summary over a selected two-column"
    , "# range - a category column and a value column - showing count/sum/"
    , "# average per distinct category. Press it again to close; there's no"
    , "# second pivot \"type\" to switch to."
    , "#"
    , "# saveKey writes the sheet back to the file it was loaded from, if"
    , "# any - a no-op if Trellis wasn't started with a file."
    , "#"
    , "# selectUp/selectDown/selectLeft/selectRight extend a selection from"
    , "# the keyboard, same as Shift+Arrow (which isn't itself configurable)"
    , "# - a second path to the same gesture for a terminal that won't"
    , "# relay Shift held on an arrow key but will relay Alt."
    , ""
    , "moveUp = ArrowUp"
    , "moveDown = ArrowDown"
    , "moveLeft = ArrowLeft"
    , "moveRight = ArrowRight"
    , "scrollUp = WheelUp"
    , "scrollDown = WheelDown"
    , "selectButton = MouseLeft"
    , "panButton = MouseMiddle"
    , "fillButton = MouseRight"
    , "confirm = Enter"
    , "cancel = Escape"
    , "clearCell = Delete"
    , "barChartKey = b"
    , "lineChartKey = l"
    , "heatmapKey = h"
    , "pivotKey = p"
    , "saveKey = Ctrl+S"
    , "selectUp = Alt+ArrowUp"
    , "selectDown = Alt+ArrowDown"
    , "selectLeft = Alt+ArrowLeft"
    , "selectRight = Alt+ArrowRight"
    ]

{- | Reads the keybindings config file (creating it from
'defaultConfigText' if missing), applying each @setting = value@ line as
an override of 'defaultKeyMap'. Unrecognised settings warn to stderr.
-}
loadKeyMap :: IO KeyMap
loadKeyMap = do
  path <- configPath
  exists <- doesFileExist path
  if exists
    then do
      contents <- readFile path
      foldM applyLine defaultKeyMap (zip [1 :: Int ..] (lines contents))
    else do
      writeFile path defaultConfigText
      pure defaultKeyMap
 where
  applyLine km (lineNo, raw) = case parseLine raw of
    Nothing -> pure km
    Just (field, value) -> case lookup field bindingSetters of
      Just setter -> case parseBinding value of
        Just (WithAlt (Char c)) -> do
          warn
            lineNo
            ( "Alt+"
                ++ [c]
                ++ " isn't supported - termbox2 can't reliably tell it apart"
                ++ " from a lone Escape without hanging Escape itself; see"
                ++ " the module docs. Alt+<named key> (e.g. Alt+ArrowUp) is"
                ++ " fine. Leaving "
                ++ field
                ++ " unchanged."
            )
          pure km
        Just b -> pure (setter b km)
        Nothing ->
          warn lineNo ("unknown key value " ++ show value) >> pure km
      Nothing -> case lookup field mouseKeySetters of
        Just setter -> case parseMouseBinding value of
          Just b -> pure (setter b km)
          Nothing ->
            warn lineNo ("unknown key value " ++ show value) >> pure km
        Nothing -> warn lineNo ("unknown setting " ++ show field) >> pure km
  warn lineNo msg =
    hPutStrLn
      stderr
      ("trellis: keybindings line " ++ show lineNo ++ ": " ++ msg)

{- [^1]:
Alt only works combined with a named key - termbox2 recognises e.g.
Alt+ArrowUp as one unambiguous escape sequence. Alt+<letter> is rejected at
load time instead of silently never firing: seeing it needs termbox2's
TB_INPUT_ALT mode, which has no timeout, so a lone Escape (needed for
'cancel') then never resolves into an event - confirmed empirically. The
two input modes are mutually exclusive.
-}

{- [^2]:
'Tb2.keyBackspace'\/'keyBackspace2' (two wire encodings of the same key) and
'Tb2.keyMouseRelease' (termbox2 doesn't say which button released) are
deliberately unbindable. Mouse-only settings use 'MouseBinding', not
'Binding', since termbox2 never reports a character for a mouse event.
Ctrl is fair game there (unlike Alt/Shift) because it arrives in the same
wire event as the button, never a separate keystroke - this needed a small
patch to termbox2's SGR\/urxvt mouse parsing, which silently dropped the
Ctrl bit on the floor.
-}
