{- |
Module: Keymap
Description: Configurable keybindings, with documented defaults.

Every key "Update" looks for comes from here, not a literal 'UI.Key'
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
  namedCtrlKey,
  showBinding,
  showMouseBinding,
  namedKeyName,
) where

import Control.Exception (IOException, try)
import Control.Monad (foldM)
import Data.Bits ((.&.))
import Data.Char (isSpace, toLower)
import Data.List (isPrefixOf, stripPrefix)
import Data.Tuple (swap)
import System.Directory (
  XdgDirectory (XdgConfig),
  createDirectoryIfMissing,
  doesFileExist,
  getXdgDirectory,
 )
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import qualified Trellis.UI as UI

-- | The part of a binding that isn't about whether Alt is held.
data BaseKey
  = -- | A named key: an arrow, Enter, Delete, and so on.
    Key UI.Key
  | {- | An ordinary typed character, case-sensitive - @Char 'k'@ and
    @Char 'K'@ differ, like vim's @j@ vs @J@.
    -}
    Char Char
  deriving (Eq, Show)

-- | A 'BaseKey', optionally held down together with Alt or Ctrl.
data Binding = Plain BaseKey | WithAlt BaseKey | WithCtrl BaseKey
  deriving (Eq, Show)

-- | Does an incoming event match a configured binding?
matches :: Binding -> UI.InputEvent -> Bool
matches binding evt = base baseKey evt && altOk && ctrlOk
 where
  (baseKey, wantAlt, wantCtrl) = case binding of
    Plain b -> (b, False, False)
    WithAlt b -> (b, True, False)
    WithCtrl b -> (b, False, True)
  altOk = wantAlt == (UI.evtMod evt .&. UI.modAlt /= 0)
  {- \| Arrow keys are the one case where this check is actually needed:
  termbox2 reports Ctrl+Up via the *same* _key as plain Up, distinguished
  solely by _mod (per its own header comment - "TB_MOD_CTRL ... only set
  as modifiers to TB_KEY_ARROW_*"). Every other named "Ctrl+X" key
  (keyCtrlS, keyCtrlEnter, keyCtrlEsc, keyCtrlTab, ...) already encodes
  the modifier in its own distinct key constant - but termbox2 *also*
  sets TB_MOD_CTRL alongside those (confirmed empirically: a real
  Ctrl+S press arrives as key=keyCtrlS *with* mod=modCtrl set, despite
  that same header comment), so requiring mod==0 for a 'Plain' binding
  on one of those silently broke every one of them - 'saveKey' (Ctrl+S)
  never actually wrote to disk.
  -}
  {- \| 'WithCtrl (Char c)' has the same problem from the other side: for
  any letter, termbox2 *also* reports Ctrl+<letter> as its own distinct
  named key (keyCtrlD, keyCtrlR, ...), not as @(_ch = <letter>, _mod =
  modCtrl)@ the way this binding assumed - so e.g. 'fillKey' (Ctrl+D)
  could never match a real keypress either, the same silent-no-op as
  'saveKey' before it. Where a named key exists, 'base' matches against
  it directly and this bypasses the (also redundantly-set) mod bit, the
  same reasoning as above; letters with no named equivalent fall back to
  the original ch+mod check, unchanged.
  -}
  ctrlOk
    | isArrowKey baseKey = wantCtrl == (UI.evtMod evt .&. UI.modCtrl /= 0)
    | Char c <- baseKey, wantCtrl, Just _ <- namedCtrlKey c = True
    | not wantCtrl = True
    | otherwise = wantCtrl == (UI.evtMod evt .&. UI.modCtrl /= 0)
  isArrowKey (Key k) =
    k
      `elem` [ UI.keyArrowUp
             , UI.keyArrowDown
             , UI.keyArrowLeft
             , UI.keyArrowRight
             ]
  isArrowKey (Char _) = False
  base (Key k) e = UI.evtType e == UI.eventKey && UI.evtKey e == k
  base (Char c) e
    | wantCtrl, Just k <- namedCtrlKey c =
        UI.evtType e == UI.eventKey && UI.evtKey e == k
    | otherwise =
        UI.evtType e == UI.eventKey
          && UI.evtKey e == UI.keyNone
          && UI.evtCh e == fromIntegral (fromEnum c)

{- | The named 'UI.Key' termbox2 reports for Ctrl+@c@, for the
letters where one exists - covers the whole alphabet, not just
'fillKey'\/'fillKeyAlt's default "d"\/"r", so a user's own remapped
config can bind Ctrl+ any letter and still work. Case-insensitive, since
a binding's 'Char' is the unshifted key, not literally what Ctrl+Shift+D
would send.
-}
namedCtrlKey :: Char -> Maybe UI.Key
namedCtrlKey c = lookup (toLower c) table
 where
  table =
    [ ('a', UI.keyCtrlA)
    , ('b', UI.keyCtrlB)
    , ('c', UI.keyCtrlC)
    , ('d', UI.keyCtrlD)
    , ('e', UI.keyCtrlE)
    , ('f', UI.keyCtrlF)
    , ('g', UI.keyCtrlG)
    , ('h', UI.keyCtrlH)
    , ('i', UI.keyCtrlI)
    , ('j', UI.keyCtrlJ)
    , ('k', UI.keyCtrlK)
    , ('l', UI.keyCtrlL)
    , ('m', UI.keyCtrlM)
    , ('n', UI.keyCtrlN)
    , ('o', UI.keyCtrlO)
    , ('p', UI.keyCtrlP)
    , ('q', UI.keyCtrlQ)
    , ('r', UI.keyCtrlR)
    , ('s', UI.keyCtrlS)
    , ('t', UI.keyCtrlT)
    , ('u', UI.keyCtrlU)
    , ('v', UI.keyCtrlV)
    , ('w', UI.keyCtrlW)
    , ('x', UI.keyCtrlX)
    , ('y', UI.keyCtrlY)
    , ('z', UI.keyCtrlZ)
    ]

-- | A mouse button, optionally required to be held together with Ctrl.
data MouseBinding = MouseBinding
  { mouseKey :: UI.Key
  , mouseCtrl :: Bool
  }
  deriving (Eq, Show)

-- | Does an incoming mouse event match a configured 'MouseBinding'?
matchesMouse :: MouseBinding -> UI.InputEvent -> Bool
matchesMouse (MouseBinding key ctrl) evt =
  UI.evtType evt == UI.eventMouse
    && UI.evtKey evt == key
    && ctrl == (UI.evtMod evt .&. UI.modCtrl /= 0)

data KeyMap = KeyMap
  { moveUp, moveDown, moveLeft, moveRight :: Binding
  , tabKey :: Binding
  {- ^ Tab moves right (next cell); Shift+Tab moves left (previous cell).
  Shift+Tab is not itself configurable - see the Shift+Arrow note.
  -}
  , scrollUp, scrollDown :: MouseBinding
  , zoomInKey, zoomOutKey, zoomResetKey :: Binding
  {- ^ Keyboard equivalents of the scroll-wheel zoom: in, out, and reset
  to the initial (unzoomed) cell width.
  -}
  , pageUp, pageDown :: Binding
  {- ^ Pan the viewport up or down by one visible page, the keyboard
  equivalent of middle-click-drag panning.
  -}
  , panUp, panDown, panLeft, panRight :: Binding
  {- ^ Pan the viewport by one visible page in the given direction,
  the keyboard equivalent of middle-click-drag panning.
  -}
  , growColKey, shrinkColKey :: Binding
  -- ^ Widen\/narrow the cursor's current column, relative to the current zoom.
  , growRowKey, shrinkRowKey :: Binding
  -- ^ Heighten\/shorten the cursor's current row, relative to the current zoom.
  , selectButton :: MouseBinding
  -- ^ Which mouse gesture selects\/drags a cell.
  , panButton :: MouseBinding
  -- ^ Which mouse gesture, held and dragged, pans instead of moving the cursor.
  , fillButton :: MouseBinding
  {- ^ Which mouse gesture, held and dragged, fills the dragged range with
  the source cell's formula, references adjusted to match.
  -}
  , fillKey, fillKeyAlt :: Binding
  {- ^ Keyboard equivalents of the fill gesture: replicate the current
  selection's source across the selection. Two keys for convenience.
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
  , saveKey :: Binding
  -- ^ Writes the sheet back to the file it was loaded from, if any.
  , editKey :: Binding
  -- ^ Opens the formula editor for the focused cell (same as 'confirm').
  , helpKey :: Binding
  -- ^ Opens the help modal listing all keybindings.
  }

{- | Arrows navigate, the wheel zooms, left-click selects\/drags,
middle-click-drag pans, right-click-drag fills a range with the source
cell's formula (adjusted), @b@\/@l@\/@h@ toggle a bar\/line\/heatmap
chart over the selection, Enter starts\/commits an edit, Escape cancels,
Delete clears a cell. Kept in sync with 'defaultConfigText' by hand.
-}
defaultKeyMap :: KeyMap
defaultKeyMap =
  KeyMap
    { moveUp = Plain (Key UI.keyArrowUp)
    , moveDown = Plain (Key UI.keyArrowDown)
    , moveLeft = Plain (Key UI.keyArrowLeft)
    , moveRight = Plain (Key UI.keyArrowRight)
    , tabKey = Plain (Key UI.keyCtrlTab)
    , scrollUp = MouseBinding UI.keyMouseWheelUp False
    , scrollDown = MouseBinding UI.keyMouseWheelDown False
    , zoomInKey = Plain (Char '=')
    , zoomOutKey = Plain (Char '-')
    , zoomResetKey = Plain (Char '0')
    , pageUp = Plain (Key UI.keyPgUp)
    , pageDown = Plain (Key UI.keyPgDn)
    , panUp = WithCtrl (Key UI.keyArrowUp)
    , panDown = WithCtrl (Key UI.keyArrowDown)
    , panLeft = WithCtrl (Key UI.keyArrowLeft)
    , panRight = WithCtrl (Key UI.keyArrowRight)
    , growColKey = Plain (Char ']')
    , shrinkColKey = Plain (Char '[')
    , growRowKey = Plain (Char '}')
    , shrinkRowKey = Plain (Char '{')
    , selectButton = MouseBinding UI.keyMouseLeft False
    , panButton = MouseBinding UI.keyMouseMiddle False
    , fillButton = MouseBinding UI.keyMouseRight False
    , fillKey = WithCtrl (Char 'd')
    , fillKeyAlt = WithCtrl (Char 'r')
    , confirm = Plain (Key UI.keyCtrlEnter)
    , cancel = Plain (Key UI.keyCtrlEsc)
    , clearCell = Plain (Key UI.keyDelete)
    , barChartKey = Plain (Char 'b')
    , lineChartKey = Plain (Char 'l')
    , heatmapKey = Plain (Char 'h')
    , saveKey = Plain (Key UI.keyCtrlS)
    , editKey = Plain (Key UI.keyF2)
    , helpKey = Plain (Char '?')
    }

-- | The named keys a config file can refer to, beyond a bare character.
namedKeys :: [(String, UI.Key)]
namedKeys =
  [ ("ArrowUp", UI.keyArrowUp)
  , ("ArrowDown", UI.keyArrowDown)
  , ("ArrowLeft", UI.keyArrowLeft)
  , ("ArrowRight", UI.keyArrowRight)
  , ("WheelUp", UI.keyMouseWheelUp)
  , ("WheelDown", UI.keyMouseWheelDown)
  , ("MouseLeft", UI.keyMouseLeft)
  , ("MouseRight", UI.keyMouseRight)
  , ("MouseMiddle", UI.keyMouseMiddle)
  , ("Enter", UI.keyCtrlEnter)
  , ("Escape", UI.keyCtrlEsc)
  , ("Delete", UI.keyDelete)
  , ("Tab", UI.keyCtrlTab)
  , ("Space", UI.keySpace)
  , ("Home", UI.keyHome)
  , ("End", UI.keyEnd)
  , ("PageUp", UI.keyPgUp)
  , ("PageDown", UI.keyPgDn)
  , ("Ctrl+S", UI.keyCtrlS)
  , ("F2", UI.keyF2)
  ]

{- | Reverse-lookup a 'UI.Key' in 'namedKeys', falling back to 'show'
for anything unlisted.
-}
namedKeyName :: UI.Key -> String
namedKeyName k = case lookup k (map swap namedKeys) of
  Just name -> name
  Nothing -> show k

{- | Render a 'Binding' as a human-readable string for display in the
help modal.
-}
showBinding :: Binding -> String
showBinding (Plain (Key k)) = namedKeyName k
showBinding (Plain (Char c)) = [c]
showBinding (WithAlt b) = "Alt+" ++ showBinding (Plain b)
showBinding (WithCtrl b) = "Ctrl+" ++ showBinding (Plain b)

{- | Render a 'MouseBinding' as a human-readable string.
-}
showMouseBinding :: MouseBinding -> String
showMouseBinding (MouseBinding k ctrl) =
  (if ctrl then "Ctrl+" else "") ++ namedKeyName k

{- | Maps a config setting name to the 'KeyMap' field it updates (full
'Binding' fields only).
-}
bindingSetters :: [(String, Binding -> KeyMap -> KeyMap)]
bindingSetters =
  [ ("moveUp", \k m -> m{moveUp = k})
  , ("moveDown", \k m -> m{moveDown = k})
  , ("moveLeft", \k m -> m{moveLeft = k})
  , ("moveRight", \k m -> m{moveRight = k})
  , ("tabKey", \k m -> m{tabKey = k})
  , ("zoomInKey", \k m -> m{zoomInKey = k})
  , ("zoomOutKey", \k m -> m{zoomOutKey = k})
  , ("zoomResetKey", \k m -> m{zoomResetKey = k})
  , ("pageUp", \k m -> m{pageUp = k})
  , ("pageDown", \k m -> m{pageDown = k})
  , ("panUp", \k m -> m{panUp = k})
  , ("panDown", \k m -> m{panDown = k})
  , ("panLeft", \k m -> m{panLeft = k})
  , ("panRight", \k m -> m{panRight = k})
  , ("growColKey", \k m -> m{growColKey = k})
  , ("shrinkColKey", \k m -> m{shrinkColKey = k})
  , ("growRowKey", \k m -> m{growRowKey = k})
  , ("shrinkRowKey", \k m -> m{shrinkRowKey = k})
  , ("fillKey", \k m -> m{fillKey = k})
  , ("fillKeyAlt", \k m -> m{fillKeyAlt = k})
  , ("confirm", \k m -> m{confirm = k})
  , ("cancel", \k m -> m{cancel = k})
  , ("clearCell", \k m -> m{clearCell = k})
  , ("barChartKey", \k m -> m{barChartKey = k})
  , ("lineChartKey", \k m -> m{lineChartKey = k})
  , ("heatmapKey", \k m -> m{heatmapKey = k})
  , ("saveKey", \k m -> m{saveKey = k})
  , ("editKey", \k m -> m{editKey = k})
  , ("helpKey", \k m -> m{helpKey = k})
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

-- | A base key name, or @Alt+@ / @Ctrl+@ followed by one.
parseBinding :: String -> Maybe Binding
parseBinding s = case stripPrefix "Alt+" s of
  Just rest -> WithAlt <$> parseBase rest
  Nothing -> case stripPrefix "Ctrl+" s of
    Just rest -> WithCtrl <$> parseBase rest
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
    , "# saveKey writes the sheet back to the file it was loaded from, if"
    , "# any - a no-op if Trellis wasn't started with a file."
    , "#"
    , "# growColKey/shrinkColKey and growRowKey/shrinkRowKey resize the"
    , "# cursor's current column/row, relative to the current zoom level -"
    , "# so a resize made at one zoom keeps its relative size after zooming"
    , "# in or out, rather than a fixed pixel size."
    , ""
    , "moveUp = ArrowUp"
    , "moveDown = ArrowDown"
    , "moveLeft = ArrowLeft"
    , "moveRight = ArrowRight"
    , "tabKey = Tab"
    , "scrollUp = WheelUp"
    , "scrollDown = WheelDown"
    , "zoomInKey = ="
    , "zoomOutKey = -"
    , "zoomResetKey = 0"
    , "pageUp = PageUp"
    , "pageDown = PageDown"
    , "panUp = Ctrl+ArrowUp"
    , "panDown = Ctrl+ArrowDown"
    , "panLeft = Ctrl+ArrowLeft"
    , "panRight = Ctrl+ArrowRight"
    , "growColKey = ]"
    , "shrinkColKey = ["
    , "growRowKey = }"
    , "shrinkRowKey = {"
    , "selectButton = MouseLeft"
    , "panButton = MouseMiddle"
    , "fillButton = MouseRight"
    , "fillKey = Ctrl+d"
    , "fillKeyAlt = Ctrl+r"
    , "confirm = Enter"
    , "cancel = Escape"
    , "clearCell = Delete"
    , "barChartKey = b"
    , "lineChartKey = l"
    , "heatmapKey = h"
    , "saveKey = Ctrl+S"
    , "editKey = F2"
    , "helpKey = ?"
    ]

{- | Reads the keybindings config file (creating it from
'defaultConfigText' if missing), applying each @setting = value@ line as
an override of 'defaultKeyMap'. Unrecognised settings warn to stderr.
Falls back to 'defaultKeyMap' on any I\/O failure reaching the config
file at all (not just "it doesn't exist yet") - a real, general
robustness gap, not a platform-specific one: under wasm32-wasi's minimal
in-browser filesystem there's no writable directory to create at all,
and 'configPath' unconditionally calls 'createDirectoryIfMissing' before
even checking whether the file exists, so an unguarded 'loadKeyMap'
would throw and crash startup before the UI ever mounts.
-}
loadKeyMap :: IO KeyMap
loadKeyMap = do
  result <- try loadFromDisk :: IO (Either IOException KeyMap)
  case result of
    Right km -> pure km
    Left _ -> pure defaultKeyMap
 where
  loadFromDisk = do
    path <- configPath
    exists <- doesFileExist path
    if exists
      then do
        contents <- readFile path
        foldM applyLine defaultKeyMap (zip [1 :: Int ..] (lines contents))
      else do
        writeFile path defaultConfigText
        pure defaultKeyMap
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
'UI.keyBackspace'\/'keyBackspace2' (two wire encodings of the same key) and
'UI.keyMouseRelease' (termbox2 doesn't say which button released) are
deliberately unbindable. Mouse-only settings use 'MouseBinding', not
'Binding', since termbox2 never reports a character for a mouse event.
Ctrl is fair game there (unlike Alt/Shift) because it arrives in the same
wire event as the button, never a separate keystroke - this needed a small
patch to termbox2's SGR\/urxvt mouse parsing, which silently dropped the
Ctrl bit on the floor.
-}
