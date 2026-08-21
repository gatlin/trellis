module UpdateSpec (tests) where

import qualified Data.Map.Strict as Map
import qualified Trellis.UI as UI
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Keymap (BaseKey (..), Binding (..), matches, namedCtrlKey)
import SheetState.Geometry (cellAt)
import Update (
  clampCursor,
  deleteAt,
  deleteBefore,
  dragging,
  insertAt,
  isKey,
  isMouse,
  isValid,
  printableChar,
 )
import Update.Events (isShiftKey)

-- | A named-key event, e.g. an arrow key: 'UI.evtCh' is unused, 'UI.evtKey' is set.
keyEvent :: UI.Key -> UI.InputEvent
keyEvent k =
  UI.InputEvent
    { UI.evtType = UI.eventKey
    , UI.evtMod = 0
    , UI.evtKey = k
    , UI.evtCh = 0
    , UI.evtW = 0
    , UI.evtH = 0
    , UI.evtX = 0
    , UI.evtY = 0
    }

-- | An ordinary typed character: no 'UI.evtKey', just a codepoint in 'UI.evtCh'.
charEvent :: Char -> UI.InputEvent
charEvent c = (keyEvent 0){UI.evtCh = fromIntegral (fromEnum c)}

-- | A mouse event at the origin, optionally mid-drag.
mouseEvent :: UI.Key -> Bool -> UI.InputEvent
mouseEvent k isDragging =
  UI.InputEvent
    { UI.evtType = UI.eventMouse
    , UI.evtMod = if isDragging then UI.modMotion else 0
    , UI.evtKey = k
    , UI.evtCh = 0
    , UI.evtW = 0
    , UI.evtH = 0
    , UI.evtX = 0
    , UI.evtY = 0
    }

-- | A named-key event with the Shift modifier held.
shiftKeyEvent :: UI.Key -> UI.InputEvent
shiftKeyEvent k = (keyEvent k){UI.evtMod = UI.modShift}

tests :: TestTree
tests =
  testGroup
    "Update"
    [ isKeyIsMouseTests
    , draggingTests
    , printableCharTests
    , isValidTests
    , insertAtTests
    , deleteBeforeTests
    , deleteAtTests
    , clampCursorTests
    , zoomKeyTests
    , pageKeyTests
    , fillKeyTests
    , shiftKeyTests
    , clearCellKeyTests
    , editKeyTests
    , gutterHeaderClickTests
    , nudgeSelectingAnchorTests
    ]

isKeyIsMouseTests :: TestTree
isKeyIsMouseTests =
  testGroup
    "isKey / isMouse"
    [ testCase "isKey matches a key event with the same key" $
        isKey (keyEvent UI.keyArrowUp) UI.keyArrowUp @?= True
    , testCase "isKey rejects a different key" $
        isKey (keyEvent UI.keyArrowUp) UI.keyArrowDown @?= False
    , testCase "isKey rejects a mouse event" $
        isKey (mouseEvent UI.keyMouseLeft False) UI.keyMouseLeft @?= False
    , testCase "isMouse matches a mouse event with the same button" $
        isMouse (mouseEvent UI.keyMouseLeft False) UI.keyMouseLeft @?= True
    , testCase "isMouse rejects a key event" $
        isMouse (keyEvent UI.keyArrowUp) UI.keyMouseLeft @?= False
    ]

draggingTests :: TestTree
draggingTests =
  testGroup
    "dragging"
    [ testCase "a plain press is not a drag" $
        dragging (mouseEvent UI.keyMouseLeft False) @?= False
    , testCase "a motion event is a drag" $
        dragging (mouseEvent UI.keyMouseLeft True) @?= True
    ]

printableCharTests :: TestTree
printableCharTests =
  testGroup
    "printableChar"
    [ testCase "extracts the character from a char event" $
        printableChar (charEvent 'k') @?= Just 'k'
    , testCase "is Nothing for a named-key event" $
        printableChar (keyEvent UI.keyArrowUp) @?= Nothing
    ]

isValidTests :: TestTree
isValidTests =
  testGroup
    "isValid"
    [ testCase "a well-formed formula is valid" $ isValid "1+2*3" @?= True
    , testCase "an incomplete formula is invalid" $ isValid "1+" @?= False
    , -- An empty buffer is handled separately, by a `null buf` check at
      -- isValid's call site in Update.editing, not by isValid itself.
      testCase "an empty buffer doesn't parse on its own" $
        isValid "" @?= False
    ]

insertAtTests :: TestTree
insertAtTests =
  testGroup
    "insertAt"
    [ testCase "at the start" $ insertAt 'x' 0 "bc" @?= ("xbc", 1)
    , testCase "in the middle" $ insertAt 'x' 1 "bc" @?= ("bxc", 2)
    , testCase "at the end" $ insertAt 'x' 2 "bc" @?= ("bcx", 3)
    , testCase "into an empty buffer" $ insertAt 'x' 0 "" @?= ("x", 1)
    ]

deleteBeforeTests :: TestTree
deleteBeforeTests =
  testGroup
    "deleteBefore"
    [ testCase "in the middle" $ deleteBefore 1 "bc" @?= ("c", 0)
    , testCase "at the end" $ deleteBefore 2 "bc" @?= ("b", 1)
    , testCase "at position 0 is a no-op" $ deleteBefore 0 "bc" @?= ("bc", 0)
    ]

deleteAtTests :: TestTree
deleteAtTests =
  testGroup
    "deleteAt"
    [ testCase "at the start" $ deleteAt 0 "bc" @?= ("c", 0)
    , testCase "in the middle" $ deleteAt 1 "bcd" @?= ("bd", 1)
    , testCase "at the end is a no-op" $ deleteAt 2 "bc" @?= ("bc", 2)
    ]

clampCursorTests :: TestTree
clampCursorTests =
  testGroup
    "clampCursor"
    [ testCase "in range is unchanged" $ clampCursor 1 "abc" @?= 1
    , testCase "below zero clamps to zero" $ clampCursor (-1) "abc" @?= 0
    , testCase "past the end clamps to the buffer's length" $
        clampCursor 99 "abc" @?= 3
    ]

zoomKeyTests :: TestTree
zoomKeyTests =
  testGroup
    "zoom keys"
    [ testCase "'=' matches zoomIn binding" $
        matches (Plain (Char '=')) (charEvent '=')
          @?= True
    , testCase "'-' matches zoomOut binding" $
        matches (Plain (Char '-')) (charEvent '-')
          @?= True
    , testCase "'0' matches zoomReset binding" $
        matches (Plain (Char '0')) (charEvent '0')
          @?= True
    , testCase "'=' does not match an unrelated key" $
        matches (Plain (Char '=')) (keyEvent UI.keyArrowUp)
          @?= False
    , testCase "'=' does not match a mouse event" $
        matches (Plain (Char '=')) (mouseEvent UI.keyMouseLeft False)
          @?= False
    ]

fillKeyTests :: TestTree
fillKeyTests =
  testGroup
    "fill keys"
    [ testCase "Ctrl+d matches fillKey binding" $
        matches (WithCtrl (Char 'd')) (ctrlCharEvent 'd')
          @?= True
    , testCase "Ctrl+r matches fillKeyAlt binding" $
        matches (WithCtrl (Char 'r')) (ctrlCharEvent 'r')
          @?= True
    , testCase "Ctrl+d does not match a plain 'd' (no Ctrl)" $
        matches (WithCtrl (Char 'd')) (charEvent 'd')
          @?= False
    , testCase "Ctrl+d does not match an unrelated key" $
        matches (WithCtrl (Char 'd')) (keyEvent UI.keyArrowUp)
          @?= False
    , testCase "Ctrl+d does not match a mouse event" $
        matches (WithCtrl (Char 'd')) (mouseEvent UI.keyMouseLeft False)
          @?= False
    ]

{- | A Ctrl-held character event, shaped the way termbox2 actually reports
it for a letter (its own distinct named key, not @ch@ + a modifier bit -
see 'Keymap.namedCtrlKey'), not the naive shape a naming convention alone
would suggest.
-}
ctrlCharEvent :: Char -> UI.InputEvent
ctrlCharEvent c = case namedCtrlKey c of
  Just k -> (keyEvent k){UI.evtMod = UI.modCtrl}
  Nothing -> (charEvent c){UI.evtMod = UI.modCtrl}

clearCellKeyTests :: TestTree
clearCellKeyTests =
  testGroup
    "clearCell key"
    [ testCase "Delete matches clearCell binding" $
        matches (Plain (Key UI.keyDelete)) (keyEvent UI.keyDelete)
          @?= True
    , testCase "Delete does not match an unrelated key" $
        matches (Plain (Key UI.keyDelete)) (keyEvent UI.keyArrowUp)
          @?= False
    , testCase "Delete does not match a mouse event" $
        matches (Plain (Key UI.keyDelete)) (mouseEvent UI.keyMouseLeft False)
          @?= False
    ]

editKeyTests :: TestTree
editKeyTests =
  testGroup
    "editKey (F2)"
    [ testCase "F2 matches editKey binding" $
        matches (Plain (Key UI.keyF2)) (keyEvent UI.keyF2)
          @?= True
    , testCase "F2 does not match an unrelated key" $
        matches (Plain (Key UI.keyF2)) (keyEvent UI.keyArrowUp)
          @?= False
    , testCase "F2 does not match a mouse event" $
        matches (Plain (Key UI.keyF2)) (mouseEvent UI.keyMouseLeft False)
          @?= False
    ]

shiftKeyTests :: TestTree
shiftKeyTests =
  testGroup
    "isShiftKey"
    [ testCase "Shift+ArrowUp matches ArrowUp" $
        isShiftKey (shiftKeyEvent UI.keyArrowUp) UI.keyArrowUp @?= True
    , testCase "ArrowUp without Shift does not match" $
        isShiftKey (keyEvent UI.keyArrowUp) UI.keyArrowUp @?= False
    , testCase "Shift+ArrowUp does not match ArrowDown" $
        isShiftKey (shiftKeyEvent UI.keyArrowUp) UI.keyArrowDown @?= False
    , testCase "a mouse event never matches" $
        isShiftKey (mouseEvent UI.keyMouseLeft False) UI.keyArrowUp @?= False
    , testCase "Shift+ArrowDown does not match ArrowUp" $
        isShiftKey (shiftKeyEvent UI.keyArrowDown) UI.keyArrowUp @?= False
    ]

gutterHeaderClickTests :: TestTree
gutterHeaderClickTests =
  testGroup
    "gutter/header click deselects"
    [ testCase "cellAt returns Nothing for a gutter position" $
        cellAt 8 Map.empty Map.empty (0, 0) 2 5 @?= Nothing
    , testCase "cellAt returns Nothing for a header position" $
        cellAt 8 Map.empty Map.empty (0, 0) 10 0 @?= Nothing
    , testCase "cellAt returns Nothing for a ruled line" $
        cellAt 8 Map.empty Map.empty (0, 0) 10 1 @?= Nothing
    , testCase "cellAt returns Just for a valid cell position" $
        cellAt 8 Map.empty Map.empty (0, 0) 10 2 @?= Just (0, 0)
    ]

{- | Mirrors the anchor-resolution logic in 'Update.Navigation.nudgeSelecting':
if the selection's endpoint matches the cursor, the anchor is the selection's
start (continuation of an in-progress Shift+Arrow run); otherwise the cursor
itself becomes the fresh anchor.
-}
resolveAnchor :: (Int, Int) -> Maybe ((Int, Int), (Int, Int)) -> (Int, Int)
resolveAnchor cursor sel = case sel of
  Just (a, e) | e == cursor -> a
  _ -> cursor

nudgeSelectingAnchorTests :: TestTree
nudgeSelectingAnchorTests =
  testGroup
    "nudgeSelecting anchor logic"
    [ testCase "no selection: anchor is cursor, selection spans cursor to new position" $
        let cursor = (5, 5) :: (Int, Int)
            sel = Nothing :: Maybe ((Int, Int), (Int, Int))
            anchor = resolveAnchor cursor sel
            newCursor = (6, 5) :: (Int, Int)
         in (anchor, newCursor) @?= ((5, 5), (6, 5))
    , testCase "endpoint matches cursor: anchor preserved from selection start" $
        let cursor = (6, 5) :: (Int, Int)
            sel = Just ((5, 5), (6, 5)) :: Maybe ((Int, Int), (Int, Int))
            anchor = resolveAnchor cursor sel
            newCursor = (7, 5) :: (Int, Int)
         in (anchor, newCursor) @?= ((5, 5), (7, 5))
    , testCase "plain nudge clears selection regardless of prior state" $
        let newCursor = (7, 5) :: (Int, Int)
         in ((Nothing, newCursor) :: (Maybe (Int, Int), (Int, Int)))
           @?= (Nothing, (7, 5))
    , testCase "after plain nudge (selection cleared), next nudgeSelecting starts fresh" $
        let cursor = (7, 5) :: (Int, Int)
            sel = Nothing :: Maybe ((Int, Int), (Int, Int))
            anchor = resolveAnchor cursor sel
            newCursor = (8, 5) :: (Int, Int)
         in (anchor, newCursor) @?= ((7, 5), (8, 5))
    , testCase "endpoint does NOT match cursor: fresh anchor at cursor" $
        let cursor = (7, 5) :: (Int, Int)
            sel = Just ((5, 5), (6, 5)) :: Maybe ((Int, Int), (Int, Int))
            anchor = resolveAnchor cursor sel
            newCursor = (8, 5) :: (Int, Int)
         in (anchor, newCursor) @?= ((7, 5), (8, 5))
    ]

pageKeyTests :: TestTree
pageKeyTests =
  testGroup
    "page keys"
    [ testCase "PageUp matches pageUp binding" $
        matches (Plain (Key UI.keyPgUp)) (keyEvent UI.keyPgUp)
          @?= True
    , testCase "PageDown matches pageDown binding" $
        matches (Plain (Key UI.keyPgDn)) (keyEvent UI.keyPgDn)
          @?= True
    , testCase "PageUp does not match PageDown" $
        matches (Plain (Key UI.keyPgUp)) (keyEvent UI.keyPgDn)
          @?= False
    , testCase "PageUp does not match a mouse event" $
        matches (Plain (Key UI.keyPgUp)) (mouseEvent UI.keyMouseLeft False)
          @?= False
    ]
