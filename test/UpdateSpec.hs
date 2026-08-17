module UpdateSpec (tests) where

import qualified Termbox2 as Tb2
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Keymap (BaseKey (..), Binding (..), matches)
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

-- | A named-key event, e.g. an arrow key: '_ch' is unused, '_key' is set.
keyEvent :: Tb2.Tb2Key -> Tb2.Tb2Event
keyEvent k =
  Tb2.Tb2Event
    { Tb2._type = Tb2.eventKey
    , Tb2._mod = 0
    , Tb2._key = k
    , Tb2._ch = 0
    , Tb2._w = 0
    , Tb2._h = 0
    , Tb2._x = 0
    , Tb2._y = 0
    }

-- | An ordinary typed character: no '_key', just a codepoint in '_ch'.
charEvent :: Char -> Tb2.Tb2Event
charEvent c = (keyEvent 0){Tb2._ch = fromIntegral (fromEnum c)}

-- | A mouse event at the origin, optionally mid-drag.
mouseEvent :: Tb2.Tb2Key -> Bool -> Tb2.Tb2Event
mouseEvent k isDragging =
  Tb2.Tb2Event
    { Tb2._type = Tb2.eventMouse
    , Tb2._mod = if isDragging then Tb2.modMotion else 0
    , Tb2._key = k
    , Tb2._ch = 0
    , Tb2._w = 0
    , Tb2._h = 0
    , Tb2._x = 0
    , Tb2._y = 0
    }

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
    , clearCellKeyTests
    , editKeyTests
    , gutterHeaderClickTests
    ]

isKeyIsMouseTests :: TestTree
isKeyIsMouseTests =
  testGroup
    "isKey / isMouse"
    [ testCase "isKey matches a key event with the same key" $
        isKey (keyEvent Tb2.keyArrowUp) Tb2.keyArrowUp @?= True
    , testCase "isKey rejects a different key" $
        isKey (keyEvent Tb2.keyArrowUp) Tb2.keyArrowDown @?= False
    , testCase "isKey rejects a mouse event" $
        isKey (mouseEvent Tb2.keyMouseLeft False) Tb2.keyMouseLeft @?= False
    , testCase "isMouse matches a mouse event with the same button" $
        isMouse (mouseEvent Tb2.keyMouseLeft False) Tb2.keyMouseLeft @?= True
    , testCase "isMouse rejects a key event" $
        isMouse (keyEvent Tb2.keyArrowUp) Tb2.keyMouseLeft @?= False
    ]

draggingTests :: TestTree
draggingTests =
  testGroup
    "dragging"
    [ testCase "a plain press is not a drag" $
        dragging (mouseEvent Tb2.keyMouseLeft False) @?= False
    , testCase "a motion event is a drag" $
        dragging (mouseEvent Tb2.keyMouseLeft True) @?= True
    ]

printableCharTests :: TestTree
printableCharTests =
  testGroup
    "printableChar"
    [ testCase "extracts the character from a char event" $
        printableChar (charEvent 'k') @?= Just 'k'
    , testCase "is Nothing for a named-key event" $
        printableChar (keyEvent Tb2.keyArrowUp) @?= Nothing
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
        matches (Plain (Char '=')) (keyEvent Tb2.keyArrowUp)
          @?= False
    , testCase "'=' does not match a mouse event" $
        matches (Plain (Char '=')) (mouseEvent Tb2.keyMouseLeft False)
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
        matches (WithCtrl (Char 'd')) (keyEvent Tb2.keyArrowUp)
          @?= False
    , testCase "Ctrl+d does not match a mouse event" $
        matches (WithCtrl (Char 'd')) (mouseEvent Tb2.keyMouseLeft False)
          @?= False
    ]

-- | A Ctrl-held character event.
ctrlCharEvent :: Char -> Tb2.Tb2Event
ctrlCharEvent c =
  (charEvent c){Tb2._mod = Tb2.modCtrl}

clearCellKeyTests :: TestTree
clearCellKeyTests =
  testGroup
    "clearCell key"
    [ testCase "Delete matches clearCell binding" $
        matches (Plain (Key Tb2.keyDelete)) (keyEvent Tb2.keyDelete)
          @?= True
    , testCase "Delete does not match an unrelated key" $
        matches (Plain (Key Tb2.keyDelete)) (keyEvent Tb2.keyArrowUp)
          @?= False
    , testCase "Delete does not match a mouse event" $
        matches (Plain (Key Tb2.keyDelete)) (mouseEvent Tb2.keyMouseLeft False)
          @?= False
    ]

editKeyTests :: TestTree
editKeyTests =
  testGroup
    "editKey (F2)"
    [ testCase "F2 matches editKey binding" $
        matches (Plain (Key Tb2.keyF2)) (keyEvent Tb2.keyF2)
          @?= True
    , testCase "F2 does not match an unrelated key" $
        matches (Plain (Key Tb2.keyF2)) (keyEvent Tb2.keyArrowUp)
          @?= False
    , testCase "F2 does not match a mouse event" $
        matches (Plain (Key Tb2.keyF2)) (mouseEvent Tb2.keyMouseLeft False)
          @?= False
    ]

gutterHeaderClickTests :: TestTree
gutterHeaderClickTests =
  testGroup
    "gutter/header click deselects"
    [ testCase "cellAt returns Nothing for a gutter position" $
        cellAt 8 (0, 0) 2 5 @?= Nothing
    , testCase "cellAt returns Nothing for a header position" $
        cellAt 8 (0, 0) 10 0 @?= Nothing
    , testCase "cellAt returns Nothing for a ruled line" $
        cellAt 8 (0, 0) 10 1 @?= Nothing
    , testCase "cellAt returns Just for a valid cell position" $
        cellAt 8 (0, 0) 10 2 @?= Just (0, 0)
    ]

pageKeyTests :: TestTree
pageKeyTests =
  testGroup
    "page keys"
    [ testCase "PageUp matches pageUp binding" $
        matches (Plain (Key Tb2.keyPgUp)) (keyEvent Tb2.keyPgUp)
          @?= True
    , testCase "PageDown matches pageDown binding" $
        matches (Plain (Key Tb2.keyPgDn)) (keyEvent Tb2.keyPgDn)
          @?= True
    , testCase "PageUp does not match PageDown" $
        matches (Plain (Key Tb2.keyPgUp)) (keyEvent Tb2.keyPgDn)
          @?= False
    , testCase "PageUp does not match a mouse event" $
        matches (Plain (Key Tb2.keyPgUp)) (mouseEvent Tb2.keyMouseLeft False)
          @?= False
    ]
