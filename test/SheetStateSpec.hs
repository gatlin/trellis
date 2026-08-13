module SheetStateSpec (tests) where

import SheetState (
  FillSource (..),
  cellAt,
  clampAxis,
  clampRange,
  classifySelection,
  previewRect,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (Positive (..), testProperty)

tests :: TestTree
tests =
  testGroup
    "SheetState"
    [ clampAxisTests
    , clampAxisInvariant
    , cellAtTests
    , clampRangeTests
    , classifySelectionTests
    , previewRectTests
    ]

clampAxisTests :: TestTree
clampAxisTests =
  testGroup
    "clampAxis"
    [ testCase "cursor already in view leaves origin unchanged" $
        clampAxis 5 2 0 @?= 0
    , testCase "cursor past the right edge scrolls by the minimum amount" $
        clampAxis 5 10 0 @?= 6
    , testCase "cursor before the left edge snaps the origin to it" $
        clampAxis 5 (-3) 0 @?= (-3)
    ]

clampAxisInvariant :: TestTree
clampAxisInvariant =
  testProperty "keeps the cursor within the visible window" $
    \(Positive visible) c o ->
      let r = clampAxis visible c o
       in c >= r && c <= r + visible - 1

cellAtTests :: TestTree
cellAtTests =
  testGroup
    "cellAt"
    [ testCase "the header row has no cell" $
        cellAt 8 (0, 0) 8 0 @?= Nothing
    , testCase "a ruled line has no cell" $
        cellAt 8 (0, 0) 8 1 @?= Nothing
    , testCase "the row-number gutter has no cell" $
        cellAt 8 (0, 0) 4 2 @?= Nothing
    , testCase "a real cell resolves at the viewport origin" $
        cellAt 8 (0, 0) 8 2 @?= Just (0, 0)
    , testCase "the viewport origin offsets the resolved cell" $
        cellAt 8 (3, 2) 8 2 @?= Just (3, 2)
    ]

clampRangeTests :: TestTree
clampRangeTests =
  testGroup
    "clampRange"
    [ testCase "a value already in range is unchanged" $
        clampRange 4 28 10 @?= 10
    , testCase "a value below the minimum clamps up to it" $
        clampRange 4 28 2 @?= 4
    , testCase "a value above the maximum clamps down to it" $
        clampRange 4 28 40 @?= 28
    ]

classifySelectionTests :: TestTree
classifySelectionTests =
  testGroup
    "classifySelection"
    [ testCase "no selection at all falls back to the pressed cell" $
        classifySelection (2, 3) Nothing @?= FillCell (2, 3)
    , testCase "a degenerate 1x1 selection falls back to the pressed cell" $
        classifySelection (2, 3) (Just ((2, 3), (2, 3))) @?= FillCell (2, 3)
    , testCase "pressing inside a row selection replicates the row" $
        classifySelection (3, 5) (Just ((1, 5), (4, 5))) @?= FillRow 5 (1, 4)
    , testCase "pressing inside a column selection replicates the column" $
        classifySelection (5, 3) (Just ((5, 1), (5, 4))) @?= FillCol 5 (1, 4)
    , testCase "the selection's corners can be given in either order" $
        classifySelection (3, 5) (Just ((4, 5), (1, 5))) @?= FillRow 5 (1, 4)
    , testCase "pressing outside the selection falls back to the pressed cell" $
        classifySelection (9, 9) (Just ((1, 5), (4, 5))) @?= FillCell (9, 9)
    , testCase "a genuine 2D block selection falls back to the pressed cell" $
        classifySelection (2, 2) (Just ((1, 1), (4, 4))) @?= FillCell (2, 2)
    ]

previewRectTests :: TestTree
previewRectTests =
  testGroup
    "previewRect"
    [ testCase "a single-cell source spans the anchor and the endpoint" $
        previewRect (FillCell (2, 3)) (5, 7) @?= ((2, 3), (5, 7))
    , testCase "a row source spans its full width, down to the current row" $
        previewRect (FillRow 5 (1, 4)) (9, 8) @?= ((1, 5), (4, 8))
    , testCase "a column source spans its full height, out to the current column" $
        previewRect (FillCol 5 (1, 4)) (8, 9) @?= ((5, 1), (8, 4))
    ]
