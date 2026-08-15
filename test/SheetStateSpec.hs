module SheetStateSpec (tests) where

import SheetState (
  Chart (..),
  ChartType (..),
  FillSource (..),
  cellAt,
  cellsInSelection,
  clampAxis,
  clampRange,
  classifyChartRange,
  classifySelection,
  heatmapStep,
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
    , classifyChartRangeTests
    , heatmapStepTests
    , cellsInSelectionTests
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

classifyChartRangeTests :: TestTree
classifyChartRangeTests =
  testGroup
    "classifyChartRange"
    [ testCase "no selection never qualifies, for any type" $ do
        classifyChartRange BarChart Nothing @?= Nothing
        classifyChartRange Heatmap Nothing @?= Nothing
    , testCase "BarChart accepts a header row plus one data row" $
        classifyChartRange BarChart (Just ((1, 5), (4, 6)))
          @?= Just (Chart BarChart ((1, 5), (4, 6)))
    , testCase "LineChart accepts a header column plus one data column" $
        classifyChartRange LineChart (Just ((3, 0), (4, 6)))
          @?= Just (Chart LineChart ((3, 0), (4, 6)))
    , testCase "BarChart accepts the smallest valid selection (2 rows, 1 column)" $
        classifyChartRange BarChart (Just ((2, 2), (2, 3)))
          @?= Just (Chart BarChart ((2, 2), (2, 3)))
    , testCase "a single selected cell no longer qualifies - no room for a header and a datum" $
        classifyChartRange BarChart (Just ((2, 2), (2, 2))) @?= Nothing
    , testCase "BarChart rejects a genuine 2D block" $
        classifyChartRange BarChart (Just ((0, 0), (2, 2))) @?= Nothing
    , testCase "LineChart rejects a genuine 2D block" $
        classifyChartRange LineChart (Just ((0, 0), (2, 2))) @?= Nothing
    , testCase "Heatmap accepts a genuine 2D block" $
        classifyChartRange Heatmap (Just ((0, 0), (2, 2)))
          @?= Just (Chart Heatmap ((0, 0), (2, 2)))
    , testCase "Heatmap accepts a row selection too" $
        classifyChartRange Heatmap (Just ((1, 5), (4, 5)))
          @?= Just (Chart Heatmap ((1, 5), (4, 5)))
    , testCase "corners normalize regardless of order" $
        classifyChartRange BarChart (Just ((4, 6), (1, 5)))
          @?= Just (Chart BarChart ((1, 5), (4, 6)))
    ]

heatmapStepTests :: TestTree
heatmapStepTests =
  testGroup
    "heatmapStep"
    [ testCase "the minimum maps to step 0" $ heatmapStep 5 (0, 10) 0 @?= 0
    , testCase "the maximum maps to the last step" $ heatmapStep 5 (0, 10) 10 @?= 4
    , testCase "the midpoint maps to the middle step" $ heatmapStep 5 (0, 10) 5 @?= 2
    , testCase "a degenerate range (min == max) maps to the middle step" $
        heatmapStep 5 (3, 3) 3 @?= 2
    , testCase "clamps a value fractionally below the minimum" $
        heatmapStep 5 (0, 10) (-0.001) @?= 0
    , testCase "clamps a value fractionally above the maximum" $
        heatmapStep 5 (0, 10) 10.001 @?= 4
    ]

cellsInSelectionTests :: TestTree
cellsInSelectionTests =
  testGroup
    "cellsInSelection"
    [ testCase "no selection returns just the cursor" $
        cellsInSelection (3, 4) Nothing @?= [(3, 4)]
    , testCase "a 1x1 selection returns that single cell" $
        cellsInSelection (2, 2) (Just ((2, 2), (2, 2))) @?= [(2, 2)]
    , testCase "a 2x2 selection returns all four cells" $
        cellsInSelection (0, 0) (Just ((1, 1), (2, 2)))
          @?= [(1, 1), (1, 2), (2, 1), (2, 2)]
    , testCase "corners in reverse order produce the same rectangle" $
        cellsInSelection (0, 0) (Just ((2, 2), (1, 1)))
          @?= [(1, 1), (1, 2), (2, 1), (2, 2)]
    , testCase "a row selection (1 row, multiple cols)" $
        cellsInSelection (0, 0) (Just ((1, 3), (4, 3)))
          @?= [(1, 3), (2, 3), (3, 3), (4, 3)]
    , testCase "a column selection (multiple rows, 1 col)" $
        cellsInSelection (0, 0) (Just ((2, 1), (2, 4)))
          @?= [(2, 1), (2, 2), (2, 3), (2, 4)]
    , testCase "a 3x4 block returns all 12 cells" $
        cellsInSelection (0, 0) (Just ((0, 0), (2, 3)))
          @?= [ (x, y) | x <- [0 .. 2], y <- [0 .. 3] ]
    ]
