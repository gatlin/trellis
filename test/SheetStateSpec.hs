module SheetStateSpec (tests) where

import SheetState (cellAt, clampAxis, clampRange)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (Positive (..), testProperty)

tests :: TestTree
tests =
  testGroup
    "SheetState"
    [clampAxisTests, clampAxisInvariant, cellAtTests, clampRangeTests]

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
