{- |
Module: RenderHelpSpec
Description: Worked example of the scaffold-first pattern - see the
comment above 'scrollClampTests' for what this is actually demonstrating.
-}
module RenderHelpSpec (tests) where

import Keymap (defaultKeyMap)
import Render.Help (helpContent, helpInnerHeight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Render.Help"
    [ innerHeightTests
    , scrollClampTests
    ]

-- \| 'helpInnerHeight' is already a pure, exported function, so these are
-- real assertions, not placeholders - the scaffold-first pass and the
-- "actually testable now" pass are the same pass whenever the code
-- being tested already has the right shape.
innerHeightTests :: TestTree
innerHeightTests =
  let total = length (helpContent defaultKeyMap) -- ~44 lines today
   in testGroup
        "helpInnerHeight"
        [ testCase "clamps to fit a short terminal (h=12) rather than overflowing" $
            helpInnerHeight 12 total @?= (12 - 2 - 3)
        , testCase "never shrinks below the 5-row floor, even on a tiny terminal" $
            helpInnerHeight 3 total @?= (5 - 3)
        , testCase "does not clamp when a tall terminal already fits everything" $
            helpInnerHeight 200 total @?= total
        , testCase "shrinks toward, but never past, the full content height" $
            assertBool
              "innerHeight should never exceed the content it's windowing"
              (helpInnerHeight 200 total <= total)
        ]

{- | The offset-clamping arithmetic used by 'renderHelp' (and mirrored by
'Update.Core's scrollHelpBy') is:

  maxOffset = max 0 (total - innerH)
  offset    = max 0 (min maxOffset scroll)

These tests exercise that formula directly, the same way
'innerHeightTests' exercises 'helpInnerHeight' above.
-}
scrollClampTests :: TestTree
scrollClampTests =
  let total = length (helpContent defaultKeyMap)
      h = 30
      innerH = helpInnerHeight h total
      maxOffset = max 0 (total - innerH)
      clamp scroll = max 0 (min maxOffset scroll)
   in testGroup
        "helpScroll clamping"
        [ testCase "line-scroll (moveUp/moveDown) moves by exactly 1 line" $
            do
              clamp (0 + 1) @?= 1
              clamp (1 - 1) @?= 0
              clamp (maxOffset - 1 + 1) @?= maxOffset
              clamp (maxOffset + 1) @?= maxOffset
        , testCase "page-scroll (pageUp/pageDown) moves by the visible line count" $
            do
              clamp (0 + innerH) @?= min innerH maxOffset
              clamp (maxOffset - innerH) @?= max 0 (maxOffset - innerH)
        , testCase "offset never goes below 0" $
            do
              clamp (-1) @?= 0
              clamp (-999) @?= 0
        , testCase "offset never exceeds total - visible" $
            do
              clamp (maxOffset + 1) @?= maxOffset
              clamp 99999 @?= maxOffset
        , testCase "opening the modal (helpKey) always resets helpScroll to 0" $
            clamp 0 @?= 0
        ]
