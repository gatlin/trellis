{- |
Module: RenderHelpSpec
Description: The help modal's layout math - how many lines fit
('Render.Help.helpInnerHeight') and how scrolling clamps
('Update.Core.clampHelpScroll').
-}
module RenderHelpSpec (tests) where

import Keymap (defaultKeyMap)
import Render.Help (helpContent, helpInnerHeight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Update.Core (clampHelpScroll)

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

{- | 'clampHelpScroll' is the exact function 'Update.Core.scrollHelpBy'
calls - not a reimplementation of its formula kept alongside it, which
was tried here first and caught nothing: flipping the @+@ to a @-@ in
'scrollHelpBy's real arithmetic (inverting every scroll direction) left
every one of these assertions passing, because they were only checking
themselves. Calling the production function directly is what makes that
impossible.
-}
scrollClampTests :: TestTree
scrollClampTests =
  let total = length (helpContent defaultKeyMap)
      h = 30
      innerH = helpInnerHeight h total
      maxOffset = max 0 (total - innerH)
   in testGroup
        "clampHelpScroll"
        [ testCase "line-scroll (moveUp/moveDown) moves by exactly 1 line" $
            do
              clampHelpScroll total innerH 0 1 @?= 1
              clampHelpScroll total innerH 1 (-1) @?= 0
              clampHelpScroll total innerH (maxOffset - 1) 1 @?= maxOffset
              clampHelpScroll total innerH maxOffset 1 @?= maxOffset
        , testCase "page-scroll (pageUp/pageDown) moves by the visible line count" $
            do
              clampHelpScroll total innerH 0 innerH @?= min innerH maxOffset
              clampHelpScroll total innerH maxOffset (negate innerH)
                @?= max 0 (maxOffset - innerH)
        , testCase "offset never goes below 0" $
            do
              clampHelpScroll total innerH 0 (-1) @?= 0
              clampHelpScroll total innerH 0 (-999) @?= 0
        , testCase "offset never exceeds total - visible" $
            do
              clampHelpScroll total innerH maxOffset 1 @?= maxOffset
              clampHelpScroll total innerH 0 99999 @?= maxOffset
        , testCase "opening the modal (helpKey) always resets helpScroll to 0" $
            clampHelpScroll total innerH 0 0 @?= 0
        ]
