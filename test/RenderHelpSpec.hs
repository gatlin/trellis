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

{- | Placeholders, not real tests yet - and *deliberately* so, not just
because nobody's gotten to them. 'Update.Core's actual offset-clamping
logic (what 'moveUp'\/'moveDown'\/'pageUp'\/'pageDown' do to
'SheetState.helpScroll') lives as a @let@-bound local inside
'scrollHelpBy', itself defined in @update@'s @where@ clause - there's no
standalone, pure function to call from a test at all right now, unlike
'helpInnerHeight' above or 'SheetState.Geometry.clampAxis' (the grid's
own equivalent, which *is* directly testable because it was pulled out
that way from the start).

That's the useful thing a scaffold-first pass surfaces: naming these
cases *before* writing real assertions makes it obvious the natural next
step isn't "figure out how to reach into @update@'s where-clause from a
test," it's "pull the clamp arithmetic out into an exported
@clampHelpScroll :: Int -> Int -> Int -> Int -> Int@ (total -> visible ->
current -> delta -> new offset), the same way clampAxis already is." The
stub names are the spec for that extraction; filling them in for real is
what should happen right after it.
-}
scrollClampTests :: TestTree
scrollClampTests =
  testGroup
    "helpScroll clamping (TODO: extract a pure clampHelpScroll first)"
    [ testCase "line-scroll (moveUp/moveDown) moves by exactly 1 line" $
        assertBool "TODO" True
    , testCase "page-scroll (pageUp/pageDown) moves by the visible line count" $
        assertBool "TODO" True
    , testCase "offset never goes below 0" $
        assertBool "TODO" True
    , testCase "offset never exceeds total - visible" $
        assertBool "TODO" True
    , testCase "opening the modal (helpKey) always resets helpScroll to 0" $
        assertBool "TODO" True
    ]
