module UpdateNavigationSpec (tests) where

import qualified Control.Comonad.Store as CS
import Data.Functor.Identity (Identity (..))
import SheetState (SheetState (..), initialState)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Trellis.UI as UI
import Update.Navigation (nudge, nudgeSelecting)

-- | Run a UI.Action against an initial state, returning the final state.
runAction ::
  UI.Action (UI.Store SheetState) IO () -> SheetState -> IO SheetState
runAction act st =
  UI.move
    (\() st' -> return st')
    act
    (CS.StoreT (\s -> Identity s) st)

tests :: TestTree
tests =
  testGroup
    "Navigation"
    [ nudgeSelectingTests
    , nudgeTests
    ]

nudgeSelectingTests :: TestTree
nudgeSelectingTests =
  testGroup
    "nudgeSelecting"
    [ testCase "no selection: anchors at cursor, extends right" $ do
        let st = initialState{cursor = (5, 5)}
        st' <- runAction (nudgeSelecting (1, 0)) st
        selection st' @?= Just ((5, 5), (6, 5))
    , testCase "continuation: endpoint matches cursor, anchor preserved" $ do
        let st =
              initialState
                { cursor = (6, 5)
                , selection = Just ((5, 5), (6, 5))
                }
        st' <- runAction (nudgeSelecting (1, 0)) st
        selection st' @?= Just ((5, 5), (7, 5))
    , testCase "fresh after plain nudge: anchor resets to current cursor" $ do
        let st = initialState{cursor = (7, 5), selection = Nothing}
        st' <- runAction (nudgeSelecting (1, 0)) st
        selection st' @?= Just ((7, 5), (8, 5))
    , testCase "mismatch: endpoint /= cursor, fresh anchor at cursor" $ do
        let st =
              initialState
                { cursor = (7, 5)
                , selection = Just ((5, 5), (6, 5))
                }
        st' <- runAction (nudgeSelecting (1, 0)) st
        selection st' @?= Just ((7, 5), (8, 5))
    ]

nudgeTests :: TestTree
nudgeTests =
  testGroup
    "nudge"
    [ testCase "plain nudge collapses selection and moves cursor" $ do
        let st =
              initialState
                { cursor = (6, 5)
                , selection = Just ((5, 5), (6, 5))
                }
        st' <- runAction (nudge (1, 0)) st
        selection st' @?= Nothing
        cursor st' @?= (7, 5)
    ]
