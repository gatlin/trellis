module Test.KeymapSpec (tests) where

import Keymap (
  BaseKey (..),
  Binding (..),
  MouseBinding (..),
  showBinding,
  showMouseBinding,
 )
import qualified Termbox2 as Tb2
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Keymap display helpers"
    [ testGroup
        "showBinding"
        [ testCase "Plain Char" $
            showBinding (Plain (Char 'h')) @?= "h"
        , testCase "Plain named Key" $
            showBinding (Plain (Key Tb2.keyArrowUp)) @?= "ArrowUp"
        , testCase "WithAlt named Key" $
            showBinding (WithAlt (Key Tb2.keyArrowUp)) @?= "Alt+ArrowUp"
        , testCase "WithCtrl Char" $
            showBinding (WithCtrl (Char 'd')) @?= "Ctrl+d"
        , testCase "WithAlt Char" $
            showBinding (WithAlt (Char 'k')) @?= "Alt+k"
        ]
    , testGroup
        "showMouseBinding"
        [ testCase "no ctrl" $
            showMouseBinding (MouseBinding Tb2.keyMouseLeft False)
              @?= "MouseLeft"
        , testCase "with ctrl" $
            showMouseBinding (MouseBinding Tb2.keyMouseLeft True)
              @?= "Ctrl+MouseLeft"
        , testCase "wheel down" $
            showMouseBinding (MouseBinding Tb2.keyMouseWheelDown False)
              @?= "WheelDown"
        ]
    ]
