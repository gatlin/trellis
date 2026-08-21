module KeymapSpec (tests) where

import Keymap (
  BaseKey (..),
  Binding (..),
  MouseBinding (..),
  showBinding,
  showMouseBinding,
 )
import qualified Trellis.UI as UI
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
            showBinding (Plain (Key UI.keyArrowUp)) @?= "ArrowUp"
        , testCase "WithAlt named Key" $
            showBinding (WithAlt (Key UI.keyArrowUp)) @?= "Alt+ArrowUp"
        , testCase "WithCtrl Char" $
            showBinding (WithCtrl (Char 'd')) @?= "Ctrl+d"
        , testCase "WithAlt Char" $
            showBinding (WithAlt (Char 'k')) @?= "Alt+k"
        ]
    , testGroup
        "showMouseBinding"
        [ testCase "no ctrl" $
            showMouseBinding (MouseBinding UI.keyMouseLeft False)
              @?= "MouseLeft"
        , testCase "with ctrl" $
            showMouseBinding (MouseBinding UI.keyMouseLeft True)
              @?= "Ctrl+MouseLeft"
        , testCase "wheel down" $
            showMouseBinding (MouseBinding UI.keyMouseWheelDown False)
              @?= "WheelDown"
        ]
    ]
