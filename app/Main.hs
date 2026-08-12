module Main (main) where

import Control.Monad (void)
import Keymap (loadKeyMap)
import Render (render, sheetOutputMode)
import SheetState (initialState)
import qualified Termbox2 as Tb2
import qualified Trellis.UI as UI
import Update (update)

main :: IO ()
main = do
  keymap <- loadKeyMap
  UI.mount id setup $
    UI.activity (update keymap) render initialState
 where
  setup = do
    void (Tb2.setInputMode (Tb2.inputEsc <> Tb2.inputMouse))
    void (Tb2.setOutputMode sheetOutputMode)
