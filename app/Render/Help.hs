{- |
Module: Render.Help
Description: The centered help modal listing all keybindings and mouse
bindings. Drawn on top of the grid when 'helpModal' is set.
-}
module Render.Help (renderHelp) where

import Control.Monad (forM_)
import Keymap (KeyMap (..), showBinding, showMouseBinding)
import Render.Theme (modalBorderBg, modalBorderFg, padTo, textBg, textFg)
import qualified Trellis.UI as UI

{- | Draw the help overlay: a centered box listing every binding, with a
footer reminding the user how to close it.
-}
renderHelp :: KeyMap -> UI.Screen ()
renderHelp km = do
  w <- UI.width
  h <- UI.height
  let content = helpContent km
      boxW = min 60 (w - 8)
      boxH = length content + 2
      x0 = max 0 ((w - boxW) `div` 2)
      y0 = max 0 ((h - boxH) `div` 2)
      innerW = boxW - 2
      keyColW = 18
      yBot = y0 + boxH - 1
  -- Top border
  UI.drawGlyph x0 y0 modalBorderFg modalBorderBg 0x250C
  UI.drawHLine y0 (x0 + 1) (x0 + boxW - 2) modalBorderFg modalBorderBg 0x2500
  UI.drawGlyph (x0 + boxW - 1) y0 modalBorderFg modalBorderBg 0x2510
  -- Bottom border
  UI.drawGlyph x0 yBot modalBorderFg modalBorderBg 0x2514
  UI.drawHLine yBot (x0 + 1) (x0 + boxW - 2) modalBorderFg modalBorderBg 0x2500
  UI.drawGlyph (x0 + boxW - 1) yBot modalBorderFg modalBorderBg 0x2518
  -- Side borders
  forM_ [y0 + 1 .. yBot - 1] $ \y -> do
    UI.drawGlyph x0 y modalBorderFg modalBorderBg 0x2502
    UI.drawGlyph (x0 + boxW - 1) y modalBorderFg modalBorderBg 0x2502
  -- Content lines
  forM_ (zip [y0 + 1 ..] content) $ \(y, (key, action)) -> do
    let line
          | null key = action
          | otherwise = padTo keyColW key ++ "  " ++ action
    UI.drawText (x0 + 1) y textFg textBg (take innerW line)

{- | The (key, action) pairs shown in the help modal, grouped into
sections separated by blank lines.
-}
helpContent :: KeyMap -> [(String, String)]
helpContent km =
  [ ("Navigation", "")
  , (showBinding (moveUp km), "Move up")
  , (showBinding (moveDown km), "Move down")
  , (showBinding (moveLeft km), "Move left")
  , (showBinding (moveRight km), "Move right")
  , (showBinding (tabKey km), "Next cell")
  , ("Shift+Arrow", "Extend selection")
  , ("Shift+Tab", "Previous cell")
  , ("", "")
  , ("Zoom", "")
  , (showMouseBinding (scrollUp km), "Zoom in")
  , (showMouseBinding (scrollDown km), "Zoom out")
  , (showBinding (zoomInKey km), "Zoom in (key)")
  , (showBinding (zoomOutKey km), "Zoom out (key)")
  , (showBinding (zoomResetKey km), "Zoom reset")
  , ("", "")
  , ("Pan", "")
  , (showBinding (pageUp km), "Page up")
  , (showBinding (pageDown km), "Page down")
  , (showBinding (panUp km), "Pan up")
  , (showBinding (panDown km), "Pan down")
  , (showBinding (panLeft km), "Pan left")
  , (showBinding (panRight km), "Pan right")
  , (showMouseBinding (panButton km), "Pan (drag)")
  , ("", "")
  , ("Selection & Fill", "")
  , (showMouseBinding (selectButton km), "Select / drag")
  , (showMouseBinding (fillButton km), "Fill (drag)")
  , (showBinding (fillKey km), "Fill")
  , (showBinding (fillKeyAlt km), "Fill (alt)")
  , ("", "")
  , ("Editing", "")
  , (showBinding (confirm km), "Confirm / commit")
  , (showBinding (editKey km), "Edit cell")
  , (showBinding (cancel km), "Cancel / clear")
  , (showBinding (clearCell km), "Clear cell")
  , ("", "")
  , ("Charts", "")
  , (showBinding (barChartKey km), "Bar chart")
  , (showBinding (lineChartKey km), "Line chart")
  , (showBinding (heatmapKey km), "Heatmap")
  , ("", "")
  , ("Other", "")
  , (showBinding (saveKey km), "Save")
  , (showBinding (helpKey km), "Help")
  , ("", "")
  , ("Press Esc or ? to close", "")
  ]
