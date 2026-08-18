{- |
Module: Render.Help
Description: The centered help modal listing all keybindings and mouse
bindings. Drawn on top of the grid when 'helpModal' is set.
-}
module Render.Help (renderHelp, helpContent, helpInnerHeight) where

import Control.Monad (forM_)
import Keymap (KeyMap (..), showBinding, showMouseBinding)
import Render.Theme (gridFg, modalBorderBg, modalBorderFg, padTo)
import SheetState.Geometry (clampRange)
import qualified Trellis.UI as UI

{- | How many 'helpContent' lines are visible at once, given the terminal
height and the full (unscrolled) line count - the box shrinks to fit a
short terminal rather than running off the bottom of the screen.
Shared with "Update.Core" so scrolling clamps against exactly what's
drawn, not a separately-maintained guess at it.
-}
helpInnerHeight :: Int -> Int -> Int
helpInnerHeight h total = max 5 (min (h - 2) (total + 3)) - 3

{- | Draw the help overlay: a centered, height-clamped box listing
@scroll@ onward of every binding, with a footer that always stays
visible (even mid-scroll) reminding the user how to close it - and, once
there's more content than fits, that it scrolls at all.
-}
renderHelp :: Int -> KeyMap -> UI.Screen ()
renderHelp scroll km = do
  w <- UI.width
  h <- UI.height
  let content = helpContent km
      total = length content
      innerH = helpInnerHeight h total
      boxW = min 60 (w - 8)
      boxH = innerH + 3
      x0 = max 0 ((w - boxW) `div` 2)
      y0 = max 0 ((h - boxH) `div` 2)
      innerW = boxW - 2
      keyColW = 18
      yBot = y0 + boxH - 1
      footerY = yBot - 1
      maxOffset = max 0 (total - innerH)
      offset = clampRange 0 maxOffset scroll
      visible = take innerH (drop offset content)
      scrollable = maxOffset > 0
      footer
        | scrollable = "^/v scroll, PgUp/PgDn page - Esc or ? to close"
        | otherwise = "Press Esc or ? to close"
      footerX = x0 + 1 + max 0 ((innerW - length footer) `div` 2)
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
  -- Solid black background fill
  forM_ [y0 + 1 .. yBot - 1] $ \y ->
    UI.drawText (x0 + 1) y 0 0 (replicate innerW ' ')
  -- Content lines - only the scrolled-to window, never the footer's row
  forM_ (zip [y0 + 1 ..] visible) $ \(y, (key, action)) -> do
    let line
          | null key = action
          | otherwise = padTo keyColW key ++ "  " ++ action
    UI.drawText (x0 + 1) y 15 0 (take innerW line)
  -- Footer: fixed in place, always visible regardless of scroll position
  UI.drawText footerX footerY gridFg 0 (take innerW footer)

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
  , ("Resize", "")
  , (showBinding (growColKey km), "Widen current column")
  , (showBinding (shrinkColKey km), "Narrow current column")
  , (showBinding (growRowKey km), "Heighten current row")
  , (showBinding (shrinkRowKey km), "Shorten current row")
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
  ]
