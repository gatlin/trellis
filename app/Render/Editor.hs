{- |
Module: Render.Editor
Description: Drawing the CPS-based formula-editing modal.
-}
module Render.Editor (
  renderEditor,
  modalWidth,
  wrapText,
  wrapForCursor,
  cursorLineCol,
) where

import Control.Monad (forM_)
import Render.Theme (
  modalBorderBg,
  modalBorderFg,
  modalHPad,
  modalVPad,
  padTo,
  textBg,
  textFg,
 )
import SheetState (EditorState (..))
import qualified Trellis.UI as UI

renderEditor :: EditorState -> UI.Screen ()
renderEditor m = do
  w <- UI.width
  h <- UI.height
  -- \| The cap leaves room for the padding and border on top of
  -- 'innerWidth' itself, or the box could run past the edge of a
  -- narrow terminal.
  let innerWidth = max 1 (min modalWidth (w - 2 * modalHPad - 4))
      wrapped = wrapForCursor innerWidth (editorBuffer m) (editorCursor m)
      boxWidth = innerWidth + 2 * modalHPad + 2
      boxHeight = length wrapped + 2 * modalVPad + 2
      left = max 0 ((w - boxWidth) `div` 2)
      top = max 0 ((h - boxHeight) `div` 2)
      right = left + boxWidth - 1
      bottom = top + boxHeight - 1
      textLeft = left + 1 + modalHPad
      textTop = top + 1 + modalVPad
  -- \| Clears the whole interior - including the padding itself - before
  -- anything else is drawn, so the padding reads as clean space instead
  -- of the sheet grid showing through gaps around the text.
  forM_ [top + 1 .. bottom - 1] $ \y ->
    UI.drawText (left + 1) y textFg textBg (replicate (boxWidth - 2) ' ')
  UI.drawHLine top left right modalBorderFg modalBorderBg 0x2501
  UI.drawHLine bottom left right modalBorderFg modalBorderBg 0x2501
  UI.drawVLine left (top + 1) (bottom - 1) modalBorderFg modalBorderBg 0x2503
  UI.drawVLine right (top + 1) (bottom - 1) modalBorderFg modalBorderBg 0x2503
  UI.drawGlyph left top modalBorderFg modalBorderBg 0x250F
  UI.drawGlyph right top modalBorderFg modalBorderBg 0x2513
  UI.drawGlyph left bottom modalBorderFg modalBorderBg 0x2517
  UI.drawGlyph right bottom modalBorderFg modalBorderBg 0x251B
  forM_ (zip [0 ..] wrapped) $ \(i, line) ->
    UI.drawText textLeft (textTop + i) textFg textBg (padTo innerWidth line)
  let (cursorLine, cursorCol) =
        cursorLineCol innerWidth (length wrapped) (editorCursor m)
      lineAtCursor = wrapped !! cursorLine
      -- \| Shows the character under the cursor in reverse video, matching
      -- the focused cell - a bare reverse-video space only past all typed
      -- text.
      cursorChar
        | cursorCol < length lineAtCursor = [lineAtCursor !! cursorCol]
        | otherwise = " "
      cursorX = textLeft + cursorCol
      cursorY = textTop + cursorLine
  UI.drawText cursorX cursorY textBg textFg cursorChar

{- | The editor's text width, before wrapping: wide enough for most
formulas on one line, capped against the terminal. Deliberately generous -
a terminal too narrow for this is already too narrow to be much use.
-}
modalWidth :: Int
modalWidth = 56

-- | Break a string into fixed-width chunks; always at least one, even empty.
wrapText :: Int -> String -> [String]
wrapText width s
  | null s = [""]
  | otherwise = go s
 where
  go [] = []
  go cs = let (line, rest) = splitAt width cs in line : go rest

{- | 'wrapText', plus one extra blank line when the cursor sits exactly at
the end of a buffer whose length is a multiple of the wrap width. [^1]
-}
wrapForCursor :: Int -> String -> Int -> [String]
wrapForCursor width s cur
  | atBoundary = base ++ [""]
  | otherwise = base
 where
  base = wrapText width s
  atBoundary = cur == length s && cur > 0 && cur `mod` width == 0

{- | Which wrapped line, and which column within it, a character offset
into the unwrapped buffer falls on.
-}
cursorLineCol :: Int -> Int -> Int -> (Int, Int)
cursorLineCol width totalLines pos =
  let line = min (totalLines - 1) (pos `div` width)
   in (line, pos - line * width)

{- [^1]:
Without this, 'cursorLineCol' would place the cursor one column past the
last real line - exactly on the box's right border, overwriting it -
instead of wrapping to a fresh line the instant the current one fills, the
same way a text editor's cursor visibly does.
-}
