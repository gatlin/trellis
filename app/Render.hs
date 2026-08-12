{- |
Module: Render
Description: Drawing the sheet, its grid, and the formula-editing modal.
-}
module Render (
  render,
  sheetOutputMode,
) where

import Control.Monad (forM_, when)
import qualified Data.Map.Strict as Map
import Formula (Value (..), blank, evaluated, renderExpr, window)
import SheetState (
  EditorState (..),
  SheetState (..),
  colBoundaries,
  gutterWidth,
  headerHeight,
  rowStride,
  visibleCols,
  visibleRows,
 )
import qualified Termbox2 as Tb2
import qualified Trellis.UI as UI

{- | One global setting for the whole app, not per-call - every color used
anywhere must agree on the same mode's numbering. 256 over grayscale:
grayscale has no white for text or black for the focus highlight.
-}
sheetOutputMode :: Tb2.Tb2Output
sheetOutputMode = Tb2.output256

-- | Ordinary text: header numbers, row numbers, unfocused cell content.
textFg, textBg :: Tb2.Tb2ColorAttr
textFg = 15 -- bright white; plain "7" reads as a dull grey on many terminals
textBg = Tb2.colorDefault

-- | The focused cell: inverted, same as before, just re-picked for 256 mode.
focusFg, focusBg :: Tb2.Tb2ColorAttr
focusFg = 0 -- black
focusBg = 15 -- bright white

{- | The grid's ruling wants to recede behind the cell text, not compete
with it. 240 is a dark-mid grey: visible without shouting.
-}
gridFg, gridBg :: Tb2.Tb2ColorAttr
gridFg = 240
gridBg = Tb2.colorDefault

{- | The opposite of the grid's ruling: a dialog demanding attention, so
drawn bright ('textFg's white) with a heavy line weight rather than a new
hue - grabs the eye without adding a color to the palette.
-}
modalBorderFg, modalBorderBg :: Tb2.Tb2ColorAttr
modalBorderFg = 15
modalBorderBg = Tb2.colorDefault

{- | Breathing room between the editor's border and its text, so it reads
as a considered dialog rather than text jammed against a frame.
-}
modalHPad, modalVPad :: Int
modalHPad = 2
modalVPad = 1

{- | The box-drawing character for a lattice junction, given which of the
four directions - up, down, left, right - have a line segment touching it.
-}
joinGlyph :: Bool -> Bool -> Bool -> Bool -> Int
joinGlyph up down left right = case (up, down, left, right) of
  (False, True, False, True) -> 0x250C -- ┌
  (False, True, True, False) -> 0x2510 -- ┐
  (True, False, False, True) -> 0x2514 -- └
  (True, False, True, False) -> 0x2518 -- ┘
  (True, True, False, True) -> 0x251C -- ├
  (True, True, True, False) -> 0x2524 -- ┤
  (False, True, True, True) -> 0x252C -- ┬
  (True, False, True, True) -> 0x2534 -- ┴
  (True, True, True, True) -> 0x253C -- ┼
  (False, False, True, True) -> 0x2500 -- ─
  (True, True, False, False) -> 0x2502 -- │
  _ -> 0x20 -- a lone point touches nothing; shouldn't occur, but draw a space

{- | Layout shared across a frame's render passes, computed once so each
piece works from the same numbers.
-}
data Geometry = Geometry
  { geoOx, geoOy :: Int
  , geoCx, geoCy :: Int
  , geoCw, geoRs :: Int
  , geoCols, geoRows :: Int
  , geoBoundaries :: [Int]
  }

-- | Pad or truncate a string to an exact width.
padTo :: Int -> String -> String
padTo width str = take width (str ++ repeat ' ')

render :: SheetState -> UI.Screen ()
render st = do
  w <- UI.width
  h <- UI.height
  let (ox, oy) = viewportOrigin st
      (cx, cy) = cursor st
      cw = cellWidth st
      cols = visibleCols cw w
      rows = visibleRows cw h
      geo =
        Geometry
          { geoOx = ox
          , geoOy = oy
          , geoCx = cx
          , geoCy = cy
          , geoCw = cw
          , geoRs = rowStride cw
          , geoCols = cols
          , geoRows = rows
          , geoBoundaries = colBoundaries cw cols
          }
      vals = window (ox, oy) cols rows (evaluated (cells st))
  renderColumnHeaders geo
  renderGridLines geo
  renderCells geo vals
  renderFooter st h

renderColumnHeaders :: Geometry -> UI.Screen ()
renderColumnHeaders Geometry{geoOx = ox, geoCx = cx, geoCw = cw, geoCols = cols} =
  forM_ [0 .. cols - 1] $ \i ->
    let (fg, bg) = if ox + i == cx then (focusFg, focusBg) else (textFg, textBg)
     in UI.drawText (gutterWidth + i * cw) 0 fg bg (padTo (cw - 1) (show (ox + i)))

renderGridLines :: Geometry -> UI.Screen ()
renderGridLines Geometry{geoRs = rs, geoRows = rows, geoBoundaries = boundaries} = do
  let nb = length boundaries
  forM_ [0 .. rows] $ \j -> do
    let y = headerHeight + j * rs
    UI.drawHLine y (minimum boundaries) (maximum boundaries) gridFg gridBg 0x2500
    forM_ (zip [0 ..] boundaries) $ \(i, x) ->
      UI.drawGlyph
        x
        y
        gridFg
        gridBg
        (joinGlyph (j > 0) (j < rows) (i > 0) (i < nb - 1))

renderCells :: Geometry -> [[Value]] -> UI.Screen ()
renderCells
  Geometry
    { geoOx = ox
    , geoOy = oy
    , geoCx = cx
    , geoCy = cy
    , geoCw = cw
    , geoRs = rs
    , geoCols = cols
    , geoRows = rows
    , geoBoundaries = boundaries
    }
  vals =
    forM_ (zip [0 ..] (take rows vals)) $ \(rowIx, rowVals) -> do
      let sheetRow = oy + rowIx
          screenRow = headerHeight + rowIx * rs + 1
          bottomRow = headerHeight + (rowIx + 1) * rs - 1
          (rowFg, rowBg) = if sheetRow == cy then (focusFg, focusBg) else (textFg, textBg)
      UI.drawText 0 screenRow rowFg rowBg (padTo (gutterWidth - 1) (show sheetRow))
      -- [^1]
      forM_ boundaries $ \x -> UI.drawVLine x screenRow bottomRow gridFg gridBg 0x2502
      forM_ (zip [0 ..] (take cols rowVals)) $ \(colIx, val) -> do
        let sheetCol = ox + colIx
            focused = (sheetCol, sheetRow) == (cx, cy)
            (fg, bg) =
              if focused
                then (focusFg, focusBg)
                else (textFg, textBg)
        UI.drawText
          (gutterWidth + colIx * cw)
          screenRow
          fg
          bg
          (padTo (cw - 1) (showValue val))
        -- \| Extends the cell's own colors down through the blank padding
        -- rows 'rowStride' adds at higher zoom, so a focused cell's
        -- highlight fills the whole cell block, not just its content line.
        forM_ [screenRow + 1 .. bottomRow] $ \padRow ->
          UI.drawText (gutterWidth + colIx * cw) padRow fg bg (padTo (cw - 1) "")

{- | The bottom-of-screen status text, and either the open editor or a
read-only preview of the focused cell's formula.
-}
renderFooter :: SheetState -> Int -> UI.Screen ()
renderFooter st h = do
  let (cx, cy) = cursor st
  UI.statusText textFg textBg $
    "Current Position: [" ++ show cx ++ "," ++ show cy ++ "]"
  case editor st of
    Just m -> renderEditor m
    Nothing ->
      let there = Map.findWithDefault blank (cx, cy) (cells st)
       in when (there /= blank) $
            UI.drawText 2 (h - 2) textFg textBg ("= " ++ renderExpr there)

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
  let (cursorLine, cursorCol) = cursorLineCol innerWidth (length wrapped) (editorCursor m)
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
the end of a buffer whose length is a multiple of the wrap width. [^2]
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

showValue :: Value -> String
showValue VBlank = ""
showValue (VErr e) = e
showValue (VStr s) = s
showValue (VBool b) = if b then "TRUE" else "FALSE"
showValue (VNum n)
  | n == fromIntegral rounded = show rounded
  | otherwise = show n
 where
  rounded = round n :: Integer

{- [^1]:
Spans the whole row block, not just its content line: at higher
zoom, 'rowStride' opens up extra blank padding rows below the
content line before the next ruled line, and the vertical walls
need to keep going all the way down through those too, or the grid
looks like it's falling apart below the first line of each cell.
-}

{- [^2]:
Without this, 'cursorLineCol' would place the cursor one column past the
last real line - exactly on the box's right border, overwriting it -
instead of wrapping to a fresh line the instant the current one fills, the
same way a text editor's cursor visibly does.
-}
