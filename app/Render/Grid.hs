{- |
Module: Render.Grid
Description: Laying out and drawing the sheet grid itself - the actual
'render' entry point lives here, since it's fundamentally "lay out the
grid, then delegate."
-}
module Render.Grid (
  Geometry (..),
  joinGlyph,
  render,
  renderColumnHeaders,
  renderGridLines,
  renderCells,
  renderFooter,
) where

import Control.Monad (forM_, when)
import qualified Data.Map.Strict as Map
import Formula (Value (..), blank, evaluated, renderExpr, showValue, window)
import Keymap (KeyMap)
import Render.Chart (renderChart)
import Render.Editor (renderEditor)
import Render.Help (renderHelp)
import Render.Theme (
  fillBg,
  fillFg,
  focusBg,
  focusFg,
  gridBg,
  gridFg,
  heatmapRamp,
  liveBg,
  liveFg,
  outBg,
  outFg,
  padTo,
  selectionBg,
  selectionFg,
  textBg,
  textFg,
  valueColors,
 )
import SheetState (
  Chart (..),
  ChartType (..),
  LiveBinding,
  SheetState (..),
  colBoundariesFrom,
  gutterWidth,
  headerHeight,
  heatmapStep,
  previewRect,
  rowBoundariesFrom,
  statusBarHeight,
 )
import qualified Termbox2 as Tb2
import qualified Trellis.UI as UI

{- | Layout shared across a frame's render passes, computed once so each
piece works from the same numbers. 'geoColBoundaries'\/'geoRowBoundaries'
are the actual screen-position boundaries of every visible column\/row -
uniform stride when nothing's been individually resized, but the single
source of truth for column\/row spans either way, since 'SheetState.colScale'\/
'SheetState.rowScale' let neighbors differ.
-}
data Geometry = Geometry
  { geoOx, geoOy :: Int
  , geoCx, geoCy :: Int
  , geoCols, geoRows :: Int
  , geoColBoundaries, geoRowBoundaries :: [Int]
  }

render :: KeyMap -> SheetState -> UI.Screen ()
render km st = do
  w <- UI.width
  h <- UI.height
  let (ox, oy) = viewportOrigin st
      (cx, cy) = cursor st
      cw = cellWidth st
      colBs = colBoundariesFrom cw (colScale st) gutterWidth w ox
      rowBs =
        rowBoundariesFrom
          cw
          (rowScale st)
          headerHeight
          (h - statusBarHeight - 1)
          oy
      cols = length colBs - 1
      rows = length rowBs - 1
      geo =
        Geometry
          { geoOx = ox
          , geoOy = oy
          , geoCx = cx
          , geoCy = cy
          , geoCols = cols
          , geoRows = rows
          , geoColBoundaries = colBs
          , geoRowBoundaries = rowBs
          }
      vals = window (ox, oy) cols rows (evaluated (cells st))
      fillPreview = fmap (uncurry previewRect) (fillDrag st)
  renderColumnHeaders geo
  renderGridLines geo
  renderCells
    geo
    (subscriptions st)
    (outBindings st)
    (selection st)
    fillPreview
    (heatmapColors st)
    vals
  renderChart st
  when (helpModal st) (renderHelp (helpScroll st) km)
  renderFooter st h

{- | Per-cell colors for an active heatmap, or 'Nothing' when no heatmap is
showing (or its range turned out to have no numeric cells to scale by at
all - nothing sensible to color by in that case).
-}
heatmapColors ::
  SheetState -> Maybe (Map.Map (Int, Int) (Tb2.Tb2ColorAttr, Tb2.Tb2ColorAttr))
heatmapColors st = case chart st of
  Just (Chart Heatmap ((x0, y0), (x1, y1))) ->
    let cols = x1 - x0 + 1
        rows = y1 - y0 + 1
        -- \| 'window' over-fetches by one in each direction (its other
        -- callers all trim the same way) - trimmed back down here.
        vals =
          map
            (take cols)
            (take rows (window (x0, y0) cols rows (evaluated (cells st))))
        cellVals =
          [ ((x0 + cx, y0 + cy), v)
          | (cy, row) <- zip [0 ..] vals
          , (cx, v) <- zip [0 ..] row
          ]
        nums = [n | (_, VNum n) <- cellVals]
     in case nums of
          [] -> Nothing
          _ ->
            let lo = minimum nums
                hi = maximum nums
                steps = length heatmapRamp
             in Just $
                  Map.fromList
                    [ (pos, heatmapRamp !! heatmapStep steps (lo, hi) n)
                    | (pos, VNum n) <- cellVals
                    ]
  _ -> Nothing

renderColumnHeaders :: Geometry -> UI.Screen ()
renderColumnHeaders
  Geometry
    { geoOx = ox
    , geoCx = cx
    , geoColBoundaries = colBs
    } =
    forM_ (zip [0 ..] (zip colBs (drop 1 colBs))) $ \(i, (x0, x1)) ->
      let (fg, bg) =
            if ox + i == cx then (focusFg, focusBg) else (textFg, textBg)
       in UI.drawText
            (x0 + 1)
            0
            fg
            bg
            (padTo (x1 - x0 - 1) (show (ox + i)))

renderGridLines :: Geometry -> UI.Screen ()
renderGridLines
  Geometry
    { geoRows = rows
    , geoColBoundaries = colBs
    , geoRowBoundaries = rowBs
    } = do
    let nb = length colBs
    forM_ (zip [0 ..] rowBs) $ \(j, y) -> do
      UI.drawHLine
        y
        (minimum colBs)
        (maximum colBs)
        gridFg
        gridBg
        0x2500
      forM_ (zip [0 ..] colBs) $ \(i, x) ->
        UI.drawGlyph
          x
          y
          gridFg
          gridBg
          (joinGlyph (j > 0) (j < rows) (i > 0) (i < nb - 1))

renderCells ::
  Geometry ->
  Map.Map (Int, Int) LiveBinding ->
  Map.Map (Int, Int) FilePath ->
  Maybe ((Int, Int), (Int, Int)) ->
  Maybe ((Int, Int), (Int, Int)) ->
  Maybe (Map.Map (Int, Int) (Tb2.Tb2ColorAttr, Tb2.Tb2ColorAttr)) ->
  [[Value]] ->
  UI.Screen ()
renderCells
  Geometry
    { geoOx = ox
    , geoOy = oy
    , geoCx = cx
    , geoCy = cy
    , geoCols = cols
    , geoRows = rows
    , geoColBoundaries = colBs
    , geoRowBoundaries = rowBs
    }
  subs
  outs
  sel
  fill
  heat
  vals =
    forM_ (zip3 [0 ..] (zip rowBs (drop 1 rowBs)) (take rows vals)) $
      \(rowIx, (yTop, yBot), rowVals) -> do
        let sheetRow = oy + rowIx
            screenRow = yTop + 1
            bottomRow = yBot - 1
            (rowFg, rowBg) =
              if sheetRow == cy then (focusFg, focusBg) else (textFg, textBg)
        UI.drawText
          0
          screenRow
          rowFg
          rowBg
          (padTo (gutterWidth - 1) (show sheetRow))
        -- [^1]
        forM_ colBs $ \x ->
          UI.drawVLine x screenRow bottomRow gridFg gridBg 0x2502
        forM_ (zip3 [0 ..] (zip colBs (drop 1 colBs)) (take cols rowVals)) $
          \(colIx, (xLeft, xRight), val) -> do
            let sheetCol = ox + colIx
                cellW = xRight - xLeft - 1
                focused = (sheetCol, sheetRow) == (cx, cy)
                inFill = maybe False (inRange (sheetCol, sheetRow)) fill
                selected = maybe False (inRange (sheetCol, sheetRow)) sel
                heated = heat >>= Map.lookup (sheetCol, sheetRow)
                live = Map.member (sheetCol, sheetRow) subs
                published = Map.member (sheetCol, sheetRow) outs
                (fg, bg)
                  | focused = (focusFg, focusBg)
                  | inFill = (fillFg, fillBg)
                  -- \| A heatmap outranks a plain completed selection - it's
                  -- the active visualization that selection is now showing,
                  -- not just a passive "this is what's chosen" marker.
                  | Just heatColors <- heated = heatColors
                  | selected = (selectionFg, selectionBg)
                  | live = (liveFg, liveBg)
                  | published = (outFg, outBg)
                  | otherwise = valueColors val
            UI.drawText
              (xLeft + 1)
              screenRow
              fg
              bg
              (padTo cellW (showValue val))
            -- \| Extends the cell's own colors down through the blank
            -- padding rows 'rowStride' adds at higher zoom, so a focused
            -- cell's highlight fills the whole cell block, not just its
            -- content line.
            forM_ [screenRow + 1 .. bottomRow] $ \padRow ->
              UI.drawText (xLeft + 1) padRow fg bg (padTo cellW "")

-- | Is a cell within the rectangle a fill drag's two corners span?
inRange :: (Int, Int) -> ((Int, Int), (Int, Int)) -> Bool
inRange (x, y) ((ax, ay), (bx, by)) =
  x >= min ax bx && x <= max ax bx && y >= min ay by && y <= max ay by

{- | The bottom-of-screen status text, and either the open editor or a
read-only preview of the focused cell's formula.
-}
renderFooter :: SheetState -> Int -> UI.Screen ()
renderFooter st h = do
  let (cx, cy) = cursor st
  UI.statusText textFg textBg $
    "Current Position: [" ++ show cx ++ "," ++ show cy ++ "]"
  UI.footerText "Press ? for help."
  case editor st of
    Just m -> renderEditor m
    Nothing ->
      let there = Map.findWithDefault blank (cx, cy) (cells st)
       in when (there /= blank) $
            UI.drawText 2 (h - 2) textFg textBg ("= " ++ renderExpr there)

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

{- [^1]:
Spans the whole row block, not just its content line: at higher
zoom, 'rowStride' opens up extra blank padding rows below the
content line before the next ruled line, and the vertical walls
need to keep going all the way down through those too, or the grid
looks like it's falling apart below the first line of each cell.
-}
