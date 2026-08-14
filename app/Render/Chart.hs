{- |
Module: Render.Chart
Description: The bar\/line chart overlay - a floating panel over the
sheet, reusing "Render.Editor"'s box-drawing style for the frame. A
heatmap needs none of this - see "Render.Grid" instead, which recolors
the already-selected cells directly.
-}
module Render.Chart (
  renderChart,
) where

import Control.Monad (forM_, when)
import Data.Bits ((.|.))
import qualified Data.Map.Strict as Map
import Formula (Value (..), evaluated, showValue, window)
import Render.Theme (
  chartBg,
  chartFg,
  chartPalette,
  modalBorderBg,
  modalBorderFg,
  textBg,
  textFg,
 )
import SheetState (Chart (..), ChartType (..), SheetState (..))
import qualified Termbox2 as Tb2
import Trellis.Sheet (Sheet2)
import qualified Trellis.UI as UI

renderChart :: SheetState -> UI.Screen ()
renderChart st = case chart st of
  Nothing -> return ()
  Just (Chart Heatmap _) -> return ()
  Just c -> do
    w <- UI.width
    h <- UI.height
    let boxWidth = max 10 (min chartPanelWidth (w - 4))
        boxHeight = max 6 (min chartPanelHeight (h - 4))
        left = max 0 ((w - boxWidth) `div` 2)
        top = max 0 ((h - boxHeight) `div` 2)
        right = left + boxWidth - 1
        bottom = top + boxHeight - 1
        innerLeft = left + 1
        innerTop = top + 1
        innerWidth = boxWidth - 2
        innerHeight = boxHeight - 2
        ((_, y0), (_, y1)) = chartRange c
        indexAlongX = y1 - y0 == 1
        vals = chartValues indexAlongX (chartRange c) (evaluated (cells st))
        labels = chartLabels indexAlongX (chartRange c) (evaluated (cells st))
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
    case chartType c of
      BarChart ->
        drawBars
          innerLeft
          innerTop
          innerWidth
          innerHeight
          indexAlongX
          labels
          vals
      LineChart ->
        drawLine innerLeft innerTop innerWidth innerHeight indexAlongX vals
      Heatmap -> return () -- unreachable - handled by the outer match above

{- | The chart's data - the range's second row (a horizontal\/row-based
chart) or second column (vertical\/column-based) - the first being the
header row\/column 'chartLabels' reads instead - as evaluated 'Value's
flattened in index order, each read as a 'Double'. A blank or
non-numeric cell reads as @0@, the same lenient stance every chart
takes (a chart isn't a formula result other cells depend on; it's a
rendering aid over whatever's there).
-}
chartValues :: Bool -> ((Int, Int), (Int, Int)) -> Sheet2 Value -> [Double]
chartValues indexAlongX ((x0, y0), (x1, y1)) sh
  | indexAlongX = map asNum (go (x0, y0 + 1) cols 1)
  | otherwise = map asNum (go (x0 + 1, y0) 1 rows)
 where
  cols = x1 - x0 + 1
  rows = y1 - y0 + 1
  -- \| 'window' over-fetches by one in each direction (its other callers
  -- all trim the same way) - trimmed back down to the exact range here.
  go origin w h = concatMap (take w) (take h (window origin w h sh))
  asNum (VNum n) = n
  asNum _ = 0

{- | The label for each bar, read from the charted range's own first row
(a horizontal\/row-based chart) or first column (vertical\/column-based)
- the "select your data including its header" convention, the same one
'SheetState.classifyChartRange' now requires room for. A header cell
left blank falls back to that bar's own row\/column number - the same
number already shown in the grid's own header - rather than showing
nothing, which reads as broken rather than "no header typed".
-}
chartLabels :: Bool -> ((Int, Int), (Int, Int)) -> Sheet2 Value -> [String]
chartLabels indexAlongX ((x0, y0), (x1, y1)) sh
  | indexAlongX = zipWith fallback [x0 ..] (go (x0, y0) cols 1)
  | otherwise = zipWith fallback [y0 ..] (go (x0, y0) 1 rows)
 where
  cols = x1 - x0 + 1
  rows = y1 - y0 + 1
  go origin w h =
    concatMap (map showValue . take w) (take h (window origin w h sh))
  fallback i lbl = if null lbl then show i else lbl

{- | The box's fixed target size, capped against the terminal the same
way "Render.Editor"'s 'Render.Editor.modalWidth' is.
-}
chartPanelWidth, chartPanelHeight :: Int
chartPanelWidth = 60
chartPanelHeight = 20

fullBlock :: Int
fullBlock = 0x2588

{- | Lower N-eighths block glyphs (index 0 = 1\/8 .. index 6 = 7\/8; 8\/8
is 'fullBlock') - fills a cell from its bottom edge, for the partially
filled tip of an upward-growing bar.
-}
eighthBlocksUp :: [Int]
eighthBlocksUp = [0x2581, 0x2582, 0x2583, 0x2584, 0x2585, 0x2586, 0x2587]

{- | Left N-eighths block glyphs, same idea, for the tip of a
rightward-growing horizontal bar.
-}
eighthBlocksRight :: [Int]
eighthBlocksRight = [0x258F, 0x258E, 0x258D, 0x258C, 0x258B, 0x258A, 0x2589]

{- | Bars, one per value, arranged either upright side by side
(@indexAlongX@ - a row selection) or flat, stacked top to bottom (a
column selection), spread evenly across the available space rather
than packed against one edge - each value gets a slot of its own,
filled mostly by the bar with a small gap to its neighbor when there's
room. Scaled by the largest magnitude present; if every value shares
one sign the baseline sits at the panel's own edge for maximum
resolution, otherwise it sits in the middle (with one row\/column given
up to a visible zero-line) and bars grow both ways. The
away-from-baseline direction only has whole-cell resolution, not the
towards-baseline direction's eighth-block precision - Unicode's
partial-block glyphs only cover the near-baseline orientations this
uses (lower-eighths, left-eighths), not their mirror images.

Each bar is labeled with the header text 'chartLabels' reads for it,
not its own value - along the bottom for upright bars (plenty of
horizontal room there for a short label), or in a narrow column at the
start for flat ones (labels need their own horizontal run, so they
can't share a bar's single row the way an upright bar's label can share
its column).
-}
drawBars ::
  Int -> Int -> Int -> Int -> Bool -> [String] -> [Double] -> UI.Screen ()
drawBars left top width height indexAlongX labels vals
  | null shown = return ()
  | otherwise = forM_ (zip3 [0 ..] shownLabels shown) $ \(i, lbl, v) -> do
      let fg = chartPalette !! (i `mod` length chartPalette)
          slotStart = indexOrigin + i * slotSize
          label = take labelSpace lbl
      forM_ [0 .. barThickness - 1] $ \o ->
        if indexAlongX
          then drawVBar fg (slotStart + o) baseline (toEighths v)
          else drawHBar fg (slotStart + o) baseline (toEighths v)
      if indexAlongX
        then UI.drawText slotStart labelRow fg chartBg label
        else UI.drawText labelCol slotStart fg chartBg label
 where
  labelSpace = 6
  -- \| Upright bars give up their bottom row for labels; flat bars give
  -- up a narrow column at the start instead - see the function doc.
  (indexOrigin, indexTotal, barLeft, barTop, barWidth, barHeight)
    | indexAlongX = (left, width, left, top, width, height - 1)
    | otherwise =
        ( top
        , height
        , left + labelSpace
        , top
        , max 1 (width - labelSpace)
        , height
        )
  labelRow = top + height - 1
  labelCol = left
  shown = take indexTotal vals
  shownLabels = take indexTotal (labels ++ repeat "")
  n = length shown
  slotSize = max 1 (indexTotal `div` n)
  barThickness = max 1 (slotSize - 1)
  magSpace = if indexAlongX then barHeight else barWidth
  hasNeg = any (< 0) shown
  scale = max 1 (maximum (map abs shown))
  -- \| Mixed signs give up one more row\/column, right at the baseline,
  -- as a visible zero-line between the positive and negative bars.
  posSpace = if hasNeg then (magSpace - 1) `div` 2 else magSpace
  negSpace = if hasNeg then (magSpace - 1) - posSpace else 0
  -- \| Vertical growth is "up" via 'drawVBar's @baseline - i@, so the
  -- positive baseline sits past the far (bottom) edge either way.
  -- Horizontal growth is "right" via 'drawHBar's @baseline + i@, so an
  -- all-positive baseline instead sits just before the near (left)
  -- edge - the two orientations aren't mirror images of each other here.
  baseline
    | indexAlongX = barTop + posSpace
    | hasNeg = barLeft + negSpace
    | otherwise = barLeft - 1
  toEighths v =
    let space = if v >= 0 then posSpace else negSpace
        e = min (space * 8) (round (abs v / scale * fromIntegral (space * 8)))
     in if v >= 0 then e else negate e

{- | One upright bar, in the given color, at screen column @x@, growing up
from @baseline@ for a positive @eighths@, or down (whole rows only)
for a negative one.
-}
drawVBar :: Tb2.Tb2ColorAttr -> Int -> Int -> Int -> UI.Screen ()
drawVBar fg x baseline eighths
  | eighths >= 0 = do
      forM_ [1 .. fullRows] $ \i ->
        UI.drawGlyph x (baseline - i) fg chartBg fullBlock
      when (partial > 0) $
        UI.drawGlyph
          x
          (baseline - fullRows - 1)
          fg
          chartBg
          (eighthBlocksUp !! (partial - 1))
  | otherwise =
      forM_ [1 .. downRows] $ \i ->
        UI.drawGlyph x (baseline + i) fg chartBg fullBlock
 where
  fullRows = eighths `div` 8
  partial = eighths `mod` 8
  downRows = (abs eighths + 7) `div` 8

{- | One flat bar, in the given color, at screen row @y@, growing right
from @baseline@ for a positive @eighths@, or left (whole columns
only) for a negative one.
-}
drawHBar :: Tb2.Tb2ColorAttr -> Int -> Int -> Int -> UI.Screen ()
drawHBar fg y baseline eighths
  | eighths >= 0 = do
      forM_ [1 .. fullCols] $ \i ->
        UI.drawGlyph (baseline + i) y fg chartBg fullBlock
      when (partial > 0) $
        UI.drawGlyph
          (baseline + fullCols + 1)
          y
          fg
          chartBg
          (eighthBlocksRight !! (partial - 1))
  | otherwise = forM_ [1 .. leftCols] $ \i ->
      UI.drawGlyph (baseline - i) y fg chartBg fullBlock
 where
  fullCols = eighths `div` 8
  partial = eighths `mod` 8
  leftCols = (abs eighths + 7) `div` 8

{- | The standard terminal line-plotting trick: each character cell is a
2x4 grid of braille dots (U+2800 plus a bit per dot, via 'UI.drawGlyph's
already-arbitrary codepoint support), giving 2x\/4x sub-cell resolution
on the index\/value axes respectively - which one is which follows
@indexAlongX@, the same as 'drawBars'. Consecutive points are connected
by filling in every sub-column crossed between them, so the result
reads as a continuous line rather than a scatter of dots. The highest
and lowest values are labeled in the top-left and bottom-left corners -
a plain scale hint rather than a real per-point label, which a dense
line has no clean room for.
-}
drawLine :: Int -> Int -> Int -> Int -> Bool -> [Double] -> UI.Screen ()
drawLine left top width height indexAlongX vals
  | length vals < 2 = return ()
  | otherwise = do
      forM_ (Map.toList dots) $ \((cx, cy), bits) ->
        UI.drawGlyph (left + cx) (top + cy) chartFg chartBg (0x2800 + bits)
      UI.drawText
        left
        top
        chartFg
        chartBg
        (take width ("max " ++ showValue (VNum hi)))
      UI.drawText
        left
        (top + height - 1)
        chartFg
        chartBg
        (take width ("min " ++ showValue (VNum lo)))
 where
  n = length vals
  lo = minimum vals
  hi = maximum vals
  subIndexMax =
    (if indexAlongX then width else height) * (if indexAlongX then 2 else 4)
  subValueMax =
    (if indexAlongX then height else width) * (if indexAlongX then 4 else 2)
  toSubIndex i =
    round
      ( fromIntegral i
          / fromIntegral (max 1 (n - 1))
          * fromIntegral (subIndexMax - 1) ::
          Double
      )
  toSubValue v
    | hi == lo = subValueMax `div` 2
    | otherwise =
        subValueMax
          - 1
          - round
            ( (v - lo)
                / (hi - lo)
                * fromIntegral (subValueMax - 1)
            )
  points =
    [ if indexAlongX
        then (toSubIndex i, toSubValue v)
        else (toSubValue v, toSubIndex i)
    | (i, v) <- zip [0 :: Int ..] vals
    ]
  allDots = concatMap (uncurry lineDots) (zip points (drop 1 points))
  dots =
    Map.fromListWith
      (.|.)
      [ ((px `div` 2, py `div` 4), brailleBit (px `mod` 2) (py `mod` 4))
      | (px, py) <- allDots
      ]

{- | Every sub-pixel dot on the straight line between two points, one per
sub-column crossed - plenty for the handful of sub-pixels a terminal
panel actually has.
-}
lineDots :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
lineDots (x0, y0) (x1, y1)
  | x0 == x1 = [(x0, y) | y <- [min y0 y1 .. max y0 y1]]
  | otherwise =
      [ ( x
        , y0
            + round
              ( fromIntegral (x - x0)
                  / fromIntegral (x1 - x0)
                  * fromIntegral (y1 - y0) ::
                  Double
              )
        )
      | x <- [min x0 x1 .. max x0 x1]
      ]

{- | The Unicode braille pattern's dot-to-bit mapping, a 2 (sub-x) by 4
(sub-y) grid within one character cell.
-}
brailleBit :: Int -> Int -> Int
brailleBit 0 0 = 0x01
brailleBit 0 1 = 0x02
brailleBit 0 2 = 0x04
brailleBit 0 3 = 0x40
brailleBit 1 0 = 0x08
brailleBit 1 1 = 0x10
brailleBit 1 2 = 0x20
brailleBit 1 3 = 0x80
brailleBit _ _ = 0
