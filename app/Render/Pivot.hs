{- |
Module: Render.Pivot
Description: The pivot-table overlay - a floating panel over the sheet,
grouping a selected two-column range by its first column and showing
count\/sum\/average of the second, per distinct category. Reuses
"Render.Chart"'s box-drawing style for the frame.
-}
module Render.Pivot (
  renderPivot,
) where

import Control.Monad (forM_)
import Data.List (nub)
import Formula (AggOp (..), Value (..), aggregate, evaluated, showValue, window)
import Render.Chart (chartPanelHeight, chartPanelWidth)
import Render.Theme (
  chartBg,
  chartFg,
  modalBorderBg,
  modalBorderFg,
  padTo,
  textBg,
  textFg,
 )
import SheetState (Pivot (..), SheetState (..))
import Trellis.Sheet (Sheet2)
import qualified Trellis.UI as UI

renderPivot :: SheetState -> UI.Screen ()
renderPivot st = case pivot st of
  Nothing -> return ()
  Just p -> do
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
        countW = 7
        sumW = 10
        avgW = 10
        labelW = max 6 (innerWidth - (countW + sumW + avgW + 3))
        rows =
          take
            (innerHeight - 1)
            (pivotRows (pivotRange p) (evaluated (cells st)))
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
    UI.drawText
      innerLeft
      innerTop
      chartFg
      chartBg
      ( padTo labelW "Category"
          ++ " "
          ++ padLeft countW "Count"
          ++ " "
          ++ padLeft sumW "Sum"
          ++ " "
          ++ padLeft avgW "Avg"
      )
    forM_ (zip [1 ..] rows) $ \(i, (cat, cnt, sm, av)) ->
      UI.drawText
        innerLeft
        (innerTop + i)
        textFg
        textBg
        ( padTo labelW cat
            ++ " "
            ++ padLeft countW (showValue cnt)
            ++ " "
            ++ padLeft sumW (showValue sm)
            ++ " "
            ++ padLeft avgW (showValue av)
        )

{- | Every distinct value in the range's first column, in first-seen order
(not sorted - a 'Data.Map' would silently re-sort), paired with count\/
sum\/average of the second column over the rows sharing it. A non-numeric
value-column cell reads as @0@ for sum\/average - the same lenient stance
'Render.Chart.chartValues' takes - but still counts toward count. Every
group is non-empty by construction and pre-mapped to 'VNum', so
'aggregate's error\/empty-range branches are unreachable here; it
degrades to a plain fold, reused rather than reimplemented.
-}
pivotRows ::
  ((Int, Int), (Int, Int)) -> Sheet2 Value -> [(String, Value, Value, Value)]
pivotRows ((x0, y0), (x1, y1)) sh =
  [ (cat, aggregate CountOp vs, aggregate SumOp vs, aggregate AvgOp vs)
  | cat <- categories
  , let vs = [v | (c, v) <- pairs, c == cat]
  ]
 where
  cols = x1 - x0 + 1 -- always 2, by classifyPivotRange's own rule
  rows = y1 - y0 + 1
  -- \| 'window' over-fetches by one in each direction - trimmed back down
  -- here, same as 'Render.Chart.chartValues'.
  raw = map (take cols) (take rows (window (x0, y0) cols rows sh))
  pairs = [(showValue c, asNum v) | [c, v] <- raw]
  asNum v@(VNum _) = v
  asNum _ = VNum 0
  categories = nub (map fst pairs)

{- | Right-aligned, unlike 'Render.Theme.padTo' - no existing helper does
this, so it's defined locally, unexported, the way "Render.Chart"
keeps its own small helpers rather than growing "Render.Theme".
-}
padLeft :: Int -> String -> String
padLeft width str = replicate (width - length clipped) ' ' ++ clipped
 where
  clipped = take width str
