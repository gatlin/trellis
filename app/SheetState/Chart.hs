{- |
Module: SheetState.Chart
Description: What a chart command means as pure data - whether a
selection qualifies for a given chart type, and the color-ramp math a
heatmap uses. None of it depends on 'SheetState.SheetState' itself.
-}
module SheetState.Chart (
  ChartType (..),
  Chart (..),
  classifyChartRange,
  heatmapStep,
) where

-- | Which kind of chart a 'Chart' is.
data ChartType = BarChart | LineChart | Heatmap
  deriving (Eq, Show)

{- | An active chart: its type, and the rectangle (normalized corners) it's
charting.
-}
data Chart = Chart
  { chartType :: ChartType
  , chartRange :: ((Int, Int), (Int, Int))
  }
  deriving (Eq, Show)

{- | Does the current selection qualify for this chart type? 'BarChart'\/
'LineChart' need a clean single row or column - a degenerate 1x1
selection counts (a one-bar\/one-point chart is harmless), but a genuine
2D block doesn't. 'Heatmap' accepts any non-empty rectangle - the one of
the three that actually uses full 2D ranges. No selection at all never
qualifies, for any type.
-}
classifyChartRange :: ChartType -> Maybe ((Int, Int), (Int, Int)) -> Maybe Chart
classifyChartRange _ Nothing = Nothing
classifyChartRange ct (Just (a, b))
  | ct == Heatmap = Just (Chart ct range)
  | x0 == x1 || y0 == y1 = Just (Chart ct range)
  | otherwise = Nothing
 where
  (x0, x1) = (min (fst a) (fst b), max (fst a) (fst b))
  (y0, y1) = (min (snd a) (snd b), max (snd a) (snd b))
  range = ((x0, y0), (x1, y1))

{- | Which step of a fixed-size color ramp a value falls into, given the
charted range's own min\/max - step @0@ at the minimum, @steps - 1@ at
the maximum, clamped either way in case rounding pushes a value
fractionally outside its own range. A degenerate range (every charted
cell holds the same value, so @min == max@) puts everything in the
middle step rather than dividing by zero.
-}
heatmapStep :: Int -> (Double, Double) -> Double -> Int
heatmapStep steps (lo, hi) v
  | steps <= 1 = 0
  | hi == lo = (steps - 1) `div` 2
  | otherwise = max 0 (min (steps - 1) (round (t * fromIntegral (steps - 1))))
 where
  t = (v - lo) / (hi - lo)
