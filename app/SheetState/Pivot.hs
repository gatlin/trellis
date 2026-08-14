{- |
Module: SheetState.Pivot
Description: What a pivot-table command means as pure data - whether a
selection qualifies. None of it depends on 'SheetState.SheetState'
itself, same as "SheetState.Chart".
-}
module SheetState.Pivot (
  Pivot (..),
  classifyPivotRange,
) where

-- | An active pivot table: the rectangle (normalized corners) it summarizes.
newtype Pivot = Pivot
  { pivotRange :: ((Int, Int), (Int, Int))
  }
  deriving (Eq, Show)

{- | Does the current selection qualify for a pivot table? Exactly two
columns - a category column and a value column - and any number of rows
(including one). No selection at all never qualifies, same as
'SheetState.Chart.classifyChartRange'.
-}
classifyPivotRange :: Maybe ((Int, Int), (Int, Int)) -> Maybe Pivot
classifyPivotRange Nothing = Nothing
classifyPivotRange (Just (a, b))
  | x1 - x0 == 1 = Just (Pivot range)
  | otherwise = Nothing
 where
  (x0, x1) = (min (fst a) (fst b), max (fst a) (fst b))
  (y0, y1) = (min (snd a) (snd b), max (snd a) (snd b))
  range = ((x0, y0), (x1, y1))
