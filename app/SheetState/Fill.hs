{- |
Module: SheetState.Fill
Description: What a fillButton drag replicates - a single cell, or a
whole row\/column selection - and the rectangle its live preview covers.
None of it depends on 'SheetState.SheetState' itself.
-}
module SheetState.Fill (
  FillSource (..),
  classifySelection,
  previewRect,
) where

-- | What a fillButton drag is replicating.
data FillSource
  = FillCell (Int, Int)
  | FillRow Int (Int, Int)
  {- ^ A fixed row and the @(xMin, xMax)@ columns of the selection that
  sits on it.
  -}
  | FillCol Int (Int, Int)
  {- ^ A fixed column and the @(yMin, yMax)@ rows of the selection that
  sits on it.
  -}
  deriving (Eq, Show)

{- | What a fillButton press at @target@ should replicate, given the
current selection (if any): the whole selection if @target@ falls on a
genuine row or column line within it - a degenerate 1x1 selection or a
real 2D block (both extents > 1) don't qualify - otherwise just
@target@ itself, 'FillCell', matching fillButton's single-cell behavior
from before selections existed at all.
-}
classifySelection :: (Int, Int) -> Maybe ((Int, Int), (Int, Int)) -> FillSource
classifySelection target Nothing = FillCell target
classifySelection target@(tx, ty) (Just (a, b))
  | y0 == y1 && x0 < x1 && within = FillRow y0 (x0, x1)
  | x0 == x1 && y0 < y1 && within = FillCol x0 (y0, y1)
  | otherwise = FillCell target
 where
  (x0, x1) = (min (fst a) (fst b), max (fst a) (fst b))
  (y0, y1) = (min (snd a) (snd b), max (snd a) (snd b))
  within = tx >= x0 && tx <= x1 && ty >= y0 && ty <= y1

{- | The rectangle a live fill-preview should highlight, given the
source and the drag's current endpoint - for a line source, spans the
full selection width\/height, from the source line to wherever the
drag currently is in the perpendicular direction.
-}
previewRect :: FillSource -> (Int, Int) -> ((Int, Int), (Int, Int))
previewRect (FillCell anchor) endpoint = (anchor, endpoint)
previewRect (FillRow y0 (x0, x1)) (_, yT) = ((x0, y0), (x1, yT))
previewRect (FillCol x0 (y0, y1)) (xT, _) = ((x0, y0), (xT, y1))
