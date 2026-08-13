{- |
Module: Update.Fill
Description: The fill-drag gesture - replicating a cell's formula, or a
whole row\/column selection, across a dragged rectangle, with
references adjusted rather than copied verbatim.
-}
module Update.Fill (
  fillDragTo,
  commitFill,
) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Formula (Expr, adjustRefs)
import SheetState (
  FillSource (..),
  SheetState (..),
  cellAt,
  classifySelection,
 )
import qualified Trellis.UI as UI
import Update.Subscriptions (cancelSubscription)

{- | Both the first press and every drag motion of the fill gesture route
through here, same as 'Update.Navigation.panBy' - tells them apart
itself via whether 'fillDrag' is already set. A fresh press classifies
what's being replicated once, via 'classifySelection'; a continuing
drag just updates the endpoint, keeping that source fixed.
-}
fillDragTo :: Int -> Int -> UI.Action (UI.Store SheetState) IO ()
fillDragTo screenX screenY = do
  st <- UI.get
  case cellAt (cellWidth st) (viewportOrigin st) screenX screenY of
    Nothing -> return ()
    Just target -> case fillDrag st of
      Nothing ->
        UI.modify
          ( \st' ->
              st'{fillDrag = Just (classifySelection target (selection st'), target)}
          )
      Just (src, _) -> UI.modify (\st' -> st'{fillDrag = Just (src, target)})

{- | Replicates whatever 'fillDrag' captured at press time - a single
cell across a 2D rectangle, or a whole row\/column selection across the
rows\/columns the drag added - with references adjusted to match. A
blank source (cell or, per-column\/per-row, within a line) is a no-op
there, not a clear. Doesn't clear 'fillDrag' itself - the caller does
that alongside 'panAnchor' on release, since termbox2 can't say which
button released.
-}
commitFill :: UI.Action (UI.Store SheetState) IO ()
commitFill = do
  st <- UI.get
  case fillDrag st of
    Nothing -> return ()
    Just (FillCell anchor, endpoint) -> commitCellFill anchor endpoint
    Just (FillRow y0 (x0, x1), (_, yT)) -> commitRowFill y0 (x0, x1) yT
    Just (FillCol x0 (y0, y1), (xT, _)) -> commitColFill x0 (y0, y1) xT

{- | Today's single-cell fill: the anchor's formula, shifted by each
destination's own offset from it, across the rest of the rectangle.
-}
commitCellFill ::
  (Int, Int) -> (Int, Int) -> UI.Action (UI.Store SheetState) IO ()
commitCellFill anchor@(ax, ay) (ex, ey) = do
  st <- UI.get
  forM_ (Map.lookup anchor (cells st)) $ \srcExpr ->
    forM_
      [ (x, y)
      | x <- [min ax ex .. max ax ex]
      , y <- [min ay ey .. max ay ey]
      , (x, y) /= anchor
      ]
      (fillCell anchor srcExpr)

{- | Each column's own formula on the source row, shifted straight down
(or up) into every other row the drag added - not the same formula
copied everywhere, so a row of distinct per-column formulas (a
cellular-automaton rule, say) replicates as successive generations
once the sheet's own evaluation reads each new row from the one above.
-}
commitRowFill ::
  Int -> (Int, Int) -> Int -> UI.Action (UI.Store SheetState) IO ()
commitRowFill y0 (x0, x1) yT
  | yT == y0 = return ()
  | otherwise = do
      st <- UI.get
      forM_ [x0 .. x1] $ \x ->
        forM_ (Map.lookup (x, y0) (cells st)) $ \srcExpr ->
          forM_ [y | y <- [min y0 yT .. max y0 yT], y /= y0] $ \y ->
            fillCell (x, y0) srcExpr (x, y)

-- | The transpose of 'commitRowFill'.
commitColFill ::
  Int -> (Int, Int) -> Int -> UI.Action (UI.Store SheetState) IO ()
commitColFill x0 (y0, y1) xT
  | xT == x0 = return ()
  | otherwise = do
      st <- UI.get
      forM_ [y0 .. y1] $ \y ->
        forM_ (Map.lookup (x0, y) (cells st)) $ \srcExpr ->
          forM_ [x | x <- [min x0 xT .. max x0 xT], x /= x0] $ \x ->
            fillCell (x0, y) srcExpr (x, y)

{- | Writes @srcExpr@ into @pos@, adjusted by @pos@'s offset from
@source@, cancelling any subscription @pos@ already had - same as
every other cell-write path (@beginEdit@, @clearFocusedCell@).
-}
fillCell ::
  (Int, Int) -> Expr -> (Int, Int) -> UI.Action (UI.Store SheetState) IO ()
fillCell (sx, sy) srcExpr pos@(x, y) = do
  cancelSubscription pos
  UI.modify
    ( \st -> st{cells = Map.insert pos (adjustRefs (x - sx, y - sy) srcExpr) (cells st)}
    )
