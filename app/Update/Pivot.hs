{- |
Module: Update.Pivot
Description: Opening and closing the pivot-table summary overlaid on the
current selection.
-}
module Update.Pivot (
  togglePivot,
) where

import SheetState (SheetState (..), classifyPivotRange)
import qualified Trellis.UI as UI

{- | Pressing the pivot key again closes it - there's only one "type" of
pivot table, unlike 'Update.Chart.toggleChart', so there's no "switch"
branch to mirror. Pressing it against a selection that doesn't qualify
(see 'classifyPivotRange') leaves whatever's showing untouched, same
stance 'toggleChart' takes. Opening a pivot closes any open chart, since
both overlays are centered at the same spot - see 'Update.Chart.toggleChart'
for the matching edit on the other side.
-}
togglePivot :: UI.Action (UI.Store SheetState) IO ()
togglePivot = UI.modify $ \st ->
  case pivot st of
    Just _ -> st{pivot = Nothing}
    Nothing -> case classifyPivotRange (selection st) of
      Just newPivot -> st{chart = Nothing, pivot = Just newPivot}
      Nothing -> st
