{- |
Module: Update.Chart
Description: Opening, closing, and switching the chart overlaid on the
current selection.
-}
module Update.Chart (
  toggleChart,
) where

import SheetState (
  Chart (..),
  ChartType,
  SheetState (..),
  classifyChartRange,
 )
import qualified Trellis.UI as UI

{- | Pressing a chart's own key again closes it; pressing a different
chart's key switches to it, if the current selection qualifies (see
'classifyChartRange'); pressing a chart's key against a selection that
doesn't qualify for it leaves whatever's currently showing untouched,
rather than clearing it.
-}
toggleChart :: ChartType -> UI.Action (UI.Store SheetState) IO ()
toggleChart ct = UI.modify $ \st ->
  case chart st of
    Just c | chartType c == ct -> st{chart = Nothing}
    _ -> case classifyChartRange ct (selection st) of
      Just newChart -> st{chart = Just newChart}
      Nothing -> st
