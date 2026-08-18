{- |
Module: SheetState
Description: The spreadsheet's state - see "SheetState.Geometry" for the
screen-layout math derived from it.
-}
module SheetState (
  SheetState (..),
  EditorState (..),
  LiveBinding (..),
  FillSource (..),
  classifySelection,
  previewRect,
  ChartType (..),
  Chart (..),
  classifyChartRange,
  heatmapStep,
  initialState,
  doubleClickWindow,
  gutterWidth,
  defaultCellWidth,
  minCellWidth,
  maxCellWidth,
  headerHeight,
  rowStride,
  statusBarHeight,
  visibleCols,
  visibleRows,
  colBoundaries,
  clampAxis,
  clampOrigin,
  cellAt,
  clampRange,
  cellsInSelection,
) where

import qualified Data.Map.Strict as Map
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Formula (Expr)
import SheetState.Chart (
  Chart (..),
  ChartType (..),
  classifyChartRange,
  heatmapStep,
 )
import SheetState.Fill (FillSource (..), classifySelection, previewRect)
import SheetState.Geometry (
  cellAt,
  clampAxis,
  clampOrigin,
  clampRange,
  colBoundaries,
  defaultCellWidth,
  gutterWidth,
  headerHeight,
  initialCellWidth,
  maxCellWidth,
  minCellWidth,
  rowStride,
  statusBarHeight,
  visibleCols,
  visibleRows,
 )
import qualified Trellis.Orc as Orc
import qualified Trellis.UI as UI

data SheetState = SheetState
  { cursor :: (Int, Int)
  , viewportOrigin :: (Int, Int)
  , cellWidth :: Int
  {- ^ Zoom level, in effect: how many screen columns each sheet column
  occupies. 'rowStride' is derived from this rather than tracked
  separately, so zooming can't leave the two dimensions out of sync.
  -}
  , cells :: Map.Map (Int, Int) Expr
  , editor :: Maybe EditorState
  {- ^ A formula editor open over the sheet, or 'Nothing' when just
  navigating. Closing it - via 'Update.Core.runEditor' - resumes whatever
  code was waiting on its result, through 'editorResume'.
  -}
  , lastClick :: Maybe ((Int, Int), UTCTime)
  {- ^ Where and when the most recent fresh left-button press landed, to
  tell a double-click from two unrelated clicks on the same cell -
  termbox2 has no native double-click event, only presses.
  -}
  , panAnchor :: Maybe (Int, Int)
  {- ^ The screen position an in-progress pan drag last moved the
  viewport from. 'Nothing' before a pan starts and right after release,
  so the next press always starts a fresh anchor.
  -}
  , subscriptions :: Map.Map (Int, Int) LiveBinding
  {- ^ Cells fed by a live\/async 'Trellis.Orc' subscription instead of
  an ordinary formula. See 'Live.declareSubscription'.
  -}
  , outBindings :: Map.Map (Int, Int) FilePath
  {- ^ Cells published out to a pipe\/file whenever their value changes -
  set once at startup from "--out" flags and read-only after that. Just
  for 'Render.hs' to know which cells to color; the live handles
  ('Live.OutBinding') are threaded through 'Update.update' separately.
  -}
  , selection :: Maybe ((Int, Int), (Int, Int))
  {- ^ The rectangle a 'selectButton' drag last covered, anchor and
  endpoint - unlike 'panAnchor'\/'fillDrag', not cleared on release: it
  needs to survive until a later, separate fillButton drag reads it
  (see 'SheetState.Fill.classifySelection'). Cleared by @cancel@
  instead.
  -}
  , fillDrag :: Maybe (FillSource, (Int, Int))
  {- ^ An in-progress fill drag: what's being replicated (captured once
  at press time via 'classifySelection') and the cell the drag is
  currently over (updates live). 'Nothing' before a fill starts and
  right after release, so the next press always starts a fresh one -
  see 'Update.Fill.fillDragTo'.
  -}
  , chart :: Maybe Chart
  {- ^ An open chart over the last 'selection' made, or 'Nothing' when
  none is showing. Unlike 'editor', doesn't suspend navigation - see
  'Update.Chart.toggleChart'.
  -}
  , helpModal :: Bool
  {- ^ When True the help overlay is showing; all navigation/editing input
  is suppressed until Escape (or the help key again) clears it.
  -}
  , helpScroll :: Int
  {- ^ How many lines of 'Render.Help.helpContent' are scrolled past, when
  'helpModal' is showing - reset to 0 each time the modal opens. Only
  matters when the content is taller than fits on screen.
  -}
  }

{- | A cell bound to a live\/async source: the 'Trellis.Orc' subscription
writing into it, and the raw spec text so re-opening the cell for edit
shows what was typed, not the last value the subscription produced.
-}
data LiveBinding = LiveBinding
  { liveGroup :: Orc.Group
  , liveSpecText :: String
  }

{- | A formula editor's live state: the text typed so far, and what to do
once it closes. 'editorResume' is a 'Trellis.CPS.shift'-captured
continuation (see 'Update.Core.runEditor') stored as an ordinary function value.
-}
data EditorState = EditorState
  { editorBuffer :: String
  , editorCursor :: Int
  {- ^ Character offset into 'editorBuffer'; @0 <= editorCursor <= length
  editorBuffer@.
  -}
  , editorResume :: Maybe String -> UI.Action (UI.Store SheetState) IO ()
  {- ^ 'Nothing' on cancel; 'Just text' on commit, where 'text' is either
  empty (clear the cell), a valid formula, or a valid live spec -
  'Update.Core.editing' only closes the modal once one of those is true.
  -}
  }

initialState :: SheetState
initialState =
  SheetState
    (0, 0)
    (0, 0)
    initialCellWidth
    Map.empty
    Nothing
    Nothing
    Nothing
    Map.empty
    Map.empty
    Nothing
    Nothing
    Nothing
    False
    0

-- | How close together two clicks on the same cell must be to count as one.
doubleClickWindow :: NominalDiffTime
doubleClickWindow = 0.4

{- | Enumerate all cell positions covered by a selection rectangle.
If no selection is active, returns just the cursor cell.
The two corners of the selection may be given in either order.
-}
cellsInSelection :: (Int, Int) -> Maybe ((Int, Int), (Int, Int)) -> [(Int, Int)]
cellsInSelection cursor Nothing = [cursor]
cellsInSelection _ (Just ((x1, y1), (x2, y2))) =
  [ (x, y)
  | x <- [min x1 x2 .. max x1 x2]
  , y <- [min y1 y2 .. max y1 y2]
  ]
