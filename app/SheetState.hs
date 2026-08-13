{- |
Module: SheetState
Description: The spreadsheet's state, and the geometry both rendering and
event handling read to lay it out.
-}
module SheetState (
  SheetState (..),
  EditorState (..),
  LiveBinding (..),
  initialState,
  gutterWidth,
  defaultCellWidth,
  minCellWidth,
  maxCellWidth,
  doubleClickWindow,
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
) where

import qualified Data.Map.Strict as Map
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Formula (Expr)
import qualified Trellis.Orc as Orc
import qualified Trellis.UI as UI

gutterWidth :: Int
gutterWidth = 5

{- | Not fixed - the mouse wheel zooms 'cellWidth' live (see 'Update.zoomBy'),
bounded so a cell never gets unreadably narrow or wide enough to stop
looking like a grid.
-}
defaultCellWidth, minCellWidth, maxCellWidth :: Int
defaultCellWidth = 8
minCellWidth = 4
maxCellWidth = 28

{- | What the sheet opens at: a few scroll-wheel clicks in from
'defaultCellWidth', which itself stays the true unzoomed reference point
'rowStride' needs.
-}
initialCellWidth :: Int
initialCellWidth = defaultCellWidth + 3

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
  navigating. Closing it - via 'Update.runEditor' - resumes whatever code
  was waiting on its result, through 'editorResume'.
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
continuation (see 'Update.runEditor') stored as an ordinary function value.
-}
data EditorState = EditorState
  { editorBuffer :: String
  , editorCursor :: Int
  -- ^ Character offset into 'editorBuffer'; @0 <= editorCursor <= length editorBuffer@.
  , editorResume :: Maybe String -> UI.Action (UI.Store SheetState) IO ()
  {- ^ 'Nothing' on cancel; 'Just text' on commit, where 'text' is either
  empty (clear the cell), a valid formula, or a valid live spec -
  'Update.editing' only closes the modal once one of those is true.
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

-- | How close together two clicks on the same cell must be to count as one.
doubleClickWindow :: NominalDiffTime
doubleClickWindow = 0.4

visibleCols :: Int -> Int -> Int
visibleCols cw w = max 1 ((w - gutterWidth) `div` cw)

-- | The column-header text row.
headerHeight :: Int
headerHeight = 1

{- | Screen rows each visible sheet row occupies: a ruled line above, one
row of content, plus extra padding from zooming (see 'cellWidth'). [^1]
-}
rowStride :: Int -> Int
rowStride cw = max 2 (2 + (cw - defaultCellWidth) `div` 4)

-- | The row 'Trellis.UI.statusText' occupies at the bottom of the terminal.
statusBarHeight :: Int
statusBarHeight = 1

visibleRows :: Int -> Int -> Int
visibleRows cw h = max 1 ((h - headerHeight - statusBarHeight - 1) `div` rowStride cw)

{- | Screen x position of each column boundary: one before the first cell,
one after every cell thereafter - @cols + 1@ positions in all.
-}
colBoundaries :: Int -> Int -> [Int]
colBoundaries cw cols = [gutterWidth - 1 + i * cw | i <- [0 .. cols]]

{- | Shift a stored viewport origin by the minimum amount needed to keep a
new cursor position in view, on one axis - scroll only when the cursor
would walk off the edge, otherwise leave it where it was.
-}
clampAxis :: Int -> Int -> Int -> Int
clampAxis visible c o
  | c < o = c
  | c > o + visible - 1 = c - visible + 1
  | otherwise = o

-- | 'clampAxis', applied to both axes of a viewport origin at once.
clampOrigin :: Int -> (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int)
clampOrigin cw (cx, cy) (w, h) (ox, oy) =
  (clampAxis (visibleCols cw w) cx ox, clampAxis (visibleRows cw h) cy oy)

{- | Which sheet cell, if any, sits under a screen position, using the
viewport origin 'Render.render' laid the grid out with. 'Nothing' means
the header row or gutter.
-}
cellAt :: Int -> (Int, Int) -> Int -> Int -> Maybe (Int, Int)
cellAt cw (ox, oy) x y
  | y < headerHeight = Nothing
  | (y - headerHeight) `mod` rowStride cw == 0 = Nothing -- on a ruled line
  | x < gutterWidth = Nothing
  | otherwise =
      Just
        ( ox + (x - gutterWidth) `div` cw
        , oy + (y - headerHeight) `div` rowStride cw
        )

-- | Clamp an 'Int' into an inclusive range - used for zoom's 'cellWidth' step.
clampRange :: Int -> Int -> Int -> Int
clampRange lo hi x = max lo (min hi x)

{- [^1]:
Every row gets a line both above and below (the one below shared with the
next row's line above, except the last row's, which closes the bottom of
the grid) - so even at the minimum stride of 2, half the terminal's height
goes to ruling rather than data.
-}
