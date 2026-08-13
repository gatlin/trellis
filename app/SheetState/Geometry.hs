{- |
Module: SheetState.Geometry
Description: Screen-layout dimensions and the pure math derived from
them - none of it depends on 'SheetState.SheetState' itself.
-}
module SheetState.Geometry (
  gutterWidth,
  defaultCellWidth,
  minCellWidth,
  maxCellWidth,
  initialCellWidth,
  headerHeight,
  statusBarHeight,
  rowStride,
  visibleCols,
  visibleRows,
  colBoundaries,
  clampAxis,
  clampOrigin,
  cellAt,
  clampRange,
) where

gutterWidth :: Int
gutterWidth = 5

{- | Not fixed - the mouse wheel zooms 'SheetState.cellWidth' live (see
'Update.Navigation.zoomBy'), bounded so a cell never gets unreadably
narrow or wide enough to stop looking like a grid.
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

-- | The column-header text row.
headerHeight :: Int
headerHeight = 1

-- | The row 'Trellis.UI.statusText' occupies at the bottom of the terminal.
statusBarHeight :: Int
statusBarHeight = 1

{- | Screen rows each visible sheet row occupies: a ruled line above, one
row of content, plus extra padding from zooming (see 'SheetState.cellWidth').
[^1]
-}
rowStride :: Int -> Int
rowStride cw = max 2 (2 + (cw - defaultCellWidth) `div` 4)

visibleCols :: Int -> Int -> Int
visibleCols cw w = max 1 ((w - gutterWidth) `div` cw)

visibleRows :: Int -> Int -> Int
visibleRows cw h =
  max 1 ((h - headerHeight - statusBarHeight - 1) `div` rowStride cw)

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
viewport origin 'Render.Grid.render' laid the grid out with. 'Nothing'
means the header row or gutter.
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

{- | Clamp an 'Int' into an inclusive range - used for zoom's
'SheetState.cellWidth' step.
-}
clampRange :: Int -> Int -> Int -> Int
clampRange lo hi x = max lo (min hi x)

{- [^1]:
Every row gets a line both above and below (the one below shared with the
next row's line above, except the last row's, which closes the bottom of
the grid) - so even at the minimum stride of 2, half the terminal's height
goes to ruling rather than data.
-}
