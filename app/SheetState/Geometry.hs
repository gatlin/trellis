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
  minScale,
  maxScale,
  colScaleStep,
  clampScale,
  colWidthAt,
  rowHeightAt,
  colBoundariesFrom,
  rowBoundariesFrom,
  visibleCols,
  visibleRows,
  clampAxis,
  clampOrigin,
  cellAt,
  clampRange,
) where

import qualified Data.Map.Strict as Map

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

{- | Screen rows each visible sheet row occupies, at the default (1.0)
row scale: a ruled line above, one row of content, plus extra padding
from zooming (see 'SheetState.cellWidth'). [^1]
-}
rowStride :: Int -> Int
rowStride cw = max 2 (2 + (cw - defaultCellWidth) `div` 4)

{- | Bounds on a per-column\/row scale factor (see 'SheetState.colScale'\/
'SheetState.rowScale'): a ratio relative to whatever the *current* zoom's
default size is, not a fixed pixel size - so a resize made at one zoom
level keeps its relative size after zooming in or out (multiplying a
positive width by a positive ratio can never go negative, unlike storing
a fixed pixel delta that a later zoom-out could shrink past zero).
Independent of 'minCellWidth'\/'maxCellWidth', which separately bound the
actual rendered size at any given zoom - see 'colWidthAt'.
-}
minScale, maxScale :: Double
minScale = 0.2
maxScale = 5.0

-- | How much one resize keypress multiplies a column\/row's scale factor by.
colScaleStep :: Double
colScaleStep = 1.15

-- | Clamp a scale factor into @['minScale', 'maxScale']@.
clampScale :: Double -> Double
clampScale x = max minScale (min maxScale x)

{- | A column's actual on-screen width at the current zoom: the zoom
level scaled by whatever ratio has been set for this specific column (via
'SheetState.colScale'), defaulting to 1 (unscaled) for every column
that's never been individually resized. Clamped to
'minCellWidth'\/'maxCellWidth', the same bounds the uniform zoom always
respected, so an aggressively resized column still can't get unreadably
narrow or wide enough to break the grid.
-}
colWidthAt :: Int -> Map.Map Int Double -> Int -> Int
colWidthAt cw scales col =
  clampRange minCellWidth maxCellWidth $
    round (fromIntegral cw * Map.findWithDefault 1 col scales)

-- | Like 'colWidthAt', for a row's height - scales 'rowStride' instead.
rowHeightAt :: Int -> Map.Map Int Double -> Int -> Int
rowHeightAt cw scales row =
  max 2 (round (fromIntegral (rowStride cw) * Map.findWithDefault 1 row scales))

{- | Cumulative screen-position boundaries for a run of items (columns or
rows) of possibly-differing sizes, starting at index @start@ from screen
position @startOffset@, continuing only while there's still room within
@limit@ - i.e. exactly as many boundaries as fit, plus one trailing entry
marking where the next (off-screen) item would begin. Item @start + i@'s
screen span is @(boundaries !! i, boundaries !! (i+1))@.
-}
boundariesFrom :: (Int -> Int) -> Int -> Int -> Int -> [Int]
boundariesFrom sizeOf startOffset limit start =
  startOffset : go startOffset start
 where
  -- \| Checks the position *after* adding the next item, not before -
  -- an item only counts as fully visible (and its trailing boundary
  -- only gets included at all) if that boundary itself still fits
  -- within @limit@. Getting this backward once let a boundary
  -- overshoot past the real screen edge and reach termbox2 as an
  -- out-of-bounds draw (TB_ERR_OUT_OF_BOUNDS) - confirmed by actually
  -- running the app, not just the type checker or unit tests.
  go pos i =
    let pos' = pos + sizeOf i
     in if pos' > limit then [] else pos' : go pos' (i + 1)

{- | 'boundariesFrom' for sheet columns - replaces the old uniform
@colBoundaries cw cols = [gutterWidth - 1 + i * cw | i <- [0..cols]]@
now that columns can have independent widths; reduces to an equivalent
sequence when @scales@ is empty.
-}
colBoundariesFrom :: Int -> Map.Map Int Double -> Int -> Int -> Int -> [Int]
colBoundariesFrom cw scales = boundariesFrom (colWidthAt cw scales)

-- | 'boundariesFrom' for sheet rows.
rowBoundariesFrom :: Int -> Map.Map Int Double -> Int -> Int -> Int -> [Int]
rowBoundariesFrom cw scales = boundariesFrom (rowHeightAt cw scales)

-- | How many whole sheet columns, starting at @startCol@, fit within
-- screen width @w@.
visibleCols :: Int -> Map.Map Int Double -> Int -> Int -> Int
visibleCols cw scales startCol w =
  max 1 (length (colBoundariesFrom cw scales gutterWidth w startCol) - 1)

-- | How many whole sheet rows, starting at @startRow@, fit within
-- screen height @h@.
visibleRows :: Int -> Map.Map Int Double -> Int -> Int -> Int
visibleRows cw scales startRow h =
  max
    1
    ( length
        (rowBoundariesFrom cw scales headerHeight (h - statusBarHeight - 1) startRow)
        - 1
    )

{- | Shift a stored viewport origin by the minimum amount needed to keep
a new cursor position in view, on one axis - scroll only when the
cursor would walk off the edge, otherwise leave it where it was. Takes
each item's actual size as a function rather than assuming a fixed
visible count, so it works the same whether every item is uniformly
sized or not.
-}
clampAxis :: (Int -> Int) -> Int -> Int -> Int -> Int -> Int
clampAxis sizeOf startOffset limit c o
  | c < o = c
  | lastVisible sizeOf startOffset limit o >= c = o
  | otherwise = originShowingLast sizeOf startOffset limit c

-- | The index of the last fully-visible item, scrolled to @origin@.
lastVisible :: (Int -> Int) -> Int -> Int -> Int -> Int
lastVisible sizeOf startOffset limit origin =
  origin + length (boundariesFrom sizeOf startOffset limit origin) - 2

{- | The minimal origin so that @target@ becomes the *last* fully-visible
item: scans backward from @target@, accumulating size, growing the
window while it still fits within the available budget.
-}
originShowingLast :: (Int -> Int) -> Int -> Int -> Int -> Int
originShowingLast sizeOf startOffset limit target =
  go (target - 1) (sizeOf target) target
 where
  budget = limit - startOffset
  go o total best
    | o < 0 = best
    | total + sizeOf o > budget = best
    | otherwise = go (o - 1) (total + sizeOf o) o

-- | 'clampAxis', applied to both axes of a viewport origin at once.
clampOrigin ::
  Int ->
  Map.Map Int Double ->
  Map.Map Int Double ->
  (Int, Int) ->
  (Int, Int) ->
  (Int, Int) ->
  (Int, Int)
clampOrigin cw colScales rowScales (cx, cy) (w, h) (ox, oy) =
  ( clampAxis (colWidthAt cw colScales) gutterWidth w cx ox
  , clampAxis
      (rowHeightAt cw rowScales)
      headerHeight
      (h - statusBarHeight - 1)
      cy
      oy
  )

{- | Which item (by index, at or after @start@) a screen position falls
into, given how items starting there are sized - along with how far into
that item's own span the position is (0 = its very first screen row\/col,
i.e. a row's ruled line). 'Nothing' if the position is before @startOffset@
entirely.
-}
indexAt :: (Int -> Int) -> Int -> Int -> Int -> Maybe (Int, Int)
indexAt sizeOf startOffset start pos
  | pos < startOffset = Nothing
  | otherwise = go startOffset start
 where
  go base i =
    let sz = sizeOf i
     in if pos < base + sz then Just (i, pos - base) else go (base + sz) (i + 1)

{- | Which sheet cell, if any, sits under a screen position, using the
viewport origin 'Render.Grid.render' laid the grid out with. 'Nothing'
means the header row, the gutter, or a ruled line.
-}
cellAt ::
  Int ->
  Map.Map Int Double ->
  Map.Map Int Double ->
  (Int, Int) ->
  Int ->
  Int ->
  Maybe (Int, Int)
cellAt cw colScales rowScales (ox, oy) x y
  | y < headerHeight = Nothing
  | x < gutterWidth = Nothing
  | otherwise =
      case
        ( indexAt (colWidthAt cw colScales) gutterWidth ox x
        , indexAt (rowHeightAt cw rowScales) headerHeight oy y
        )
        of
          (Just (col, _), Just (row, rowOffset))
            | rowOffset /= 0 -> Just (col, row)
          _ -> Nothing

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
