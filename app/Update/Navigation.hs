{- |
Module: Update.Navigation
Description: Cursor movement, zoom, and pan - the parts of sheet
navigation with no dependency on the current keymap or event.
-}
module Update.Navigation (
  termSize,
  moveTo,
  nudge,
  nudgeSelecting,
  zoomBy,
  panBy,
  panPage,
  resizeCol,
  resizeRow,
) where

import Control.Monad (when)
import qualified Data.Map.Strict as Map
import SheetState (
  SheetState (..),
  clampOrigin,
  clampRange,
  clampScale,
  maxCellWidth,
  minCellWidth,
  rowStride,
  visibleCols,
  visibleRows,
 )
import qualified Trellis.UI as UI

termSize :: UI.Action (UI.Store SheetState) IO (Int, Int)
termSize = UI.liftIO UI.screenSize

{- | Moves the cursor to an absolute cell, scrolling the viewport to keep it
visible.
-}
moveTo :: (Int, Int) -> UI.Action (UI.Store SheetState) IO ()
moveTo target = do
  st <- UI.get
  dims <- termSize
  UI.put
    st
      { cursor = target
      , viewportOrigin =
          clampOrigin
            (cellWidth st)
            (colScale st)
            (rowScale st)
            target
            dims
            (viewportOrigin st)
      }

{- | Moves the cursor by a relative offset from its current position,
collapsing any active selection - the same plain-arrow-key behavior
every spreadsheet has, whether that selection came from a mouse drag or
from 'nudgeSelecting'.
-}
nudge :: (Int, Int) -> UI.Action (UI.Store SheetState) IO ()
nudge (dx, dy) = do
  st <- UI.get
  let (cx, cy) = cursor st
  moveTo (cx + dx, cy + dy)
  UI.modify (\st' -> st'{selection = Nothing})

{- | Like 'nudge', but extends 'selection' instead of collapsing it - the
Shift+Arrow gesture. Keeps the same anchor across repeated calls as
long as the selection's endpoint still matches where the cursor was
before this move (i.e. this continues a still-in-progress
Shift+Arrow run, mouse drag, or click); otherwise (no selection yet, or
the last move was a plain, collapsing 'nudge') starts fresh, anchored
at the cursor's current position - the same "was this a fresh press or
a continuation" question 'Update.Core.clickCell' answers for a mouse
drag, just with no press\/motion distinction to lean on here.
-}
nudgeSelecting :: (Int, Int) -> UI.Action (UI.Store SheetState) IO ()
nudgeSelecting (dx, dy) = do
  st <- UI.get
  let (cx, cy) = cursor st
      anchor = case selection st of
        Just (a, e) | e == (cx, cy) -> a
        _ -> (cx, cy)
  moveTo (cx + dx, cy + dy)
  UI.modify (\st' -> st'{selection = Just (anchor, cursor st')})

{- | Adjusts the cursor's current column's 'SheetState.colScale' by
@adjust@ (e.g. @(* colScaleStep)@ to grow, @(/ colScaleStep)@ to
shrink), clamped. A column absent from the map starts at scale 1 (the
current zoom's default width), same as every other column.
-}
resizeCol :: (Double -> Double) -> UI.Action (UI.Store SheetState) IO ()
resizeCol adjust = do
  st <- UI.get
  let (cx, _) = cursor st
      newScale = clampScale (adjust (Map.findWithDefault 1 cx (colScale st)))
  UI.put st{colScale = Map.insert cx newScale (colScale st)}

-- | Like 'resizeCol', for the cursor's current row's 'SheetState.rowScale'.
resizeRow :: (Double -> Double) -> UI.Action (UI.Store SheetState) IO ()
resizeRow adjust = do
  st <- UI.get
  let (_, cy) = cursor st
      newScale = clampScale (adjust (Map.findWithDefault 1 cy (rowScale st)))
  UI.put st{rowScale = Map.insert cy newScale (rowScale st)}

-- | Adjusts zoom by @delta@ steps, clamped, re-clamping the viewport to match.
zoomBy :: Int -> UI.Action (UI.Store SheetState) IO ()
zoomBy delta = do
  st <- UI.get
  dims <- termSize
  let newWidth = clampRange minCellWidth maxCellWidth (cellWidth st + delta)
  UI.put
    st
      { cellWidth = newWidth
      , viewportOrigin =
          clampOrigin
            newWidth
            (colScale st)
            (rowScale st)
            (cursor st)
            dims
            (viewportOrigin st)
      }

{- | Shifts the viewport by one visible "page" in the given direction
(@dx@, @dy@ in units of ±1), the keyboard equivalent of the
middle-click-drag pan. The step is the number of visible rows/cols
minus one, so consecutive presses tile the sheet without gaps or
overlaps.
-}
panPage :: (Int, Int) -> UI.Action (UI.Store SheetState) IO ()
panPage (dx, dy) = do
  st <- UI.get
  dims <- termSize
  let cw = cellWidth st
      (ox, oy) = viewportOrigin st
      vc = visibleCols cw (colScale st) ox (fst dims)
      vr = visibleRows cw (rowScale st) oy (snd dims)
      stepX = (vc - 1) * dx
      stepY = (vr - 1) * dy
  UI.put
    st
      { viewportOrigin = (ox + stepX, oy + stepY)
      }

{- | Both the first press and every drag motion of the pan gesture route
through here - tells them apart itself via whether 'panAnchor' is
already set.

Deliberately still converts screen-pixel motion to a column\/row delta
using the plain zoom level ('cellWidth'\/'rowStride'), not each column\/
row's actual scaled size - unlike 'clampOrigin'\/'cellAt', which need to
be exact (a click has to land on the cell it visually looks like it
did). A drag's whole point is responsive scrolling, not pixel-precise
cell alignment, so approximating "how many columns of about-normal
width did the mouse cross" is a fine trade against the real complexity
'panBy' would need to walk variable widths between two arbitrary screen
positions instead.
-}
panBy :: Int -> Int -> UI.Action (UI.Store SheetState) IO ()
panBy screenX screenY = do
  st <- UI.get
  case panAnchor st of
    Nothing -> UI.modify (\st' -> st'{panAnchor = Just (screenX, screenY)})
    Just (ax, ay) -> do
      let cw = cellWidth st
          rs = rowStride cw
          dCols = (screenX - ax) `div` cw
          dRows = (screenY - ay) `div` rs
      when (dCols /= 0 || dRows /= 0) $
        UI.modify $ \st' ->
          let (ox, oy) = viewportOrigin st'
           in st'
                { viewportOrigin = (ox - dCols, oy - dRows)
                , panAnchor = Just (ax + dCols * cw, ay + dRows * rs)
                }
