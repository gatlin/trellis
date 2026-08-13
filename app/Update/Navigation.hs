{- |
Module: Update.Navigation
Description: Cursor movement, zoom, and pan - the parts of sheet
navigation with no dependency on the current keymap or event.
-}
module Update.Navigation (
  termSize,
  moveTo,
  nudge,
  zoomBy,
  panBy,
) where

import Control.Monad (when)
import SheetState (
  SheetState (..),
  clampOrigin,
  clampRange,
  maxCellWidth,
  minCellWidth,
  rowStride,
 )
import qualified Termbox2 as Tb2
import qualified Trellis.UI as UI

termSize :: UI.Action (UI.Store SheetState) IO (Int, Int)
termSize = UI.liftIO (Tb2.runTermbox2 ((,) <$> Tb2.width <*> Tb2.height))

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
          clampOrigin (cellWidth st) target dims (viewportOrigin st)
      }

-- | Moves the cursor by a relative offset from its current position.
nudge :: (Int, Int) -> UI.Action (UI.Store SheetState) IO ()
nudge (dx, dy) = do
  st <- UI.get
  let (cx, cy) = cursor st
  moveTo (cx + dx, cy + dy)

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
          clampOrigin newWidth (cursor st) dims (viewportOrigin st)
      }

{- | Both the first press and every drag motion of the pan gesture route
through here - tells them apart itself via whether 'panAnchor' is
already set.
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
