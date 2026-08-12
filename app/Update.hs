{-# LANGUAGE FlexibleContexts #-}

{- |
Module: Update
Description: Event handling: navigation, zoom, pan, and the CPS-based
formula-editing modal.
-}
module Update (
  update,
  isKey,
  isMouse,
  dragging,
  printableChar,
  isValid,
  insertAt,
  deleteBefore,
  deleteAt,
  clampCursor,
) where

import Control.Monad (forM_, when)
import Data.Bits ((.&.))
import qualified Data.Map.Strict as Map
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Formula (blank, renderExpr)
import Parser (parseExpr)
import SheetState (
  EditorState (..),
  SheetState (..),
  cellAt,
  clampOrigin,
  clampRange,
  doubleClickWindow,
  maxCellWidth,
  minCellWidth,
  rowStride,
 )
import qualified Termbox2 as Tb2
import Keymap (KeyMap (..), matches, matchesMouse)
import Trellis.CPS (CPS, lift, reset, shift)
import qualified Trellis.UI as UI

termSize :: UI.Action (UI.Store SheetState) IO (Int, Int)
termSize = UI.liftIO (Tb2.runTermbox2 ((,) <$> Tb2.width <*> Tb2.height))

{- | A computation that may suspend across many termbox2 events - however
many keystrokes it takes to finish typing into a modal - written in
ordinary do-notation despite that. [^1]
-}
type Editor = CPS () (UI.Action (UI.Store SheetState) IO)

{- | Open a formula editor pre-filled with 'initial', suspending the
calling 'Editor' computation until it closes. [^2]
-}
runEditor :: String -> Editor (Maybe String)
runEditor initial = shift $ \k ->
  lift
    (UI.modify (\st -> st{editor = Just (EditorState initial (length initial) k)}))

{- | The direct-style payoff: reads top to bottom like an ordinary
blocking call - open an editor, get its result, act on it. Both ways of
starting an edit (confirm key, double-click) call this via 'reset'.
-}
beginEdit :: (Int, Int) -> String -> Editor ()
beginEdit pos initial = do
  result <- runEditor initial
  case result of
    Nothing -> return ()
    Just buf
      | null buf -> lift . UI.modify $ \st ->
          st{cells = Map.delete pos (cells st)}
      | otherwise -> case parseExpr buf of
          Right expr -> lift . UI.modify $ \st ->
            st{cells = Map.insert pos expr (cells st)}
          Left _ -> return ()

-- | Does an incoming event carry a given key, of the given event type?
isKey :: Tb2.Tb2Event -> Tb2.Tb2Key -> Bool
isKey evt k = Tb2._type evt == Tb2.eventKey && Tb2._key evt == k

isMouse :: Tb2.Tb2Event -> Tb2.Tb2Key -> Bool
isMouse evt k = Tb2._type evt == Tb2.eventMouse && Tb2._key evt == k

-- | Is this mouse event part of an in-progress drag?
dragging :: Tb2.Tb2Event -> Bool
dragging evt = Tb2._mod evt .&. Tb2.modMotion /= 0

{- | A key event carrying an ordinary printable character rather than a
named key: termbox2 reports these with no key code, just a Unicode
codepoint. Not a binding - a typed character always means itself.
-}
printableChar :: Tb2.Tb2Event -> Maybe Char
printableChar evt
  | Tb2._type evt == Tb2.eventKey
  , Tb2._key evt == 0
  , Tb2._ch evt /= 0 =
      Just (toEnum (fromIntegral (Tb2._ch evt)) :: Char)
  | otherwise = Nothing

-- | Does a formula editor's buffer currently parse?
isValid :: String -> Bool
isValid buf = case parseExpr buf of
  Right _ -> True
  Left _ -> False

-- | Keep a cursor position within the bounds of a buffer.
clampCursor :: Int -> String -> Int
clampCursor c s = max 0 (min (length s) c)

{- | Insert a character at a cursor position, returning the new buffer and
the cursor position just after the inserted character.
-}
insertAt :: Char -> Int -> String -> (String, Int)
insertAt c pos s =
  let (before, after) = splitAt pos s
   in (before ++ [c] ++ after, pos + 1)

{- | Delete the character just before a cursor position (backspace),
returning the new buffer and cursor. A no-op at position 0.
-}
deleteBefore :: Int -> String -> (String, Int)
deleteBefore pos s
  | pos <= 0 = (s, pos)
  | otherwise =
      let (before, after) = splitAt pos s
       in (init before ++ after, pos - 1)

{- | Delete the character just after a cursor position (forward-delete).
The cursor itself doesn't move; a no-op at the end of the buffer.
-}
deleteAt :: Int -> String -> (String, Int)
deleteAt pos s
  | pos >= length s = (s, pos)
  | otherwise =
      let (before, after) = splitAt pos s
       in (before ++ drop 1 after, pos)

update :: KeyMap -> UI.Event -> UI.Action (UI.Store SheetState) IO ()
update keymap (UI.Event evt) = do
  st <- UI.get
  maybe navigating editorUpdate (editor st)
 where
  navigating
    | matches (moveUp keymap) evt = nudge (0, -1)
    | matches (moveDown keymap) evt = nudge (0, 1)
    | matches (moveLeft keymap) evt = nudge (-1, 0)
    | matches (moveRight keymap) evt = nudge (1, 0)
    | matchesMouse (scrollUp keymap) evt = zoomBy 1
    | matchesMouse (scrollDown keymap) evt = zoomBy (-1)
    -- \| A fresh press of the select button: also checks for a double-click
    -- within 'doubleClickWindow', opening the cell for editing instead of
    -- just selecting it.
    | matchesMouse (selectButton keymap) evt && not (dragging evt) = pressCell
    -- \| Dragging: just follow the mouse, no double-click logic.
    | matchesMouse (selectButton keymap) evt && dragging evt = clickCell
    -- \| Both the first press and every drag motion of the pan button
    -- route through 'panBy' - it tells them apart itself via whether
    -- 'panAnchor' is already set.
    | matchesMouse (panButton keymap) evt =
        panBy (fromIntegral (Tb2._x evt)) (fromIntegral (Tb2._y evt))
    -- \| Release isn't bound to anything, but still clears 'panAnchor'. [^3]
    | isMouse evt Tb2.keyMouseRelease = UI.modify (\st -> st{panAnchor = Nothing})
    | matches (confirm keymap) evt = beginEditHere
    | matches (clearCell keymap) evt = clearFocusedCell
    | otherwise = return ()

  beginEditHere = do
    st <- UI.get
    let pos = cursor st
        existing = renderExpr (Map.findWithDefault blank pos (cells st))
    reset (beginEdit pos existing)

  -- \| While a formula is being typed, navigation is suspended entirely -
  -- only editing the buffer or leaving it (committed or cancelled) does
  -- anything, so a stray arrow key can't quietly abandon an edit.
  editorUpdate m
    | matches (confirm keymap) evt =
        when (null (editorBuffer m) || isValid (editorBuffer m)) $
          closeEditor m (Just (editorBuffer m)) -- invalid, non-empty: stay open to fix it
    | matches (cancel keymap) evt = closeEditor m Nothing
    -- \| Cursor movement is always the physical arrow keys\/Home\/End,
    -- never routed through the keymap - unlike sheet navigation, so it
    -- can't be broken by a remap like @moveLeft = h@.
    | isKey evt Tb2.keyArrowLeft = moveCursor (\p b -> clampCursor (p - 1) b)
    | isKey evt Tb2.keyArrowRight = moveCursor (\p b -> clampCursor (p + 1) b)
    | isKey evt Tb2.keyHome = moveCursor (\_ _ -> 0)
    | isKey evt Tb2.keyEnd = moveCursor (\_ b -> length b)
    | isKey evt Tb2.keyDelete = editBuffer deleteAt
    -- \| Both wire encodings of "backspace" are accepted unconditionally -
    -- which one a terminal sends isn't a preference to configure, it's a
    -- compatibility fact about that terminal.
    | isKey evt Tb2.keyBackspace || isKey evt Tb2.keyBackspace2 =
        editBuffer deleteBefore
    | Just c <- printableChar evt = editBuffer (insertAt c)
    | otherwise = return ()

  moveCursor f =
    UI.modify
      ( \st ->
          st
            { editor =
                fmap (\p -> p{editorCursor = f (editorCursor p) (editorBuffer p)}) (editor st)
            }
      )

  -- \| Runs a buffer-editing function ('insertAt'\/'deleteBefore'\/
  -- 'deleteAt', partially applied to a common shape) against the open
  -- editor, threading its returned cursor back in.
  editBuffer f =
    UI.modify
      ( \st ->
          st
            { editor =
                fmap
                  ( \p ->
                      let (b, c) = f (editorCursor p) (editorBuffer p)
                       in p{editorBuffer = b, editorCursor = c}
                  )
                  (editor st)
            }
      )

  closeEditor m result = do
    UI.modify (\st -> st{editor = Nothing})
    editorResume m result

  clearFocusedCell =
    UI.modify (\st -> st{cells = Map.delete (cursor st) (cells st)})

  clickCell = do
    st <- UI.get
    forM_
      ( cellAt
          (cellWidth st)
          (viewportOrigin st)
          (fromIntegral (Tb2._x evt))
          (fromIntegral (Tb2._y evt))
      )
      moveTo

  pressCell = do
    st <- UI.get
    case cellAt
      (cellWidth st)
      (viewportOrigin st)
      (fromIntegral (Tb2._x evt))
      (fromIntegral (Tb2._y evt)) of
      Nothing -> return ()
      Just target -> do
        now <- UI.liftIO getCurrentTime
        let isDouble = case lastClick st of
              Just (prevTarget, prevTime) ->
                prevTarget == target && diffUTCTime now prevTime < doubleClickWindow
              Nothing -> False
        moveTo target
        UI.modify (\st' -> st'{lastClick = Just (target, now)})
        when isDouble (openForEditing target)

  openForEditing target = do
    st <- UI.get
    let existing = renderExpr (Map.findWithDefault blank target (cells st))
    reset (beginEdit target existing)
  nudge (dx, dy) = do
    st <- UI.get
    let (cx, cy) = cursor st
    moveTo (cx + dx, cy + dy)
  moveTo target = do
    st <- UI.get
    dims <- termSize
    UI.put
      st
        { cursor = target
        , viewportOrigin = clampOrigin (cellWidth st) target dims (viewportOrigin st)
        }

  zoomBy delta = do
    st <- UI.get
    dims <- termSize
    let newWidth = clampRange minCellWidth maxCellWidth (cellWidth st + delta)
    UI.put
      st
        { cellWidth = newWidth
        , viewportOrigin = clampOrigin newWidth (cursor st) dims (viewportOrigin st)
        }

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

{- [^1]:
The answer type is fixed to @()@ throughout, matching the fact that every
'UI.Action' produced anywhere in this module already is one - that's what
makes 'shift'\/'reset' storable here at all.
-}

{- [^2]:
'shift' hands us the rest of the enclosing computation as an ordinary
callback @k@; rather than run it now (there's nothing to run it with yet -
the user hasn't typed anything), it's stashed in a fresh 'EditorState' for
'editorUpdate' to call once editing actually finishes.
-}

{- [^3]:
termbox2 doesn't say which button was released, so whatever
'selectButton'\/'panButton' is set to can't tell us this release belongs to
it in particular. Clearing 'panAnchor' unconditionally means the next
press of the pan button starts a fresh anchor rather than resuming a stale
one from whatever drag last touched it.
-}
