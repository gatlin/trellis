{-# LANGUAGE FlexibleContexts #-}

{- |
Module: Update.Core
Description: The top-level event dispatcher: navigation vs. editing
mode, and the CPS-based formula-editing modal.
-}
module Update.Core (
  update,
  Interaction,
  runEditor,
  beginEdit,
  clampHelpScroll,
) where

import Control.Concurrent.STM.MonadIO (TVar)
import Control.Monad (forM_, when)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Formula (Expr)
import Keymap (KeyMap (..), matches, matchesMouse)
import Live (OutBinding, declareSubscription, parseLiveSpec)
import Parser (parseExpr)
import Render.Help (helpContent, helpInnerHeight)
import SheetFile (serialize)
import SheetState (
  ChartType (..),
  EditorState (..),
  LiveBinding (..),
  SheetState (..),
  cellAt,
  cellsInSelection,
  colScaleStep,
  doubleClickWindow,
 )
import SheetState.Geometry (clampRange, initialCellWidth)
import qualified Termbox2 as Tb2
import Trellis.CPS (CPS, lift, reset, shift)
import qualified Trellis.Orc as Orc
import qualified Trellis.UI as UI
import Update.Buffer (clampCursor, deleteAt, deleteBefore, insertAt)
import Update.Chart (toggleChart)
import Update.Events (dragging, isKey, isMouse, isShiftKey, printableChar)
import Update.Fill (commitFill, fillDragTo, keyboardFill)
import Update.Navigation (
  moveTo,
  nudge,
  nudgeSelecting,
  panBy,
  panPage,
  resizeCol,
  resizeRow,
  termSize,
  zoomBy,
 )
import Update.Subscriptions (
  cancelSubscription,
  drainLiveUpdates,
  existingText,
  publishOutUpdates,
 )

{- | A computation that may suspend across many termbox2 events - however
many keystrokes it takes to finish typing into a modal - written in
ordinary do-notation despite that. [^1]
-}
type Interaction = CPS () (UI.Action (UI.Store SheetState) IO)

{- | Open a formula editor pre-filled with 'initial', suspending the
calling 'Interaction' until it closes. [^2]
-}
runEditor :: String -> Interaction (Maybe String)
runEditor initial = shift $ \k ->
  lift
    ( UI.modify
        ( \st ->
            st{editor = Just (EditorState initial (length initial) k)}
        )
    )

{- | The direct-style payoff: reads top to bottom like an ordinary
blocking call - open an editor, get its result, act on it. Both ways of
starting an edit (confirm key, double-click) call this via 'reset'.
-}
beginEdit ::
  Orc.Group ->
  TVar [((Int, Int), Expr)] ->
  (Int, Int) ->
  String ->
  Interaction ()
beginEdit root mailbox pos initial = do
  result <- runEditor initial
  case result of
    Nothing -> return ()
    Just buf
      | null buf -> lift $ do
          cancelSubscription pos
          UI.modify (\st -> st{cells = Map.delete pos (cells st)})
      | Just spec <- parseLiveSpec buf -> lift $ do
          cancelSubscription pos
          grp <- UI.liftIO (declareSubscription root mailbox pos spec)
          UI.modify
            ( \st ->
                st
                  { subscriptions =
                      Map.insert pos (LiveBinding grp buf) (subscriptions st)
                  }
            )
      | otherwise -> case parseExpr buf of
          Right expr -> lift $ do
            cancelSubscription pos
            UI.modify (\st -> st{cells = Map.insert pos expr (cells st)})
          Left _ -> return ()

update ::
  KeyMap ->
  Orc.Group ->
  TVar [((Int, Int), Expr)] ->
  [OutBinding] ->
  Maybe FilePath ->
  UI.Event ->
  UI.Action (UI.Store SheetState) IO ()
update _ _ mailbox outs _ UI.Tick = do
  drainLiveUpdates mailbox
  publishOutUpdates outs
update keymap root mailbox _outs maybeFile (UI.InputEvent evt) = do
  st <- UI.get
  if helpModal st
    then helpMode
    else maybe navigating editing (editor st)
 where
  helpMode
    | matches (cancel keymap) evt
        || matches (helpKey keymap) evt =
        UI.modify (\st -> st{helpModal = False})
    | matches (moveUp keymap) evt = scrollHelpBy (const (-1))
    | matches (moveDown keymap) evt = scrollHelpBy (const 1)
    | matches (pageUp keymap) evt = scrollHelpBy negate
    | matches (pageDown keymap) evt = scrollHelpBy id
    | otherwise = return ()

  -- \| Moves 'helpScroll' by @stepFor visible@ lines, where @visible@ is
  -- however many 'helpContent' lines the modal is currently showing at
  -- once (@const (-1)@\/@const 1@ for a single line regardless of that,
  -- @negate@\/@id@ for a full page of it). All the actual arithmetic -
  -- applying the step and clamping the result - lives in
  -- 'clampHelpScroll', not here, so it's exactly what a test calls too.
  scrollHelpBy :: (Int -> Int) -> UI.Action (UI.Store SheetState) IO ()
  scrollHelpBy stepFor = do
    st <- UI.get
    (_, h) <- termSize
    let total = length (helpContent keymap)
        visible = helpInnerHeight h total
        offset' = clampHelpScroll total visible (helpScroll st) (stepFor visible)
    UI.put st{helpScroll = offset'}

  navigating
    -- \| Checked ahead of the plain arrow bindings below, which would
    -- otherwise also match a Shift-held arrow event and swallow it as an
    -- ordinary move - 'matches' only ever looks at the Alt bit. Always
    -- the physical arrow keys, like 'editing's cursor movement, since
    -- Shift is a modifier termbox2 only ever reports on those, not on a
    -- remapped 'moveUp' binding.
    | isShiftKey evt Tb2.keyArrowUp = nudgeSelecting (0, -1)
    | isShiftKey evt Tb2.keyArrowDown = nudgeSelecting (0, 1)
    | isShiftKey evt Tb2.keyArrowLeft = nudgeSelecting (-1, 0)
    | isShiftKey evt Tb2.keyArrowRight = nudgeSelecting (1, 0)
    | matches (moveUp keymap) evt = nudge (0, -1)
    | matches (moveDown keymap) evt = nudge (0, 1)
    | matches (moveLeft keymap) evt = nudge (-1, 0)
    | matches (moveRight keymap) evt = nudge (1, 0)
    -- | Tab moves right, Shift+Tab moves left - the traditional
    -- "next/previous cell" gesture. Shift+Tab is hard-coded (like
    -- Shift+Arrow above) since termbox2 only reports Shift reliably
    -- on physical keys, not remapped bindings. It isn't a shift-modified
    -- Tab event at all - termbox2 reports it as its own distinct key,
    -- keyBackTab, with no modifier bit set (confirmed live via debug
    -- trace); checking 'isShiftKey' against keyCtrlTab could never
    -- match a real keypress.
    | isKey evt Tb2.keyBackTab = nudge (-1, 0)
    | matches (tabKey keymap) evt = nudge (1, 0)
    | matchesMouse (scrollUp keymap) evt = zoomBy 1
    | matchesMouse (scrollDown keymap) evt = zoomBy (-1)
    -- \| Keyboard zoom: the same three steps the scroll wheel gives,
    -- plus a reset to the initial (unzoomed) width.
    | matches (zoomInKey keymap) evt = zoomBy 1
    | matches (zoomOutKey keymap) evt = zoomBy (-1)
    | matches (zoomResetKey keymap) evt =
        UI.modify (\st -> st{cellWidth = initialCellWidth})
    -- \| Keyboard pan: shifts the viewport by one visible page, the
    -- same gesture as middle-click-drag but without a mouse.
    | matches (pageUp keymap) evt = panPage (0, -1)
    | matches (pageDown keymap) evt = panPage (0, 1)
    | matches (panUp keymap) evt = panPage (0, -1)
    | matches (panDown keymap) evt = panPage (0, 1)
    | matches (panLeft keymap) evt = panPage (-1, 0)
    | matches (panRight keymap) evt = panPage (1, 0)
    -- \| Resize the cursor's current column\/row, relative to the
    -- current zoom - see 'Update.Navigation.resizeCol'\/'resizeRow'.
    | matches (growColKey keymap) evt = resizeCol (* colScaleStep)
    | matches (shrinkColKey keymap) evt = resizeCol (/ colScaleStep)
    | matches (growRowKey keymap) evt = resizeRow (* colScaleStep)
    | matches (shrinkRowKey keymap) evt = resizeRow (/ colScaleStep)
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
    -- \| Same self-distinguishing shape as 'panBy', for the fill gesture.
    | matchesMouse (fillButton keymap) evt =
        fillDragTo (fromIntegral (Tb2._x evt)) (fromIntegral (Tb2._y evt))
    -- \| Keyboard fill: replicate the current selection's source across
    -- the selection, the keyboard equivalent of right-click-drag.
    | matches (fillKey keymap) evt = keyboardFill
    | matches (fillKeyAlt keymap) evt = keyboardFill
    -- \| Release isn't bound to anything, but still commits any
    -- in-progress fill and clears both drag anchors. [^3]
    | isMouse evt Tb2.keyMouseRelease = do
        commitFill
        UI.modify (\st -> st{panAnchor = Nothing, fillDrag = Nothing})
    | matches (confirm keymap) evt = beginEditHere
    | Tb2._type evt == Tb2.eventKey
        && ( Tb2._key evt == Tb2.keyCtrlEnter
             || Tb2._ch evt == 13
             || Tb2._ch evt == 10
           ) =
        beginEditHere
    | matches (editKey keymap) evt = beginEditHere
    | matches (clearCell keymap) evt = clearFocusedCell
    | matches (barChartKey keymap) evt = toggleChart BarChart
    | matches (lineChartKey keymap) evt = toggleChart LineChart
    | matches (helpKey keymap) evt =
        UI.modify (\st -> st{helpModal = True, helpScroll = 0})
    | matches (heatmapKey keymap) evt = toggleChart Heatmap
    | matches (saveKey keymap) evt = saveSheet
    -- \| Otherwise inert while navigating (only 'editing' uses it to
    -- discard an edit) - repurposed here as the obvious way to back out
    -- of a selection, or an open chart, that's no longer wanted.
    | matches (cancel keymap) evt =
        UI.modify (\st -> st{selection = Nothing, chart = Nothing})
    | otherwise = return ()

  -- \| While a formula is being typed, navigation is suspended entirely -
  -- only editing the buffer or leaving it (committed or cancelled) does
  -- anything, so a stray arrow key can't quietly abandon an edit.
  editing est
    | matches (confirm keymap) evt
        || matches (editKey keymap) evt
        || ( Tb2._type evt == Tb2.eventKey
             && ( Tb2._ch evt == 13
                  || Tb2._ch evt == 10
                  || Tb2._key evt == Tb2.keyCtrlM
                )
           ) =
        closeEditor est (Just (editorBuffer est))
    | matches (cancel keymap) evt = closeEditor est Nothing
    -- \| Cursor movement is always the physical arrow keys\/Home\/End,
    -- never routed through the keymap - unlike sheet navigation, so it
    -- can't be broken by a remap like @moveLeft = h@.
    | isKey evt Tb2.keyArrowLeft = moveCursor (\p b -> clampCursor (p - 1) b)
    | isKey evt Tb2.keyArrowRight = moveCursor (\p b -> clampCursor (p + 1) b)
    | isKey evt Tb2.keyHome = moveCursor (\_ _ -> 0)
    | isKey evt Tb2.keyEnd = moveCursor (\_ b -> length b)
    | isKey evt Tb2.keyCtrlA = moveCursor (\_ _ -> 0)
    | isKey evt Tb2.keyCtrlE = moveCursor (\_ b -> length b)
    | isKey evt Tb2.keyDelete = editBuffer deleteAt
    -- \| Both wire encodings of "backspace" are accepted unconditionally -
    -- which one a terminal sends isn't a preference to configure, it's a
    -- compatibility fact about that terminal.
    | isKey evt Tb2.keyBackspace || isKey evt Tb2.keyBackspace2 =
        editBuffer deleteBefore
    | Just c <- printableChar evt = editBuffer (insertAt c)
    | otherwise = return ()

  beginEditHere = do
    st <- UI.get
    let pos = cursor st
        existing = existingText st pos
    reset (beginEdit root mailbox pos existing)

  moveCursor f =
    UI.modify
      ( \st ->
          st
            { editor =
                fmap
                  ( \p ->
                      p{editorCursor = f (editorCursor p) (editorBuffer p)}
                  )
                  (editor st)
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

  clearFocusedCell = do
    st <- UI.get
    let targets = cellsInSelection (cursor st) (selection st)
    forM_ targets cancelSubscription
    UI.modify
      ( \st' ->
          st'
            { cells = foldr Map.delete (cells st') targets
            , selection = Nothing
            }
      )

  -- \| Writes the sheet back to the file it was loaded from - a no-op
  -- if Trellis wasn't started with one, since there's no "save as" yet.
  saveSheet = case maybeFile of
    Nothing -> return ()
    Just path -> do
      st <- UI.get
      UI.liftIO (writeFile path (serialize st))

  -- \| Extends the in-progress selection's endpoint, keeping its anchor
  -- fixed - same rectangle 'selectButton' has been walking the cursor
  -- across all along, now also remembered for 'fillButton' to read.
  clickCell = do
    st <- UI.get
    forM_
      ( cellAt
          (cellWidth st)
          (colScale st)
          (rowScale st)
          (viewportOrigin st)
          (fromIntegral (Tb2._x evt))
          (fromIntegral (Tb2._y evt))
      )
      $ \target -> do
        moveTo target
        UI.modify
          ( \st' ->
              st'{selection = Just (maybe target fst (selection st'), target)}
          )

  pressCell = do
    st <- UI.get
    case cellAt
      (cellWidth st)
      (colScale st)
      (rowScale st)
      (viewportOrigin st)
      (fromIntegral (Tb2._x evt))
      (fromIntegral (Tb2._y evt)) of
      Nothing -> UI.modify (\st -> st{selection = Nothing, chart = Nothing})
      Just target -> do
        now <- UI.liftIO getCurrentTime
        let isDouble = case lastClick st of
              Just (prevTarget, prevTime) ->
                prevTarget == target
                  && diffUTCTime now prevTime < doubleClickWindow
              Nothing -> False
        moveTo target
        UI.modify
          ( \st' ->
              st'
                { lastClick = Just (target, now)
                , selection = Just (target, target)
                }
          )
        when isDouble (openForEditing target)

  openForEditing target = do
    st <- UI.get
    let existing = existingText st target
    reset (beginEdit root mailbox target existing)

{- | Applies a scroll step (@delta@ lines) to a @current@ 'helpScroll'
offset and clamps the result into the valid range for the given content
length and visible line count - never below 0, never past the point
where the last line is already on screen. This is *all* of
'scrollHelpBy's arithmetic, factored out specifically so a test can call
the same function production code calls, rather than a hand-copied
reimplementation of it (which caught nothing - see the git history of
"test/RenderHelpSpec.hs" for a demonstrated example of exactly that
going wrong). Built on 'SheetState.Geometry.clampRange', the same clamp
primitive the grid's own viewport uses.
-}
clampHelpScroll :: Int -> Int -> Int -> Int -> Int
clampHelpScroll total visible current delta =
  clampRange 0 (max 0 (total - visible)) (current + delta)

{- [^1]:
The answer type is fixed to @()@ throughout, matching the fact that every
'UI.Action' produced anywhere in this module already is one - that's what
makes 'shift'\/'reset' storable here at all.
-}

{- [^2]:
'shift' hands us the rest of the enclosing computation as an ordinary
callback @k@; rather than run it now (there's nothing to run it with yet -
the user hasn't typed anything), it's stashed in a fresh 'EditorState' for
'editing' to call once editing actually finishes.
-}

{- [^3]:
termbox2 doesn't say which button was released, so whatever
'selectButton'\/'panButton'\/'fillButton' is set to can't tell us this
release belongs to it in particular. Committing and clearing unconditionally
means the next press of either drag button starts fresh rather than
resuming or re-triggering something from whatever drag last touched it.
-}
