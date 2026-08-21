{- |
Module: Trellis.UI
Description: The one seam between the spreadsheet app and whatever
renders it. Re-exports 'Trellis.UI.Backend's event\/key\/color vocabulary
and 'Trellis.UI.Screen's drawing\/event-loop surface (swapped per target -
termbox2 natively, canvas\/DOM under wasm - via @hs-source-dirs@, same as
'Trellis.Orc' already is), plus the backend-agnostic comonadic component
machinery every target shares unchanged.
-}
module Trellis.UI (
  -- Backend vocabulary
  Backend.Key,
  Backend.keyNone,
  Backend.keyArrowUp,
  Backend.keyArrowDown,
  Backend.keyArrowLeft,
  Backend.keyArrowRight,
  Backend.keyCtrlA,
  Backend.keyCtrlB,
  Backend.keyCtrlC,
  Backend.keyCtrlD,
  Backend.keyCtrlE,
  Backend.keyCtrlF,
  Backend.keyCtrlG,
  Backend.keyCtrlH,
  Backend.keyCtrlI,
  Backend.keyCtrlJ,
  Backend.keyCtrlK,
  Backend.keyCtrlL,
  Backend.keyCtrlM,
  Backend.keyCtrlN,
  Backend.keyCtrlO,
  Backend.keyCtrlP,
  Backend.keyCtrlQ,
  Backend.keyCtrlR,
  Backend.keyCtrlS,
  Backend.keyCtrlT,
  Backend.keyCtrlU,
  Backend.keyCtrlV,
  Backend.keyCtrlW,
  Backend.keyCtrlX,
  Backend.keyCtrlY,
  Backend.keyCtrlZ,
  Backend.keyCtrlEnter,
  Backend.keyCtrlEsc,
  Backend.keyCtrlTab,
  Backend.keyBackTab,
  Backend.keyDelete,
  Backend.keyHome,
  Backend.keyEnd,
  Backend.keyPgUp,
  Backend.keyPgDn,
  Backend.keyF2,
  Backend.keySpace,
  Backend.keyBackspace,
  Backend.keyBackspace2,
  Backend.keyMouseLeft,
  Backend.keyMouseRight,
  Backend.keyMouseMiddle,
  Backend.keyMouseRelease,
  Backend.keyMouseWheelUp,
  Backend.keyMouseWheelDown,
  Backend.Modifiers,
  Backend.modAlt,
  Backend.modCtrl,
  Backend.modShift,
  Backend.modMotion,
  Backend.EventType,
  Backend.eventKey,
  Backend.eventMouse,
  Backend.InputEvent (..),
  Backend.Color,
  Backend.colorBlack,
  Backend.colorGreen,
  Backend.colorMagenta,
  Backend.colorWhite,
  Backend.colorDefault,
  Backend.attrBold,
  Backend.attrUnderline,
  Backend.OutputMode,
  Backend.output256,
  Backend.InputMode,
  Backend.inputEsc,
  Backend.inputMouse,
  -- Screen
  Screen.Screen,
  Screen.Event (..),
  Screen.Console (..),
  Screen.console,
  Screen.Activity,
  Screen.activity,
  Screen.mount,
  Screen.setInputMode,
  Screen.setOutputMode,
  Screen.glyphCode,
  Screen.blockGlyph,
  Screen.drawBlock,
  Screen.drawText,
  Screen.drawGlyph,
  Screen.drawVLine,
  Screen.drawHLine,
  Screen.drawRect,
  Screen.screenBorder,
  Screen.centerText,
  Screen.footerText,
  Screen.statusText,
  Screen.width,
  Screen.height,
  Screen.screenSize,
  -- Comonadic component machinery
  Action (..),
  BehaviorOf,
  behavior,
  Control.Comonad.Cofree.unwrap,
  modify,
  put,
  get,
  Handler,
  Interface,
  Component,
  move,
  hoist,
  withBorder,
  -- Composing lists of homogeneous children
  ListSpace,
  list,
  tapeFromList,
  -- Re-exports for convenience
  Control.Comonad.Store.Store,
  Control.Comonad.Store.store,
  Control.Comonad.Store.runStore,
  Control.Monad.IO.Class.liftIO,
  Trellis.Sheet.Tape (..),
  Trellis.Sheet.moveL,
  Trellis.Sheet.moveR,
) where

import Control.Comonad (Comonad (..), ComonadApply (..), (=>>))
import Control.Comonad.Cofree (ComonadCofree (unwrap))
import Control.Comonad.Store (
  Store,
  StoreT (..),
  runStore,
  runStoreT,
  store,
 )
import qualified Control.Monad.IO.Class
import Trellis.Sheet (Stream (..), Tape (..), enumerate, moveL, moveR)
import qualified Trellis.UI.Backend as Backend
import Trellis.UI.Core
import qualified Trellis.UI.Screen as Screen

-- = Composing a dynamic list of homogeneous children.

{- | A focus-navigable list of independently-stateful 'Screen.Activity's
sharing one 'space'. Built on 'Tape' so 'moveL'\/'moveR' mean "focus
prev\/next" for free; only the focused child renders or receives events.
[^3]
-}
data ListSpace space a = ListSpace
  { listBounds :: (Int, Int)
  {- ^ (steps remaining to the left, steps remaining to the right) of the
  currently focused child before hitting the real ends of the list.
  -}
  , listFocus :: Tape (space a)
  }

instance (Functor space) => Functor (ListSpace space) where
  fmap f (ListSpace b t) = ListSpace b (fmap (fmap f) t)

instance (Comonad space) => Comonad (ListSpace space) where
  extract (ListSpace _ t) = extract (focus t)
  duplicate (ListSpace (l, r) t) =
    ListSpace (l, r) (fmap rebuild offsets <@> duplicate t)
   where
    {- \| Only the focused child moves; every other child at this tape
    position is carried over untouched (a zipper of comonads, not a full
    product of positions). [^4]
    -}
    offsets = enumerate (0 :: Int)
    rebuild off here =
      fmap
        (\st -> ListSpace (l + off, r - off) (setFocus st here))
        (duplicate (focus here))
    setFocus st (Tape ls _ rs) = Tape ls st rs

{- | Build a list 'Screen.Activity' from a starting child and the rest of a
finite list. First predicate moves focus left, second right (clamped to
real ends); anything else goes to the focused child's own update.
-}
list ::
  (Comonad space) =>
  (Screen.Event -> Bool) ->
  (Screen.Event -> Bool) ->
  Screen.Activity space effect ->
  [Screen.Activity space effect] ->
  Screen.Activity (ListSpace space) effect
list isPrev isNext x0 rest =
  ListSpace (0, length rest) (fmap wireChild (tapeFromList x0 rest))
 where
  wireChild child = child =>> renderChild

  renderChild childHere send =
    let ~(Screen.Console rC uC) = extract childHere $ send . hoist adapt
        handle event
          | isPrev event = send focusPrev
          | isNext event = send focusNext
          | otherwise = uC event
     in Screen.Console rC handle

  adapt = focus . listFocus

  focusPrev = Action $ \(ListSpace (l, _) conts) ->
    if l <= 0
      then extract (focus conts) ()
      else extract (focus (moveL conts)) ()

  focusNext = Action $ \(ListSpace (_, r) conts) ->
    if r <= 0
      then extract (focus conts) ()
      else extract (focus (moveR conts)) ()

{- | Build a 'Tape' from a starting element and a finite list, clamped at
both ends: moving past the first or last element just keeps yielding it,
rather than needing separate filler for 'Tape's infinite tails.
-}
tapeFromList :: a -> [a] -> Tape a
tapeFromList x0 rest = Tape (repeatT x0) x0 (go rest)
 where
  repeatT x = Cons x (repeatT x)
  go [] = repeatT x0
  go [y] = repeatT y
  go (y : ys) = Cons y (go ys)

-- | Wrap another Activity with a tasteful border.
withBorder ::
  (Comonad space) =>
  Screen.Activity space effect ->
  Screen.Activity (StoreT () space) effect
withBorder inner = StoreT (inner =>> render) ()
 where
  render child () send =
    let ~(Screen.Console rC uC) = extract child $ send . hoist adapt
     in Screen.Console (rC >> Screen.screenBorder 0) uC

  adapt wrapped = let (idx, k) = runStoreT wrapped in ($ k) <$> idx
  {-# INLINE adapt #-}

{- [^3]:
'Tape' pads infinitely in both directions, and that padding can never tell
"still real content" from "off the end" of the actual list. 'listBounds'
tracks the remaining real steps in each direction alongside the 'Tape' so
navigation can be clamped to the finite list instead of wandering into the
filler.
-}

{- [^4]:
'offsets' is zero at the current focus, negative to the left, positive to
the right; it lets each position bake in its own correctly-adjusted
'listBounds' ahead of time, because 'focusPrev'/'focusNext' can only
\*select* one of these already-built continuations - never inspect or
amend the value one produces afterwards, per the CPS encoding 'Action'
uses.
-}
