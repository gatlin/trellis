{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Trellis.UI (
  -- UI
  Screen,
  Action (..),
  mount,
  BehaviorOf,
  behavior,
  Control.Comonad.Cofree.unwrap,
  Activity,
  activity,
  Console (..),
  console,
  modify,
  put,
  get,
  Event (..),
  -- Drawing utilities
  glyphCode,
  blockGlyph,
  drawBlock,
  drawText,
  drawGlyph,
  drawVLine,
  drawHLine,
  drawRect,
  screenBorder,
  centerText,
  footerText,
  statusText,
  width,
  height,
  withBorder,
  -- Composing lists of homogeneous children
  ListSpace,
  list,
  tapeFromList,
  -- Remainder
  Handler,
  Interface,
  Component,
  move,
  hoist,
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
import Control.Comonad.Cofree (Cofree, ComonadCofree (unwrap), coiter)
import Control.Comonad.Store (
  ComonadStore (..),
  Store,
  StoreT (..),
  runStore,
  runStoreT,
  store,
 )
import Control.Concurrent.MonadIO (MVar, newMVar, putMVar, takeMVar)
import Control.Exception (bracket_)
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Char (ord)
import qualified Termbox2 as Tb2
import Trellis.Sheet (Stream (..), Tape (..), enumerate, moveL, moveR)
import Trellis.Tubes (
  Generator,
  Reducer,
  Tube,
  await,
  deliver,
  embed,
  finish,
  yield,
  (><),
 )

-- = Part 1: Actions, spaces, and components - oh my!

-- | Handles some action parameterized by and resulting in a type of effect.
type Handler effect action = action effect () -> effect ()

-- | With an action 'Handler' we may construct and react to some type of @view@.
type Interface effect action view = Handler effect action -> view

-- | A space of 'Interface's which may be composed in various useful ways.
type Component effect space action view = space (Interface effect action view)

{- | Represents some action performed with or on a given component @space@.
These actions have side effects in a base monad.
-}
newtype Action space effect a = Action
  { perform :: forall r. space (a -> effect r) -> effect r
  }
  deriving (Functor)

instance (Comonad space) => Applicative (Action space effect) where
  pure !a = Action (`extract` a)
  mf <*> ma = mf >>= \f -> fmap f ma

instance (Comonad space) => Monad (Action space effect) where
  Action k >>= f =
    Action $
      k
        . extend
          ( \wa !a ->
              let !(Action fa) = f a
               in fa wa
          )

instance (Comonad space) => MonadTrans (Action space) where
  lift m = Action (extract . fmap (m >>=))

instance (Comonad space, MonadIO effect) => MonadIO (Action space effect) where
  liftIO = lift . liftIO

-- | Carries out an 'Action' in a space yielding a result with side effects.
move ::
  (Functor space) =>
  (a -> b -> effect r) ->
  Action space effect a ->
  space b ->
  effect r
move f (Action a) !s = a $! fmap (flip f) s

-- | Hoist an 'Action' for one space into a different space contravariantly.
hoist ::
  (forall x. w x -> v x) ->
  Action v effect a ->
  Action w effect a
hoist transform (Action action) = Action $ action . transform

{- | 'Action' for components built from a 'ComonadStore': modifies state.
Uses 'pos'\/'seek' rather than 'seeks' to force the new index. [^1]
-}
modify :: (ComonadStore state w) => (state -> state) -> Action w effect ()
modify fn = Action $ \(!st) ->
  let !newIndex = fn (pos st)
      !st' = seek newIndex st
      !v = extract st' ()
   in v

-- | 'Action' for components built from a 'ComonadStore': overwrites state.
put :: (ComonadStore state w) => state -> Action w effect ()
put !x = Action $ \st -> extract (seek x st) ()

-- | 'Action' for components built from a 'ComonadStore': loads state.
get :: (ComonadStore state w) => Action w effect state
get = Action $ \st -> extract st (pos st)

-- | Defines a space with the behavior of a given base functor.
type BehaviorOf = Cofree

-- | Constructs a space with the behavior of a given base functor.
behavior :: (Functor f) => (a -> f a) -> a -> BehaviorOf f a
behavior = coiter

-- = Part 2: Components in the terminal console.

{- | DSL based on 'Tb2.Termbox2' for UI drawing operations. Deliberately
does not derive 'MonadIO' or export the constructor.
-}
newtype Screen a = Screen (Tb2.Termbox2 a)
  deriving (Functor, Applicative, Monad)

{- | Either a real termbox2 input event, or a periodic wakeup carrying none -
the hook a component uses to notice state that changed off-screen (e.g. a
'Trellis.Orc' subscription's result) without any input at all. [^5]
-}
data Event = InputEvent Tb2.Tb2Event | Tick
  deriving (Show, Eq)

{- | How often 'events' wakes the loop with a 'Tick' when no real input
arrives. Fast enough that an off-screen update feels immediate; cheap
enough to matter little at idle, since termbox2's own 'Tb2.present' only
redraws cells that actually changed.
-}
tickIntervalMs :: Int
tickIntervalMs = 250

-- | A console view.
data Console effect
  = Console
      -- | Renders output when called.
      (Screen ())
      -- | Awaits incoming events.
      (Event -> effect ())

console :: (Event -> t) -> Screen () -> (t -> effect ()) -> Console effect
console update render send = Console render (send . update)

{- | A 'Component' specialized to 'Action' as its action type and
'Console' as its view - the concrete shape 'mount' runs.
-}
type Activity space effect =
  Component effect space (Action space) (Console effect)

activity ::
  (Event -> Action (Store s) effect ()) ->
  (s -> Screen ()) ->
  s ->
  Activity (Store s) effect
activity u r = store (console u . r)

events :: Generator Tb2.Termbox2 Event
events = forever $ do
  !event <- embed (Tb2.peekEvent tickIntervalMs)
  case event of
    Nothing -> yield Tick
    Just !event' -> yield (InputEvent event')

{- | Ctrl+Q is a global escape hatch: it short-circuits here, before any
'Activity's own 'update' logic, so no component can swallow it - a queued
'Tick' never delays it. [^2]
-}
loopOrQuit :: Tube Tb2.Termbox2 Event Event
loopOrQuit = forever $ do
  event <- await
  case event of
    InputEvent evt | Tb2._key evt == Tb2.keyCtrlQ -> finish
    _ -> yield event

{- | An implicit parameter's type must be monomorphic, so a rank-2 "run this
effect down to IO" function has to be smuggled through one in a newtype.
-}
newtype Runner effect = Runner {runWith :: forall a. effect a -> IO a}

display ::
  ( Comonad space
  , MonadIO effect
  , ?ref :: MVar (Activity space effect)
  , ?run :: Runner effect
  ) =>
  Reducer Tb2.Termbox2 Event
display = forever $ do
  view <- embed $ do
    space <- takeMVar ?ref
    return $ extract space $ \action -> do
      !space' <- move (const id) action (space =>> return)
      putMVar ?ref space'
  let Console (Screen render) handle = view
  embed $ Tb2.clear >> render >> Tb2.present
  await >>= embed . liftIO . runWith ?run . handle

{- | Sets up a component for execution and catches exceptions. @run@
discharges the effect monad to 'IO' ('id' for 'IO'-based components).
@setup@ runs once after 'Tb2.init', before the loop (e.g. mouse input).
-}
mount ::
  (Comonad space, MonadIO effect) =>
  (forall a. effect a -> IO a) ->
  Tb2.Termbox2 () ->
  Activity space effect ->
  IO ()
mount run setup !component = do
  !ref <- newMVar component
  let ?ref = ref
      ?run = Runner run
  bracket_
    (Tb2.runTermbox2 (tbInit >> setup))
    (Tb2.runTermbox2 Tb2.shutdown)
    (Tb2.runTermbox2 $ deliver $ events >< loopOrQuit >< display)
 where
  -- 'Tb2.init' opens /dev/tty directly, which doesn't exist on Windows -
  -- 'Tb2.initRwFd' against stdin/stdout (fds 0/1) instead, there.
#if defined(mingw32_HOST_OS)
  tbInit = Tb2.initRwFd (0 :: Int) (1 :: Int)
#else
  tbInit = Tb2.init
#endif

-- = Part 3: Drawing utilities.

glyphCode :: (Integral n) => Char -> n
glyphCode = fromIntegral . ord

blockGlyph :: (Integral n) => n
blockGlyph = glyphCode '▄'

drawBlock :: Int -> Int -> Screen ()
drawBlock x y = Screen $! Tb2.setCell x y blockGlyph Tb2.colorWhite Tb2.colorDefault

{- | Draw a string at an arbitrary position with given fg\/bg - the
general-purpose escape hatch 'centerText'\/'footerText'\/'statusText'
don't provide, for anything laid out as more than one fixed string.
-}
drawText ::
  Int -> Int -> Tb2.Tb2ColorAttr -> Tb2.Tb2ColorAttr -> String -> Screen ()
drawText x y fg bg str = Screen $! Tb2.print x y fg bg str

{- | Draw a single glyph at an arbitrary position with given fg\/bg - the
building block 'drawVLine'\/'drawHLine' share. Colors are explicit since
different callers want different contrast (e.g. ruling should recede).
-}
drawGlyph ::
  Int -> Int -> Tb2.Tb2ColorAttr -> Tb2.Tb2ColorAttr -> Int -> Screen ()
drawGlyph x y fg bg ch = Screen $! Tb2.setCell x y (fromIntegral ch) fg bg

{- | A vertical line of the given glyph from @(x, y0)@ to @(x, y1)@,
inclusive. Glyph is caller-supplied rather than fixed @'│'@, since a
grid's ruling and a dialog's border reasonably want different weights.
-}
drawVLine ::
  Int -> Int -> Int -> Tb2.Tb2ColorAttr -> Tb2.Tb2ColorAttr -> Int -> Screen ()
drawVLine x y0 y1 fg bg ch = forM_ [y0 .. y1] $ \y -> drawGlyph x y fg bg ch

{- | A horizontal line of the given glyph from @(x0, y)@ to @(x1, y)@,
inclusive. See 'drawVLine' for why the glyph isn't fixed.
-}
drawHLine ::
  Int -> Int -> Int -> Tb2.Tb2ColorAttr -> Tb2.Tb2ColorAttr -> Int -> Screen ()
drawHLine y x0 x1 fg bg ch = forM_ [x0 .. x1] $ \x -> drawGlyph x y fg bg ch

drawRect :: Int -> Int -> Int -> Int -> Screen ()
drawRect left top w h =
  Screen $! do
    let bottom = top + h - 1
    let right = left + w - 1
    let setCell x y ch = Tb2.setCell x y ch Tb2.colorGreen Tb2.colorBlack
    forM_ [left .. right] $ \i -> do
      setCell i top 0x2500
      setCell i bottom 0x2500
    forM_ [top .. bottom] $ \i -> do
      setCell left i 0x2502
      setCell right i 0x2502
    setCell left top 0x250C
    setCell right top 0x2510
    setCell left bottom 0x2514
    setCell right bottom 0x2518

width, height :: Screen Int
width = Screen Tb2.width
height = Screen Tb2.height

screenBorder :: Int -> Screen ()
screenBorder border = do
  w <- width
  h <- height
  drawRect border border (w - border) (h - border)

centerText :: String -> Screen ()
centerText msg =
  Screen $! do
    w <- Tb2.width
    h <- Tb2.height
    let cx = (w `div` 2) - length msg `div` 2
    let cy = h `div` 2
    let fgAttrs = Tb2.colorGreen <> Tb2.attrUnderline <> Tb2.attrBold
    let bgAttrs = Tb2.colorMagenta
    Tb2.print cx cy fgAttrs bgAttrs msg

footerText :: String -> Screen ()
footerText msg =
  Screen $! do
    h <- Tb2.height
    let cx = 2
    let cy = h - 2
    Tb2.print cx cy Tb2.colorGreen Tb2.colorDefault msg

{- | Takes color explicitly: callers under a non-default output mode
(see 'Tb2.setOutputMode') need a color value that still means what they
intend under that mode, rather than one hardcoded for the default.
-}
statusText :: Tb2.Tb2ColorAttr -> Tb2.Tb2ColorAttr -> String -> Screen ()
statusText fg bg msg =
  Screen $! do
    w <- Tb2.width
    h <- Tb2.height
    let cx = w - length msg - 4
    let cy = h - 2
    Tb2.print cx cy fg bg msg

-- | Wrap another Activity with a tasteful border.
withBorder ::
  (Comonad space) =>
  Activity space effect ->
  Activity (StoreT () space) effect
withBorder inner = StoreT (inner =>> render) ()
 where
  render child () send =
    let ~(Console rC uC) = extract child $ send . hoist adapt
     in Console (rC >> screenBorder 0) uC

  adapt wrapped = let (idx, k) = runStoreT wrapped in ($ k) <$> idx
  {-# INLINE adapt #-}

-- = Part 4: Composing a dynamic list of homogeneous children.

{- | A focus-navigable list of independently-stateful 'Activity's sharing
one 'space'. Built on 'Tape' so 'moveL'\/'moveR' mean "focus prev\/next"
for free; only the focused child renders or receives events. [^3]
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

{- | Build a list 'Activity' from a starting child and the rest of a
finite list. First predicate moves focus left, second right (clamped to
real ends); anything else goes to the focused child's own update.
-}
list ::
  (Comonad space) =>
  (Event -> Bool) ->
  (Event -> Bool) ->
  Activity space effect ->
  [Activity space effect] ->
  Activity (ListSpace space) effect
list isPrev isNext x0 rest =
  ListSpace (0, length rest) (fmap wireChild (tapeFromList x0 rest))
 where
  wireChild child = child =>> renderChild

  renderChild childHere send =
    let ~(Console rC uC) = extract childHere $ send . hoist adapt
        handle event
          | isPrev event = send focusPrev
          | isNext event = send focusNext
          | otherwise = uC event
     in Console rC handle

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

{- [^1]:
'modify' goes via 'pos'/'seek' rather than 'seeks' specifically so the new
index can be forced: 'Control.Comonad.Trans.Store.StoreT' calls itself the
"strict" store transformer, but its index field carries no strictness
annotation and 'seeks' builds the updated index as an unforced thunk. A
long enough chain of 'modify's between renders would otherwise leak.
-}

{- [^2]:
This is deliberate: Termbox2 puts the terminal into raw mode, and without
a quit key the runtime itself guarantees, a broken or incomplete 'update'
function could leave a session with no way out short of killing the
process from elsewhere.
-}

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

{- [^5]:
Used to wrap a bare 'Tb2.Tb2Event' with no other variants, with a FIXME
noting the tb2 type shouldn't leak through like that. 'Tick' is the
reason to finally generalize it - 'events' now polls with a timeout
('Tb2.peekEvent') instead of blocking forever on 'Tb2.pollEvent', so a
timeout needs some value of its own to become.
-}
