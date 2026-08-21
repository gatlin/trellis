{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

{- |
Module: Trellis.UI.Screen
Description: The termbox2-backed 'Screen' - the only file in the native
build that imports 'Termbox2' at all. A wasm\/browser build swaps this
whole module out for 'src-wasi/Trellis/UI/Screen.hs' (same module name,
different @hs-source-dirs@, per @os(wasi)@ in trellis.cabal) - everything
exported here needs a same-named, same-shaped counterpart there.
-}
module Trellis.UI.Screen (
  Screen,
  Event (..),
  Console (..),
  console,
  Activity,
  activity,
  mount,
  setInputMode,
  setOutputMode,
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
  screenSize,
) where

import Control.Comonad (Comonad (..), (=>>))
import Control.Comonad.Store (Store, store)
import Control.Concurrent.MonadIO (MVar, newMVar, putMVar, takeMVar)
import Control.Exception (bracket_)
import Control.Monad (forM_, forever, void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Char (ord)
import qualified Termbox2 as Tb2
import Trellis.UI.Backend (
  Color,
  EventType,
  InputEvent (..),
  InputMode,
  Key,
  Modifiers,
  OutputMode,
 )
import qualified Trellis.UI.Backend as Backend
import Trellis.UI.Core (Action (..), Component, move)
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

{- | DSL based on 'Tb2.Termbox2' for UI drawing operations. Deliberately
does not derive 'MonadIO' or export the constructor.
-}
newtype Screen a = Screen (Tb2.Termbox2 a)
  deriving (Functor, Applicative, Monad)

{- | Either a real input event, or a periodic wakeup carrying none - the
hook a component uses to notice state that changed off-screen (e.g. a
'Trellis.Orc' subscription's result) without any input at all.
-}
data Event = Input InputEvent | Tick
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

-- | Translates one of termbox2's own named key constants into the
-- backend-neutral 'Key' vocabulary; anything not in this app's own
-- vocabulary (function keys, etc.) becomes 'Backend.keyNone' - inert,
-- since nothing anywhere matches against an unnamed key, and 'evtCh'
-- being genuinely 0 for such an event still correctly fails
-- 'Update.Events.printableChar's "ch \/= 0" check.
toBackendKey :: Tb2.Tb2Key -> Key
toBackendKey k
  | k == Tb2.keyArrowUp = Backend.keyArrowUp
  | k == Tb2.keyArrowDown = Backend.keyArrowDown
  | k == Tb2.keyArrowLeft = Backend.keyArrowLeft
  | k == Tb2.keyArrowRight = Backend.keyArrowRight
  | k == Tb2.keyCtrlA = Backend.keyCtrlA
  | k == Tb2.keyCtrlB = Backend.keyCtrlB
  | k == Tb2.keyCtrlC = Backend.keyCtrlC
  | k == Tb2.keyCtrlD = Backend.keyCtrlD
  | k == Tb2.keyCtrlE = Backend.keyCtrlE
  | k == Tb2.keyCtrlF = Backend.keyCtrlF
  | k == Tb2.keyCtrlG = Backend.keyCtrlG
  | k == Tb2.keyCtrlH = Backend.keyCtrlH
  | k == Tb2.keyCtrlI = Backend.keyCtrlI
  | k == Tb2.keyCtrlJ = Backend.keyCtrlJ
  | k == Tb2.keyCtrlK = Backend.keyCtrlK
  | k == Tb2.keyCtrlL = Backend.keyCtrlL
  | k == Tb2.keyCtrlM = Backend.keyCtrlM
  | k == Tb2.keyCtrlN = Backend.keyCtrlN
  | k == Tb2.keyCtrlO = Backend.keyCtrlO
  | k == Tb2.keyCtrlP = Backend.keyCtrlP
  | k == Tb2.keyCtrlQ = Backend.keyCtrlQ
  | k == Tb2.keyCtrlR = Backend.keyCtrlR
  | k == Tb2.keyCtrlS = Backend.keyCtrlS
  | k == Tb2.keyCtrlT = Backend.keyCtrlT
  | k == Tb2.keyCtrlU = Backend.keyCtrlU
  | k == Tb2.keyCtrlV = Backend.keyCtrlV
  | k == Tb2.keyCtrlW = Backend.keyCtrlW
  | k == Tb2.keyCtrlX = Backend.keyCtrlX
  | k == Tb2.keyCtrlY = Backend.keyCtrlY
  | k == Tb2.keyCtrlZ = Backend.keyCtrlZ
  | k == Tb2.keyCtrlEnter = Backend.keyCtrlEnter
  | k == Tb2.keyCtrlEsc = Backend.keyCtrlEsc
  | k == Tb2.keyCtrlTab = Backend.keyCtrlTab
  | k == Tb2.keyBackTab = Backend.keyBackTab
  | k == Tb2.keyDelete = Backend.keyDelete
  | k == Tb2.keyHome = Backend.keyHome
  | k == Tb2.keyEnd = Backend.keyEnd
  | k == Tb2.keyPgUp = Backend.keyPgUp
  | k == Tb2.keyPgDn = Backend.keyPgDn
  | k == Tb2.keyF2 = Backend.keyF2
  | k == Tb2.keySpace = Backend.keySpace
  | k == Tb2.keyBackspace = Backend.keyBackspace
  | k == Tb2.keyBackspace2 = Backend.keyBackspace2
  | k == Tb2.keyMouseLeft = Backend.keyMouseLeft
  | k == Tb2.keyMouseRight = Backend.keyMouseRight
  | k == Tb2.keyMouseMiddle = Backend.keyMouseMiddle
  | k == Tb2.keyMouseRelease = Backend.keyMouseRelease
  | k == Tb2.keyMouseWheelUp = Backend.keyMouseWheelUp
  | k == Tb2.keyMouseWheelDown = Backend.keyMouseWheelDown
  | otherwise = Backend.keyNone

-- | Every field here shares an identical wire layout with 'Tb2.Tb2Event'
-- except '_key' (see 'toBackendKey') - 'EventType'\/'Modifiers' are
-- numerically identical to termbox2's own @TB_EVENT_*@\/@TB_MOD_*@ values
-- by construction (see 'Trellis.UI.Backend'), so those are plain
-- 'fromIntegral'.
fromTb2Event :: Tb2.Tb2Event -> InputEvent
fromTb2Event e =
  InputEvent
    { evtType = fromIntegral (Tb2._type e) :: EventType
    , evtMod = fromIntegral (Tb2._mod e) :: Modifiers
    , evtKey = toBackendKey (Tb2._key e)
    , evtCh = Tb2._ch e
    , evtW = Tb2._w e
    , evtH = Tb2._h e
    , evtX = Tb2._x e
    , evtY = Tb2._y e
    }

-- | 'Color'\/'OutputMode'\/'InputMode' are numerically identical to
-- termbox2's own encodings by construction (see 'Trellis.UI.Backend'),
-- so every direction here is a plain 'fromIntegral'.
toTb2Color :: Color -> Tb2.Tb2ColorAttr
toTb2Color = fromIntegral

toTb2Output :: OutputMode -> Tb2.Tb2Output
toTb2Output = fromIntegral

toTb2Input :: InputMode -> Tb2.Tb2Input
toTb2Input = fromIntegral

events :: Generator Tb2.Termbox2 Event
events = forever $ do
  !event <- embed (Tb2.peekEvent tickIntervalMs)
  case event of
    Nothing -> yield Tick
    Just !event' -> yield (Input (fromTb2Event event'))

{- | Ctrl+Q is a global escape hatch: it short-circuits here, before any
'Activity's own 'update' logic, so no component can swallow it - a queued
'Tick' never delays it.
-}
loopOrQuit :: Tube Tb2.Termbox2 Event Event
loopOrQuit = forever $ do
  event <- await
  case event of
    Input evt | evtKey evt == Backend.keyCtrlQ -> finish
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
@setup@ runs once after init, before the loop (e.g. mouse input).
-}
mount ::
  (Comonad space, MonadIO effect) =>
  (forall a. effect a -> IO a) ->
  Screen () ->
  Activity space effect ->
  IO ()
mount run (Screen setup) !component = do
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

setInputMode :: InputMode -> Screen ()
setInputMode im = Screen $! void (Tb2.setInputMode (toTb2Input im))

setOutputMode :: OutputMode -> Screen ()
setOutputMode om = Screen $! void (Tb2.setOutputMode (toTb2Output om))

-- = Drawing utilities.

glyphCode :: (Integral n) => Char -> n
glyphCode = fromIntegral . ord

blockGlyph :: (Integral n) => n
blockGlyph = glyphCode '▄'

drawBlock :: Int -> Int -> Screen ()
drawBlock x y =
  Screen $!
    Tb2.setCell x y blockGlyph (toTb2Color Backend.colorWhite) (toTb2Color Backend.colorDefault)

{- | Draw a string at an arbitrary position with given fg\/bg - the
general-purpose escape hatch 'centerText'\/'footerText'\/'statusText'
don't provide, for anything laid out as more than one fixed string.
-}
drawText :: Int -> Int -> Color -> Color -> String -> Screen ()
drawText x y fg bg str = Screen $! Tb2.print x y (toTb2Color fg) (toTb2Color bg) str

{- | Draw a single glyph at an arbitrary position with given fg\/bg - the
building block 'drawVLine'\/'drawHLine' share. Colors are explicit since
different callers want different contrast (e.g. ruling should recede).
-}
drawGlyph :: Int -> Int -> Color -> Color -> Int -> Screen ()
drawGlyph x y fg bg ch =
  Screen $! Tb2.setCell x y (fromIntegral ch) (toTb2Color fg) (toTb2Color bg)

{- | A vertical line of the given glyph from @(x, y0)@ to @(x, y1)@,
inclusive. Glyph is caller-supplied rather than fixed @'│'@, since a
grid's ruling and a dialog's border reasonably want different weights.
-}
drawVLine :: Int -> Int -> Int -> Color -> Color -> Int -> Screen ()
drawVLine x y0 y1 fg bg ch = forM_ [y0 .. y1] $ \y -> drawGlyph x y fg bg ch

{- | A horizontal line of the given glyph from @(x0, y)@ to @(x1, y)@,
inclusive. See 'drawVLine' for why the glyph isn't fixed.
-}
drawHLine :: Int -> Int -> Int -> Color -> Color -> Int -> Screen ()
drawHLine y x0 x1 fg bg ch = forM_ [x0 .. x1] $ \x -> drawGlyph x y fg bg ch

drawRect :: Int -> Int -> Int -> Int -> Screen ()
drawRect left top w h =
  Screen $! do
    let bottom = top + h - 1
    let right = left + w - 1
    let setCell x y ch =
          Tb2.setCell x y ch (toTb2Color Backend.colorGreen) (toTb2Color Backend.colorBlack)
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

-- | The current terminal size, outside a 'Screen' action - what
-- 'Update.Navigation.termSize' needs for layout math it does between
-- frames, not while actually drawing one.
screenSize :: IO (Int, Int)
screenSize = Tb2.runTermbox2 ((,) <$> Tb2.width <*> Tb2.height)

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
    let fgAttrs = toTb2Color (Backend.colorGreen <> Backend.attrUnderline <> Backend.attrBold)
    let bgAttrs = toTb2Color Backend.colorMagenta
    Tb2.print cx cy fgAttrs bgAttrs msg

footerText :: String -> Screen ()
footerText msg =
  Screen $! do
    h <- Tb2.height
    let cx = 2
    let cy = h - 2
    Tb2.print cx cy (toTb2Color Backend.colorGreen) (toTb2Color Backend.colorDefault) msg

{- | Takes color explicitly: callers under a non-default output mode
(see 'setOutputMode') need a color value that still means what they
intend under that mode, rather than one hardcoded for the default.
-}
statusText :: Color -> Color -> String -> Screen ()
statusText fg bg msg =
  Screen $! do
    w <- Tb2.width
    h <- Tb2.height
    let cx = w - length msg - 4
    let cy = h - 2
    Tb2.print cx cy (toTb2Color fg) (toTb2Color bg) msg
