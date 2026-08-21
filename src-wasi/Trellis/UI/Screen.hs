{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

{- |
Module: Trellis.UI.Screen
Description: The canvas\/DOM-backed 'Screen' - wasm32-wasi's counterpart to
'src-native/Trellis/UI/Screen.hs' (same module name, swapped @hs-source-dirs@
per @os(wasi)@). JS drives the loop via @requestAnimationFrame@ calling the
exported, synchronous 'tick'; Haskell never blocks on JS at all - see
@\/home\/gcj\/.claude\/plans\/prancy-enchanting-star.md@'s "Event loop" design
note for why, and @wasm\/spike\/@ for the proven mechanics this builds on.

Deliberately avoids 'GHC.Wasm.Prim.JSString' everywhere (this GHC snapshot
can't marshal it - see the spike's own note) - every JSFFI boundary here is
'Int'\/'GHC.Wasm.Prim.JSVal' only. Text (glyphs, key names) is handled
entirely on the JS side, in @wasm\/trellis-host.mjs@, which every @unsafe@
import here calls into by name via @window.trellisHost@.
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
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.&.))
import Data.IORef
import Foreign.C.Types (CInt (..))
import GHC.Wasm.Prim (JSVal)
import System.IO.Unsafe (unsafePerformIO)
import Trellis.UI.Backend (Color, InputEvent (..), InputMode, OutputMode)
import qualified Trellis.UI.Backend as Backend
import Trellis.UI.Core (Action (..), Component, move)

-- | Every draw call is an immediate canvas mutation - no back buffer, no
-- separate "present" step, since a full redraw already happens once per
-- 'tick'. Constructor intentionally unexported (matches the native
-- backend's own 'Trellis.UI.Screen.Screen', which also keeps its wrapped
-- monad private).
newtype Screen a = Screen (IO a)
  deriving (Functor, Applicative, Monad)

runScreen :: Screen a -> IO a
runScreen (Screen a) = a

data Event = Input InputEvent | Tick
  deriving (Show, Eq)

data Console effect = Console (Screen ()) (Event -> effect ())

console :: (Event -> t) -> Screen () -> (t -> effect ()) -> Console effect
console update render send = Console render (send . update)

type Activity space effect =
  Component effect space (Action space) (Console effect)

activity ::
  (Event -> Action (Store s) effect ()) ->
  (s -> Screen ()) ->
  s ->
  Activity (Store s) effect
activity u r = store (console u . r)

-- = JSFFI: canvas setup/drawing, all via wasm/trellis-host.mjs's
-- window.trellisHost - see that file for the actual DOM/canvas code.

foreign import javascript unsafe "window.trellisHost.cols()"
  js_cols :: IO CInt

foreign import javascript unsafe "window.trellisHost.rows()"
  js_rows :: IO CInt

foreign import javascript unsafe "window.trellisHost.clear($1,$2,$3)"
  js_clear :: CInt -> CInt -> CInt -> IO ()

foreign import javascript unsafe
  "window.trellisHost.drawCell($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11)"
  js_drawCell ::
    CInt ->
    CInt ->
    CInt ->
    CInt ->
    CInt ->
    CInt ->
    CInt ->
    CInt ->
    CInt ->
    CInt ->
    CInt ->
    IO ()

-- = JSFFI: input listeners.

-- | Wraps a Haskell closure as a JS-callable value - the proven torc\/
-- Phase-0-spike "wrapper" pattern, reused for every DOM listener below.
-- Every listener receives the raw @Event@\/@MouseEvent@\/@WheelEvent@ as
-- an opaque 'JSVal' and extracts whatever fields it needs via small
-- separate 'unsafe' calls, exactly like the spike's own @jsEventKeyCode@.
foreign import javascript "wrapper sync"
  makeListener :: (JSVal -> IO ()) -> IO JSVal

-- | Every listener below also fires an immediate 'tick' right after
-- delivering the DOM event to Haskell (see 'window.__trellisTick', set
-- up by @wasm\/main.mjs@ once the module's instantiated) - real input
-- gets drawn essentially the same frame, independent of whatever slower
-- heartbeat cadence drives idle ticks (see @main.mjs@'s own comment).
-- Guarded in case a listener somehow fires before that global exists.
-- | Canvas-scoped, not @document@-level: a page embedding this as a
-- component can't have it stealing every keydown on the page (and
-- two instances on one page each need their own keydowns, not every
-- keydown anywhere reaching both) - matches the mouse listeners below,
-- which were already scoped this way. Requires the canvas to actually
-- be focusable (@tabindex@) and focused for a real keydown to reach
-- it at all - the host page/component wrapper's job, not this file's.
foreign import javascript unsafe
  "window.trellisHost.canvasEl().addEventListener('keydown', (e) => { $1(e); if (window.__trellisTick) window.__trellisTick(); })"
  registerKeydown :: JSVal -> IO ()

foreign import javascript unsafe
  "window.trellisHost.canvasEl().addEventListener('mousedown', (e) => { $1(e); if (window.__trellisTick) window.__trellisTick(); })"
  registerMousedown :: JSVal -> IO ()

foreign import javascript unsafe
  "window.trellisHost.canvasEl().addEventListener('mouseup', (e) => { $1(e); if (window.__trellisTick) window.__trellisTick(); })"
  registerMouseup :: JSVal -> IO ()

foreign import javascript unsafe
  "window.trellisHost.canvasEl().addEventListener('mousemove', (e) => { $1(e); if (window.__trellisTick) window.__trellisTick(); })"
  registerMousemove :: JSVal -> IO ()

foreign import javascript unsafe
  "window.trellisHost.canvasEl().addEventListener('wheel', (e) => { $1(e); if (window.__trellisTick) window.__trellisTick(); })"
  registerWheel :: JSVal -> IO ()

foreign import javascript unsafe "window.trellisHost.namedKey($1)"
  js_namedKey :: JSVal -> IO CInt

foreign import javascript unsafe "window.trellisHost.charCode($1)"
  js_charCode :: JSVal -> IO CInt

foreign import javascript unsafe "window.trellisHost.mods($1)"
  js_mods :: JSVal -> IO CInt

foreign import javascript unsafe "window.trellisHost.mouseCellX($1)"
  js_mouseCellX :: JSVal -> IO CInt

foreign import javascript unsafe "window.trellisHost.mouseCellY($1)"
  js_mouseCellY :: JSVal -> IO CInt

foreign import javascript unsafe "window.trellisHost.mouseKey($1)"
  js_mouseKey :: JSVal -> IO CInt

foreign import javascript unsafe "window.trellisHost.mouseMotion($1)"
  js_mouseMotion :: JSVal -> IO CInt

foreign import javascript unsafe "$1.preventDefault()"
  js_preventDefault :: JSVal -> IO ()

-- = The event queue every listener pushes into, and 'tick' drains.

{-# NOINLINE eventQueueRef #-}
eventQueueRef :: IORef [InputEvent]
eventQueueRef = unsafePerformIO (newIORef [])

pushEvent :: InputEvent -> IO ()
pushEvent e = modifyIORef' eventQueueRef (\q -> q ++ [e])

onKeydown :: JSVal -> IO ()
onKeydown ev = do
  js_preventDefault ev -- Backspace/Tab/arrows must not also scroll/navigate the page.
  named <- js_namedKey ev
  ch <- js_charCode ev
  m <- js_mods ev
  pushEvent
    InputEvent
      { evtType = Backend.eventKey
      , evtMod = fromIntegral m
      , evtKey = fromIntegral named
      , evtCh = fromIntegral ch
      , evtW = 0
      , evtH = 0
      , evtX = 0
      , evtY = 0
      }

onMouse :: JSVal -> IO ()
onMouse ev = do
  key <- js_mouseKey ev
  m <- js_mouseMotion ev
  x <- js_mouseCellX ev
  y <- js_mouseCellY ev
  pushEvent
    InputEvent
      { evtType = Backend.eventMouse
      , evtMod = fromIntegral m
      , evtKey = fromIntegral key
      , evtCh = 0
      , evtW = 0
      , evtH = 0
      , evtX = fromIntegral x
      , evtY = fromIntegral y
      }

registerListeners :: IO ()
registerListeners = do
  kd <- makeListener onKeydown
  registerKeydown kd
  forM_ [registerMousedown, registerMouseup, registerMousemove, registerWheel] $ \reg -> do
    cb <- makeListener onMouse
    reg cb

-- = The tick-driven step: what 'mount' installs, what 'tick' runs.

{-# NOINLINE stepRef #-}
stepRef :: IORef (IO ())
stepRef = unsafePerformIO (newIORef (return ()))

-- | Called once at startup by 'app-wasi/Main.hs' - registers listeners
-- and returns the setup action 'mount' below wraps. Exported so the
-- harness (or a future dedicated setup export) can call it if 'mount'
-- alone isn't enough; currently 'mount' calls it directly.
setupListeners :: IO ()
setupListeners = registerListeners

{- | Called repeatedly by JS via @requestAnimationFrame@ (see
@wasm\/index.html@). Runs whatever step 'mount' installed - a no-op
before 'mount' has been called at all, which never happens in practice
since 'app-wasi/Main.hs' calls 'mount' before returning from its own
@foreign export@ed setup.
-}
foreign export javascript "trellisTick sync" tick :: IO ()
tick :: IO ()
tick = do
  step <- readIORef stepRef
  step

setInputMode :: InputMode -> Screen ()
setInputMode _ = return () -- no raw-terminal-mode analog under canvas

setOutputMode :: OutputMode -> Screen ()
setOutputMode _ = return () -- no ANSI-color-mode analog under canvas

{- | Sets up a component for execution. Unlike the native backend's
blocking 'mount' (which owns a @forever@ loop internally), this
registers listeners and installs the per-tick step into 'stepRef', then
returns immediately - JS's own @requestAnimationFrame@ loop is what
actually drives 'tick' afterward. @run@\/@setup@ have the same meaning
as native's 'mount' (discharge the effect monad to 'IO'; run once before
the loop starts).
-}
mount ::
  (Comonad space, MonadIO effect) =>
  (forall a. effect a -> IO a) ->
  Screen () ->
  Activity space effect ->
  IO ()
mount run (Screen setup) !component0 = do
  setupListeners
  setup
  spaceRef <- newIORef component0
  writeIORef stepRef (stepOnce run spaceRef)

popEvent :: IO Event
popEvent = do
  q <- readIORef eventQueueRef
  case q of
    [] -> return Tick
    (e : rest) -> writeIORef eventQueueRef rest >> return (Input e)

{- | One JS-driven tick's worth of work: exactly native's own per-iteration
'display' choreography (same 'move'\/'extract'\/'(=>>)' expression,
verbatim - see 'src-native/Trellis/UI/Screen.hs') - build this frame's
'Console' view from the *current* stored space, render it, then read the
next queued 'Event' (or synthesize 'Tick') and hand it to the view's own
handler, which is what actually writes the *next* space back into
'spaceRef' (via the closure captured below, exactly mirroring how
native's own handler closes over its '?ref').
-}
stepOnce ::
  (Comonad space, MonadIO effect) =>
  (forall a. effect a -> IO a) ->
  IORef (Activity space effect) ->
  IO ()
stepOnce run spaceRef = do
  space <- readIORef spaceRef
  let view =
        extract space $ \action -> do
          !space' <- move (const id) action (space =>> return)
          liftIO (writeIORef spaceRef space')
      Console (Screen render) handle = view
  js_clear 0 0 0
  render
  event <- popEvent
  _ <- run (handle event)
  return ()

-- = Drawing utilities.

glyphCode :: (Integral n) => Char -> n
glyphCode = fromIntegral . fromEnum

blockGlyph :: (Integral n) => n
blockGlyph = glyphCode '▄'

-- | Palette index -> RGB, the standard xterm 256-color layout: 0-15 the
-- classic 16 colors, 16-231 a 6x6x6 cube, 232-255 a grayscale ramp -
-- termbox2's own TB_OUTPUT_256 wire values are raw indices into exactly
-- this same table, so this reproduces native's rendering faithfully
-- (bugs and all - e.g. 'Trellis.UI.Backend.colorGreen' (3) really is
-- "yellow" in this table under 256-color mode, matching what native
-- actually sends over the wire, not what the name suggests).
paletteRGB :: Int -> (Int, Int, Int)
paletteRGB i
  | i < 16 = basic16 !! i
  | i < 232 =
      let idx = i - 16
          r = idx `div` 36
          g = (idx `div` 6) `mod` 6
          b = idx `mod` 6
          level n = if n == 0 then 0 else 55 + n * 40
       in (level r, level g, level b)
  | otherwise =
      let v = 8 + (i - 232) * 10 in (v, v, v)
 where
  basic16 =
    [ (0, 0, 0)
    , (128, 0, 0)
    , (0, 128, 0)
    , (128, 128, 0)
    , (0, 0, 128)
    , (128, 0, 128)
    , (0, 128, 128)
    , (192, 192, 192)
    , (128, 128, 128)
    , (255, 0, 0)
    , (0, 255, 0)
    , (255, 255, 0)
    , (0, 0, 255)
    , (255, 0, 255)
    , (0, 255, 255)
    , (255, 255, 255)
    ]

-- | A sensible ground when 'Trellis.UI.Backend.colorDefault' is set - a
-- plain terminal-like light-grey-on-black, since canvas has no ambient
-- "the terminal's own default colors" to defer to the way termbox2 does.
defaultFg, defaultBg :: (Int, Int, Int)
defaultFg = (229, 229, 229)
defaultBg = (0, 0, 0)

-- | Decodes a 'Color' into (r,g,b,bold,underline) - low byte is a raw
-- 256-palette index (see 'paletteRGB'), bit 0x2000 is the "default"
-- sentinel (checked first, overriding the palette lookup entirely - a
-- raw literal like @colorDefault = 0x2000@ has palette-index 0 in its
-- low byte too, which would otherwise wrongly render as black), bits
-- 0x0100\/0x0200 are the bold\/underline attribute flags.
decodeColor :: Bool -> Color -> (Int, Int, Int, Bool, Bool)
decodeColor isFg c =
  let n = fromIntegral c :: Int
      isDefault = n .&. 0x2000 /= 0
      (r, g, b) =
        if isDefault
          then if isFg then defaultFg else defaultBg
          else paletteRGB (n .&. 0xFF)
      bold = n .&. 0x0100 /= 0
      underline = n .&. 0x0200 /= 0
   in (r, g, b, bold, underline)

drawBlock :: Int -> Int -> Screen ()
drawBlock x y = drawGlyph x y Backend.colorWhite Backend.colorDefault blockGlyph

drawText :: Int -> Int -> Color -> Color -> String -> Screen ()
drawText x y fg bg str =
  forM_ (zip [x ..] str) $ \(i, c) -> drawGlyph i y fg bg (glyphCode c)

drawGlyph :: Int -> Int -> Color -> Color -> Int -> Screen ()
drawGlyph x y fg bg ch = Screen $! do
  let (fr, fg', fb, bold, underline) = decodeColor True fg
      (br, bg', bb, _, _) = decodeColor False bg
  js_drawCell
    (fromIntegral x)
    (fromIntegral y)
    (fromIntegral ch)
    (if bold then 1 else 0)
    (if underline then 1 else 0)
    (fromIntegral fr)
    (fromIntegral fg')
    (fromIntegral fb)
    (fromIntegral br)
    (fromIntegral bg')
    (fromIntegral bb)

drawVLine :: Int -> Int -> Int -> Color -> Color -> Int -> Screen ()
drawVLine x y0 y1 fg bg ch = forM_ [y0 .. y1] $ \y -> drawGlyph x y fg bg ch

drawHLine :: Int -> Int -> Int -> Color -> Color -> Int -> Screen ()
drawHLine y x0 x1 fg bg ch = forM_ [x0 .. x1] $ \x -> drawGlyph x y fg bg ch

drawRect :: Int -> Int -> Int -> Int -> Screen ()
drawRect left top w h = do
  let bottom = top + h - 1
      right = left + w - 1
      setCell x y ch = drawGlyph x y Backend.colorGreen Backend.colorBlack ch
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
width = Screen (fromIntegral <$> js_cols)
height = Screen (fromIntegral <$> js_rows)

screenSize :: IO (Int, Int)
screenSize = (,) <$> (fromIntegral <$> js_cols) <*> (fromIntegral <$> js_rows)

screenBorder :: Int -> Screen ()
screenBorder border = do
  w <- width
  h <- height
  drawRect border border (w - border) (h - border)

centerText :: String -> Screen ()
centerText msg = do
  w <- width
  h <- height
  let cx = (w `div` 2) - length msg `div` 2
      cy = h `div` 2
  drawText cx cy (Backend.colorGreen <> Backend.attrUnderline <> Backend.attrBold) Backend.colorMagenta msg

footerText :: String -> Screen ()
footerText msg = do
  h <- height
  drawText 2 (h - 2) Backend.colorGreen Backend.colorDefault msg

statusText :: Color -> Color -> String -> Screen ()
statusText fg bg msg = do
  w <- width
  h <- height
  let cx = w - length msg - 4
      cy = h - 2
  when (cx >= 0) (drawText cx cy fg bg msg)
