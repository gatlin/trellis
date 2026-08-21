{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Data.IORef
import Foreign.C.Types (CInt (..))
import GHC.Wasm.Prim (JSVal)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE queueRef #-}
queueRef :: IORef [Int]
queueRef = unsafePerformIO (newIORef [])

{-# NOINLINE tickCountRef #-}
tickCountRef :: IORef Int
tickCountRef = unsafePerformIO (newIORef 0)

{- | GHC's JSString FFI marshaling in this toolchain snapshot references
rts_getJSString/rts_mkJSString/HsJSString in its auto-generated C stub
without ever declaring them anywhere (confirmed: JSVal marshaling
generates its own inline forward declarations in the stub, JSString
marshaling doesn't) - a genuine toolchain gap in this snapshot, not a
mistake here. Every JSFFI boundary below uses only Int/JSVal, with any
actual text formatting done JS-side, to route around it entirely.
-}

-- | event.keyCode as a plain Int - avoids JSString for the event data
-- itself.
foreign import javascript unsafe "$1.keyCode"
  jsEventKeyCode :: JSVal -> IO CInt

-- | Turns a Haskell closure into a JS-callable function synchronously -
-- the proven torc "wrapper" pattern, reused here for a raw keydown
-- listener. Takes the raw Event JSVal (addEventListener's own callback
-- shape).
foreign import javascript "wrapper sync"
  makeKeydownCallback :: (JSVal -> IO ()) -> IO JSVal

foreign import javascript unsafe "document.addEventListener('keydown', $1)"
  registerKeydownListener :: JSVal -> IO ()

foreign import javascript unsafe
  "document.getElementById('tickCount').textContent = String($1)"
  setTickCountText :: CInt -> IO ()

foreign import javascript unsafe
  "document.getElementById('lastKey').textContent = String($1)"
  setLastKeyText :: CInt -> IO ()

onKeydown :: JSVal -> IO ()
onKeydown ev = do
  code <- jsEventKeyCode ev
  modifyIORef' queueRef (++ [fromIntegral code])

-- | Called once at setup: registers the keydown listener via the
-- "wrapper" pattern (no blocking/suspending JSFFI anywhere).
foreign export javascript "trellisSetup sync" setup :: IO ()
setup :: IO ()
setup = do
  cb <- makeKeydownCallback onKeydown
  registerKeydownListener cb

-- | Called repeatedly by JS via requestAnimationFrame. Drains the
-- queue, bumps the tick counter, writes both to the DOM for Playwright
-- to observe. No blocking/suspending calls anywhere in this path.
foreign export javascript "trellisTick sync" tick :: IO ()
tick :: IO ()
tick = do
  n <- readIORef tickCountRef
  let n' = n + 1
  writeIORef tickCountRef n'
  setTickCountText (fromIntegral n')
  q <- readIORef queueRef
  case q of
    [] -> return ()
    (k : rest) -> do
      writeIORef queueRef rest
      setLastKeyText (fromIntegral k)

main :: IO ()
main = return ()
