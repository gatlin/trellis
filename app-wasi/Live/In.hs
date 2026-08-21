{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module: Live.In
Description: wasm32-wasi backend - the "!"-prefixed mini-language and its
parser are unchanged from native. @!Ns url@ (ShellInterval) is real here:
no shell exists in a browser sandbox, so it's reinterpreted, not ported -
same "interval plus a string" shape, but the string is a URL, re-fetched
every @N@ seconds via a genuine, repeating torc 'Observable' built with
'shift', rather than a shell command re-run. @!tail path@ (TailFile) has
no comparably direct browser analog (no filesystem to tail), so it stays
the placeholder it always was: a real torc Observable, built and torn down
exactly like the real thing, that just emits a fixed "unsupported" value.

Deliberately avoids 'GHC.Wasm.Prim.JSString' entirely, both directions:
this GHC snapshot (9.14.1.20260731) can't marshal it at all (a hard
compile error - its generated stub references @rts_getJSString@\/
@rts_mkJSString@\/@HsJSString@ without ever declaring them, confirmed
empirically getting Phase 0's spike working, see @wasm\/spike\/Tick.hs@).
Also avoids 'GHC.IO.Encoding'\/locale-dependent encoding for the same
reason a past wasi port attempt at a different library in this project
needed hand-rolled termios\/pipe shims: wasi-libc doesn't implement
everything base's usual text-encoding path assumes, and that's unproven
territory not worth risking here when a ~20-line hand-rolled UTF-8
codec is simpler and has no such dependency at all.

The URL crosses Haskell->JS exactly once per subscription, synchronously,
inside a single @unsafe@ JSFFI call: encoded to raw bytes, handed over as
a pointer+length into wasm linear memory (@__exports.memory@, per the GHC
wasm user's guide's own documented pattern for this), and decoded into an
independent JS string entirely JS-side before that call returns - so the
pointer only needs to stay valid for that one synchronous call, not for
the subscription's lifetime ('Foreign.Marshal.Array.withArrayLen's
auto-freed temporary buffer is exactly right for this). The response body
crosses JS->Haskell repeatedly (once per poll, for as long as the
subscription lives) through one persistent buffer allocated up front and
reused every tick, not a fresh allocation each time - capped at
'responseBufCap' bytes, truncating anything larger, freed via a second
callback fired from the same cleanup closure 'shift' hands to torc's
@Activity.finish()@.
-}
module Live.In (
  LiveSpec (..),
  parseLiveSpec,
  literal,
  declareSubscription,
  declareExternalSubscription,
) where

import Control.Concurrent.STM.MonadIO (TVar, modifyTVar_)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Time.Clock (NominalDiffTime)
import Data.Word (Word8)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Marshal.Array (peekArray, withArrayLen)
import Foreign.Ptr (Ptr, ptrToIntPtr)
import Formula (Expr (..), ExprF (..))
import GHC.Wasm.Prim (JSVal)
import qualified Trellis.Orc as Orc

-- | A cell fed by something other than a pure formula.
data LiveSpec
  = {- | @!Ns url@ - re-fetches @url@ every @N@ seconds; the cell becomes
    the response body (or an @\"ERR: ...\"@ value on failure).
    -}
    ShellInterval NominalDiffTime String
  | {- | @!tail path@ - still unsupported here; no filesystem to tail in a
    browser sandbox.
    -}
    TailFile FilePath
  deriving (Eq, Show)

{- | Parses the "!"-prefixed mini-language. "!" is unused by
'Parser.parseExpr's grammar (cell refs use "@"), so there's no collision
with an ordinary formula - a spec and a formula can never be ambiguous.
-}
parseLiveSpec :: String -> Maybe LiveSpec
parseLiveSpec ('!' : rest) = case words rest of
  ["tail", path] -> Just (TailFile path)
  (intervalTok : cmdWords@(_ : _)) -> do
    n <- parseSeconds intervalTok
    Just (ShellInterval n (unwords cmdWords))
  _ -> Nothing
parseLiveSpec _ = Nothing

-- | Parses a trailing-@s@ seconds token, e.g. @"5s"@ or @"0.5s"@.
parseSeconds :: String -> Maybe NominalDiffTime
parseSeconds tok = case reverse tok of
  ('s' : rdigits) -> case reads (reverse rdigits) :: [(Double, String)] of
    [(n, "")] -> Just (realToFrac n)
    _ -> Nothing
  _ -> Nothing

{- | Bypasses 'Parser.parseExpr' entirely - live output is a value, not
formula syntax, same as any typed-in @NumLitF@\/@StrLitF@ literal.
-}
literal :: String -> Expr
literal raw = case reads trimmed of
  [(n, "")] -> Expr (NumLitF n)
  _ -> Expr (StrLitF trimmed)
 where
  trimmed = dropWhileEnd isSpace (dropWhile isSpace raw)

{- | Hand-rolled, dependency-free UTF-8 - see the module haddock for why
this doesn't just reach for 'GHC.IO.Encoding'.
-}
utf8Encode :: String -> [Word8]
utf8Encode = concatMap encodeChar
 where
  encodeChar c
    | n < 0x80 = [fromIntegral n]
    | n < 0x800 =
        [ 0xC0 .|. fromIntegral (n `shiftR` 6)
        , contByte n
        ]
    | n < 0x10000 =
        [ 0xE0 .|. fromIntegral (n `shiftR` 12)
        , contByte (n `shiftR` 6)
        , contByte n
        ]
    | otherwise =
        [ 0xF0 .|. fromIntegral (n `shiftR` 18)
        , contByte (n `shiftR` 12)
        , contByte (n `shiftR` 6)
        , contByte n
        ]
   where
    n = fromEnum c
  contByte n = 0x80 .|. (fromIntegral n .&. 0x3F)

-- | The inverse of 'utf8Encode' - malformed/truncated input just stops
-- decoding early rather than throwing, since the source is always either
-- our own 'utf8Encode' output or a real fetch response.
utf8Decode :: [Word8] -> String
utf8Decode [] = []
utf8Decode (b0 : bs)
  | b0 < 0x80 = toEnum (fromIntegral b0) : utf8Decode bs
  | b0 .&. 0xE0 == 0xC0, (b1 : rest) <- bs =
      toEnum (((fromIntegral b0 .&. 0x1F) `shiftL` 6) .|. cont b1) : utf8Decode rest
  | b0 .&. 0xF0 == 0xE0, (b1 : b2 : rest) <- bs =
      toEnum
        ( ((fromIntegral b0 .&. 0x0F) `shiftL` 12)
            .|. (cont b1 `shiftL` 6)
            .|. cont b2
        )
        : utf8Decode rest
  | b0 .&. 0xF8 == 0xF0, (b1 : b2 : b3 : rest) <- bs =
      toEnum
        ( ((fromIntegral b0 .&. 0x07) `shiftL` 18)
            .|. (cont b1 `shiftL` 12)
            .|. (cont b2 `shiftL` 6)
            .|. cont b3
        )
        : utf8Decode rest
  | otherwise = utf8Decode bs
 where
  cont b = fromIntegral b .&. 0x3F :: Int

-- | The plain 'Int' JSFFI can actually carry - wasm32's 'Int' is 32-bit,
-- matching pointer width exactly.
ptrArg :: Ptr a -> Int
ptrArg = fromIntegral . ptrToIntPtr

-- | Fixed capacity for the reused response buffer - generous for a
-- realistic polled value (a number, a JSON snippet, a short status text)
-- without unbounded per-subscription growth. Longer responses truncate.
responseBufCap :: Int
responseBufCap = 65536

{- | The real @ShellInterval@ path. Builds one torc 'Observable' via
'shift' that fetches the URL immediately, then again every @intervalMs@,
publishing the response body (or @-1@ on any fetch failure) back through
'onNext'; the cleanup closure @shift@'s function returns clears the
interval and fires 'onDone' once, so the persistent buffer gets freed on
cancellation rather than only when the whole page unloads.
-}
foreign import javascript unsafe
  "const url = new TextDecoder().decode(new Uint8Array(__exports.memory.buffer, $1, $2)); \
  \const bufPtr = $3, bufCap = $4, intervalMs = $5, onNext = $6, onDone = $7; \
  \const obs = torc.shift((publish) => { \
  \  const tick = () => { \
  \    fetch(url).then((r) => r.text()).then((text) => { \
  \      const bytes = new TextEncoder().encode(text); \
  \      const n = Math.min(bytes.length, bufCap); \
  \      new Uint8Array(__exports.memory.buffer, bufPtr, n).set(bytes.subarray(0, n)); \
  \      publish(n); \
  \    }).catch(() => publish(-1)); \
  \  }; \
  \  tick(); \
  \  const id = setInterval(tick, intervalMs); \
  \  return () => { clearInterval(id); onDone(0); }; \
  \}); \
  \return obs.subscribe({ next: onNext });"
  js_pollFetch ::
    Int -> Int -> Int -> Int -> Int -> JSVal -> JSVal -> IO JSVal

-- | torc's @pure@, for @TailFile@'s still-placeholder message - a
-- one-shot Observable, unlike @ShellInterval@'s real repeating one.
foreign import javascript unsafe "torc.pure(1)"
  js_torcPureTail :: IO JSVal

-- | torc's @Observable.subscribe@, given an @{ next }@ observer whose
-- @next@ is a callback already wrapped via 'makeCallback'.
foreign import javascript unsafe "$1.subscribe({ next: $2 })"
  js_subscribe :: JSVal -> JSVal -> IO JSVal

-- | Turns a Haskell closure into a callable JS value.
foreign import javascript "wrapper sync"
  makeCallback :: (Int -> IO ()) -> IO JSVal

{- | Subscribes to @spec@'s (real, for ShellInterval; placeholder, for
TailFile) torc 'Observable', writing every result it produces into
@mailbox@ tagged with @pos@; returns the child 'Orc.Group' the caller
keeps (in 'SheetState.subscriptions') to cancel it later.
-}
declareSubscription ::
  Orc.Group ->
  TVar [((Int, Int), Expr)] ->
  (Int, Int) ->
  LiveSpec ->
  IO Orc.Group
declareSubscription root mailbox pos (ShellInterval interval url) = do
  buf <- mallocBytes responseBufCap :: IO (Ptr Word8)
  onNext <- makeCallback $ \n ->
    if n < 0
      then modifyTVar_ mailbox ((pos, literal "ERR: fetch failed") :)
      else do
        bytes <- peekArray n buf
        modifyTVar_ mailbox ((pos, literal (utf8Decode bytes)) :)
  onDone <- makeCallback $ \_ -> free buf
  let intervalMs = max 1 (round (interval * 1000))
      urlBytes = utf8Encode url
  activity <-
    withArrayLen urlBytes $ \len ptr ->
      js_pollFetch
        (ptrArg ptr)
        len
        (ptrArg buf)
        responseBufCap
        intervalMs
        onNext
        onDone
  Orc.newChildGroup root (Orc.Activity activity)
declareSubscription root mailbox pos (TailFile _) = do
  cb <- makeCallback $ \_ ->
    modifyTVar_
      mailbox
      ((pos, literal "ERR: file tailing isn't supported in the browser") :)
  obs <- js_torcPureTail
  activity <- js_subscribe obs cb
  Orc.newChildGroup root (Orc.Activity activity)

{- | Wraps a raw JS value in @torc.shift@ (@(publish) => cleanupFn@,
torc's own callback shape - see @~\/code\/torc\/index.ts@), then
subscribes exactly like 'declareSubscription' does. Deliberately
duck-typed rather than requiring torc's own 'Observable' type in the
public API: any function shaped @(next) => cleanupFn@ works, so an
RxJS 'Observable', a torc one, or a hand-rolled callback registrar all
adapt with at most a one-line wrapper on the JS side - the wasm
component's public @bindIn@ (see @wasm\/trellis-sheet.js@) hands this
straight through unchanged. Every published value is coerced to text
via JS's own @String(value)@ before crossing (same JSString-avoidance
reasoning as 'declareSubscription' - see the module haddock), so a
numeric or object-shaped source still works, it just always becomes a
cell's text the same way every other live value does.

The returned 'Orc.Group' is deliberately NOT the caller's only handle
on cancellation - the JS-side 'Activity' this wraps also gets closed
whenever the *caller* (the web component wrapper) replaces or clears
this binding, which is tracked JS-side, not here; this function only
guarantees the buffer backing @onNext@ gets freed exactly once,
whichever side cancels first.
-}
declareExternalSubscription ::
  Orc.Group -> TVar [((Int, Int), Expr)] -> (Int, Int) -> JSVal -> IO Orc.Group
declareExternalSubscription root mailbox pos subscribeFn = do
  buf <- mallocBytes responseBufCap :: IO (Ptr Word8)
  onNext <- makeCallback $ \n -> do
    bytes <- peekArray n buf
    modifyTVar_ mailbox ((pos, literal (utf8Decode bytes)) :)
  onDone <- makeCallback $ \_ -> free buf
  activity <-
    js_bindExternal subscribeFn (ptrArg buf) responseBufCap onNext onDone
  Orc.newChildGroup root (Orc.Activity activity)

{- | @subscribeFn@ is called once via @torc.shift@, exactly like
'js_pollFetch' builds its own Observable internally - the difference is
this one comes from outside rather than being built here. The returned
'JSVal' wraps torc's own 'Activity' so that finishing it *also* frees
this subscription's buffer (via @onDone@), not just runs whatever
cleanup @subscribeFn@ itself returned - both need to happen exactly
once, on whichever side cancels first.
-}
foreign import javascript unsafe
  "const subscribeFn = $1, bufPtr = $2, bufCap = $3, onNext = $4, onDone = $5; \
  \const obs = torc.shift((publish) => subscribeFn(publish)); \
  \const inner = obs.subscribe({ next: (value) => { \
  \  const bytes = new TextEncoder().encode(String(value)); \
  \  const n = Math.min(bytes.length, bufCap); \
  \  new Uint8Array(__exports.memory.buffer, bufPtr, n).set(bytes.subarray(0, n)); \
  \  onNext(n); \
  \} }); \
  \let done = false; \
  \return { finish: () => { if (done) return; done = true; inner.finish(); onDone(0); } };"
  js_bindExternal :: JSVal -> Int -> Int -> JSVal -> JSVal -> IO JSVal
