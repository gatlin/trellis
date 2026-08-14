# Plan: Trellis in the Browser

## Goal

Trellis compiles and runs in a browser tab, rendering into `xterm.js`, with
`termbox2-hs` doing all the terminal I/O heavy lifting and `torc` standing in
for `Trellis.Orc`'s external-world edges (in place of `System.Process`/real
filesystem). Native (`wasmtime` / real terminal) build stays fully working
and unforked at the module level wherever possible.

## Phase 0: Upstream the WASI patches into termbox2-hs itself

A throwaway hand-patched copy of `termbox.h` is the wrong shape for a real
target — `cabal.project.local` should point at one `termbox2-hs` that both
the native and wasm builds compile against.

- [ ] In `lib/termbox.h`, wrap the WASI-specific bits behind
      `#if defined(__wasi__)` (clang predefines this for the `wasm32-wasi`
      target) rather than maintaining a diverged file: the `termios.h`-shim
      types/decls, the disabled `init_resize_handler` body (no real
      `SIGWINCH` under WASI), and the `update_term_size` fallthrough to the
      cursor-position-report probe. Original POSIX code stays in the
      `#else` branch. One file, one source of truth.
- [ ] In `termbox2-hs.cabal`, add `if os(wasi)` stanzas for
      `cc-options: -D_WASI_EMULATED_SIGNAL -DTB_RESIZE_FALLBACK_MS=500` and
      `extra-libraries: wasi-emulated-signal`, so `wasm32-wasi-cabal build`
      just works with no extra flags to pass.
- [ ] Confirm `initRwFd` is the documented "no real tty" entry point the
      wasm target's `Main` calls (it already exists as of the current
      binding).
- [ ] Open question, deferred: is a resize hook needed? There's currently
      no way to tell termbox2 "the size changed" outside of `SIGWINCH`,
      which doesn't exist under WASI. A spreadsheet cares about resize a
      lot more than a toy demo does. Options: (a) fixed size at boot for
      v1; (b) a small `tb_wasi_notify_resize()`-style export that re-runs
      the CPR probe on demand, called from JS on `xterm.js`'s `onResize`.

## Phase 1: Trellis's own build gets a wasm target

- [ ] `cabal.project.local` points `termbox2-hs` at the Phase 0 version.
- [ ] `trellis.cabal` gets `if os(wasi)` stanzas: drop `process`,
      `directory`, `filepath` from `build-depends` for that target; swap in
      the `.Wasm` backend modules (Phase 2) for `other-modules`.
- [ ] New minimal wasm entry point (e.g. `app/Main.Wasm.hs`) — skips
      `getArgs`/CLI parsing entirely, calls `Tb2.initRwFd 0 1`, sources
      initial sheet content however the browser wiring decides (start
      blank for v1).

## Phase 2: Module split for the parts that can't be one implementation

Shared interface, two backends:

- [ ] **`Live.In`** — keep `LiveSpec` and the subscription-registration
      signature as the shared contract. `Live.In.Native` is today's
      `System.Process`-based implementation, untouched. `Live.In.Wasm` is
      torc-backed: each subscription builds a torc `Observable<string>` on
      the JS side (`shift` for WebSocket/SSE sources, `each` for anything
      already materialized), `.subscribe()`s a callback that forwards
      values through an exported Haskell push function straight into
      `Trellis.Orc`'s mailbox, and hands the `Activity` back so `Orc`'s
      teardown can call `.finish()` on it — same cancellation vocabulary on
      both sides, no translation layer needed.
- [ ] **File persistence** (`SheetFile`'s parse/serialize, `Main.hs`'s sheet
      load, `Update/Core.hs`'s save, `Keymap.hs`'s config) — pure
      parse/serialize logic is unchanged and shared. Only "read bytes from
      a path" / "write bytes to a path" gets backend-swapped. v1: wasm
      backend doesn't persist at all — matches how a missing path already
      means "start blank." Fast-follow: OPFS, ideally via a Worker so
      `FileSystemSyncAccessHandle` gives genuinely synchronous open/read/
      write, matching what `readFile`/`writeFile` already assume — no
      Asyncify-style plumbing needed for this path either.
- [ ] **`Trellis.Orc`/`Trellis.HIO`** — `threadDelay`, STM, `forkIO`-based
      groups. Expectation is no change needed (GHC's wasm backend's green
      threads and STM shouldn't need real OS threads), but this is
      unverified against this specific backend — first thing to sanity
      check once the build is wired up, before anything downstream of it.

## Phase 3: Terminal I/O

- [ ] Reuse the proven approach: `xterm.js` + a hand-rolled WASI preview1
      shim (~20 functions covers termbox2-hs's actual import surface) +
      `tb_init_rwfd(0, 1)`.
- [ ] Because `Trellis.UI.hs` already calls bounded `Tb2.peekEvent
      tickIntervalMs` (250ms) rather than infinite `pollEvent`, **Asyncify
      is likely unnecessary** for the core loop: the shim's `poll_oneoff`
      can always resolve immediately (ignore the requested timeout), and a
      plain `setInterval` at ~250ms becomes the real pacing mechanism,
      matching the existing tick-driven architecture. This drops the
      hardest, most fragile part of a from-scratch port.

## Phase 4: Validation strategy

- [ ] Build with `wasm32-wasi-cabal`, sanity-test under `wasmtime` with
      piped synthetic input (fake CPR reply, staggered fake events) before
      touching a browser at all.
- [ ] Then wire the real `xterm.js` + shim + torc-backed `Live.In.Wasm`.
- [ ] Keep visible wake/delivery diagnostics in the page during bring-up —
      this is what actually surfaced real bugs during the proof-of-concept
      (a misleading state reading, a demo-app redraw bug), not guessing.

## Explicitly deferred

- Live resize (Phase 0).
- OPFS-backed persistence (Phase 2, v1 ships without it).
- Whether `Trellis.Orc`'s concurrency needs changes under wasm (Phase 2,
  first thing to test once the skeleton builds).
