// Production bring-up for the real trellis wasm build - a grown-up
// version of wasm/spike/browser_main.mjs, following the same recipe
// (proven working there): instantiate against the WASI shim (a
// preopened directory is required even though the module never
// touches the filesystem in any load-bearing way - see
// wasm/spike/init_shim.c's note), initialize, then drive the real
// trellisTick export.
//
// Ticking is *not* a plain requestAnimationFrame loop, deliberately:
// nothing in this app animates continuously, so redrawing 60x/sec
// forever (even at total idle) was just wasted CPU/GPU - a real,
// user-reported "melts my CPU" issue. Two triggers instead: (1) every
// real DOM input event fires an immediate tick, via
// src-wasi/Trellis/UI/Screen.hs's listener-registration snippets
// calling window.__trellisTick right after delivering the event to
// Haskell, so input still feels instant; (2) a plain setInterval
// heartbeat at src-native/Trellis/UI/Screen.hs's own tickIntervalMs
// (250ms) drives everything else - live-cell updates, cursor blink if
// one's ever added, etc. - at the same cadence native already lives
// with at idle today. Not a new, worse tradeoff, just wasm no longer
// polling ~15x faster than native ever needed to.
import { WASI, File, OpenFile, ConsoleStdout, PreopenDirectory } from "./node_modules/@bjorn3/browser_wasi_shim/dist/index.js";
import ghc_wasm_jsffi from "./dist/trellis.js";
import createTrellisHost from "./trellis-host.mjs";
import * as torc from "./vendor/torc.js";

const status = document.getElementById("status");

// trellis-host.mjs is a factory now (see its own module comment - it
// needs to be, so wasm/trellis-sheet.js's <trellis-sheet> can create
// one independent host per instance instead of a single shared one).
// This page only ever has one canvas, so window.trellisHost is set
// once here and never needs repointing the way the component does.
window.trellisHost = createTrellisHost(document.getElementById("trellis-canvas"), 14);

// Live-cell JSFFI snippets (Live.In's generated glue) reference a bare
// `torc` global (`torc.pure(...)`, `torc.shift(...)`) - set up before the
// wasm module is instantiated, same reasoning as trellisHost above.
window.torc = torc;

const args = ["trellis.wasm"];
const env = [];
const fds = [
  new OpenFile(new File([])),
  ConsoleStdout.lineBuffered((msg) => console.log(`[wasi stdout] ${msg}`)),
  ConsoleStdout.lineBuffered((msg) => console.error(`[wasi stderr] ${msg}`)),
  new PreopenDirectory(".", []),
];
const wasi = new WASI(args, env, fds);

let __exports = {};
const resp = await fetch("./dist/trellis.wasm");
const wasmBuf = await resp.arrayBuffer();
const { instance } = await WebAssembly.instantiate(wasmBuf, {
  ghc_wasm_jsffi: ghc_wasm_jsffi(__exports),
  wasi_snapshot_preview1: wasi.wasiImport,
});
Object.assign(__exports, instance.exports);

wasi.initialize(instance);

window.__trellisTicksRun = 0;
// Exposed so Screen.hs's DOM listener registrations can fire an
// immediate tick right after delivering a real event to Haskell -
// see the module comment above. Set up before trellisSetup() runs
// (which is what actually registers those listeners), so there's no
// window where a real event could fire before this exists.
window.__trellisTick = () => {
  __exports.trellisTick();
  window.__trellisTicksRun++;
};

await __exports.trellisSetup();
status.textContent = "running";

// Trellis.UI.Screen's keydown listener is canvas-scoped now (not
// document-level - see src-wasi/Trellis/UI/Screen.hs's own note on
// why), so this single-purpose full-page demo focuses it immediately
// to preserve "just start typing" behavior - a real embedded
// <trellis-sheet> component deliberately does NOT auto-focus itself
// (see wasm/trellis-sheet.js), since stealing keyboard focus on
// connect is bad practice for a component sharing a page with other
// focusable content.
document.getElementById("trellis-canvas").focus();

// The idle/background heartbeat - see the module comment above for why
// this is 250ms and not a 60fps rAF loop.
const TICK_INTERVAL_MS = 250;
setInterval(() => window.__trellisTick(), TICK_INTERVAL_MS);
