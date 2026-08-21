import { WASI, File, OpenFile, ConsoleStdout, PreopenDirectory } from "./node_modules/@bjorn3/browser_wasi_shim/dist/index.js";
import ghc_wasm_jsffi from "./tick.js";

const args = ["tick.wasm"];
const env = [];
const fds = [
  new OpenFile(new File([])),
  ConsoleStdout.lineBuffered((msg) => console.log(`[wasi stdout] ${msg}`)),
  ConsoleStdout.lineBuffered((msg) => console.error(`[wasi stderr] ${msg}`)),
  // A preopened directory is required even though this module never
  // touches the filesystem - GHC's own generated __ghc_wasm_jsffi_init
  // constructor calls _Exit(71) during _initialize without one
  // (confirmed empirically under Node first, same fix applies here).
  new PreopenDirectory(".", []),
];
const wasi = new WASI(args, env, fds);

let __exports = {};
const resp = await fetch("./tick.wasm");
const wasmBuf = await resp.arrayBuffer();
const { instance } = await WebAssembly.instantiate(wasmBuf, {
  ghc_wasm_jsffi: ghc_wasm_jsffi(__exports),
  wasi_snapshot_preview1: wasi.wasiImport,
});
Object.assign(__exports, instance.exports);

wasi.initialize(instance);

await __exports.trellisSetup();
document.getElementById("status").textContent = "running";

window.__trellisTicksRun = 0;
function frame() {
  __exports.trellisTick();
  window.__trellisTicksRun++;
  requestAnimationFrame(frame);
}
requestAnimationFrame(frame);
