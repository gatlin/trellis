import { readFile } from "node:fs/promises";
import { WASI } from "node:wasi";
import ghc_wasm_jsffi from "./hello.js";

const wasi = new WASI({ version: "preview1", args: [], env: {} });

let __exports = {};
const wasmBuf = await readFile(new URL("./hello.wasm", import.meta.url));
const { instance } = await WebAssembly.instantiate(wasmBuf, {
  ghc_wasm_jsffi: ghc_wasm_jsffi(__exports),
  wasi_snapshot_preview1: wasi.wasiImport,
});
Object.assign(__exports, instance.exports);

wasi.initialize(instance);

const result = await __exports.hs_add(2, 3);
console.log("hs_add(2,3) =", result);
if (result !== 5) {
  console.error("FAIL: expected 5");
  process.exit(1);
}
console.log("OK");
