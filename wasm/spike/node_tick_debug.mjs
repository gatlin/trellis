import { WASI } from "node:wasi";
import { readFile } from "node:fs/promises";
import ghc_wasm_jsffi from "./tick.js";

globalThis.document = {
  addEventListener() {},
  getElementById() {
    return { textContent: "" };
  },
};

const wasi = new WASI({
  version: "preview1",
  args: ["tick.wasm"],
  env: { PATH: "", PWD: process.cwd() },
  preopens: { "/": "/" },
});
const realProcExit = wasi.wasiImport.proc_exit;
const wrappedImport = new Proxy(wasi.wasiImport, {
  get(target, prop) {
    if (prop === "proc_exit") {
      return (code) => {
        console.error(">>> proc_exit called with code:", code);
        console.error(new Error("stack at proc_exit").stack);
        return realProcExit(code);
      };
    }
    const v = target[prop];
    if (typeof v === "function") {
      return (...args) => {
        try {
          return v.apply(target, args);
        } catch (e) {
          console.error(`>>> wasi import '${String(prop)}' threw:`, e);
          throw e;
        }
      };
    }
    return v;
  },
});

let __exports = {};
const wasmBuf = await readFile(new URL("./tick.wasm", import.meta.url));
const { instance } = await WebAssembly.instantiate(wasmBuf, {
  ghc_wasm_jsffi: ghc_wasm_jsffi(__exports),
  wasi_snapshot_preview1: wrappedImport,
});
Object.assign(__exports, instance.exports);

try {
  wasi.initialize(instance);
  console.log("initialize OK");
} catch (e) {
  console.error("initialize threw:", e);
}
