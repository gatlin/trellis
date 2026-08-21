import { WASI } from "node:wasi";
import { readFile } from "node:fs/promises";
import ghc_wasm_jsffi from "./tick.js";

// Minimal stand-in for the DOM, just enough for Tick.hs's FFI imports.
let keydownListener = null;
const elements = { tickCount: { textContent: "" }, lastKey: { textContent: "" } };
globalThis.document = {
  addEventListener(name, cb) {
    if (name === "keydown") keydownListener = cb;
  },
  getElementById(id) {
    return elements[id];
  },
};

console.log("step: reading wasm");
const wasi = new WASI({
  version: "preview1",
  args: ["tick.wasm"],
  env: { PATH: "", PWD: process.cwd() },
  preopens: { "/": "/" },
});
let __exports = {};
const wasmBuf = await readFile(new URL("./tick.wasm", import.meta.url));
console.log("step: instantiating");
const { instance } = await WebAssembly.instantiate(wasmBuf, {
  ghc_wasm_jsffi: ghc_wasm_jsffi(__exports),
  wasi_snapshot_preview1: wasi.wasiImport,
});
console.log("step: instantiated");
Object.assign(__exports, instance.exports);
console.log("step: calling wasi.initialize");
try {
  wasi.initialize(instance);
} catch (e) {
  console.error("wasi.initialize threw. typeof:", typeof e);
  console.error("constructor:", e && e.constructor && e.constructor.name);
  console.error("String(e):", String(e));
  console.error("keys:", e && Object.keys(e));
  console.error("full:", e);
  process.exit(1);
}
console.log("step: initialized, calling trellisSetup");

try {
  const r = __exports.trellisSetup();
  console.log("step: trellisSetup returned", r);
  await r;
} catch (e) {
  console.error("trellisSetup threw:", e, e && e.stack);
  process.exit(1);
}
console.log("setup done, listener registered:", keydownListener !== null);

// Fire a bunch of ticks with no input first - this exercises "repeated
// calls in a loop" in isolation, the specific risk from the prior
// attempt's documented failure mode.
for (let i = 0; i < 300; i++) {
  await __exports.trellisTick();
}
console.log("after 300 ticks, tickCount text:", elements.tickCount.textContent);
if (elements.tickCount.textContent !== "300") {
  console.error("FAIL: expected tickCount 300");
  process.exit(1);
}

// Now simulate a keydown (keyCode 65 = 'A') via the registered listener,
// then tick again and confirm it was observed.
keydownListener({ keyCode: 65 });
await __exports.trellisTick();
console.log("after keydown+tick, lastKey text:", elements.lastKey.textContent);
if (elements.lastKey.textContent !== "65") {
  console.error("FAIL: expected lastKey 65");
  process.exit(1);
}

// More ticks after the keydown, to make sure nothing wedges afterward.
for (let i = 0; i < 300; i++) {
  await __exports.trellisTick();
}
console.log("after 300 more ticks, tickCount text:", elements.tickCount.textContent);
if (elements.tickCount.textContent !== "601") {
  console.error("FAIL: expected tickCount 601");
  process.exit(1);
}

console.log("OK");
