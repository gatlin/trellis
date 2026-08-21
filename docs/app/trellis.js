// This file implements the JavaScript runtime logic for Haskell
// modules that use JSFFI. It is not an ESM module, but the template
// of one; the post-linker script will copy all contents into a new
// ESM module.

// Manage a mapping from 32-bit ids to actual JavaScript values.
class JSValManager {
  #lastk = 0;
  #kv = new Map();

  newJSVal(v) {
    const k = ++this.#lastk;
    this.#kv.set(k, v);
    return k;
  }

  // A separate has() call to ensure we can store undefined as a value
  // too. Also, unconditionally check this since the check is cheap
  // anyway, if the check fails then there's a use-after-free to be
  // fixed.
  getJSVal(k) {
    if (!this.#kv.has(k)) {
      throw new WebAssembly.RuntimeError(`getJSVal(${k})`);
    }
    return this.#kv.get(k);
  }

  // Check for double free as well.
  freeJSVal(k) {
    if (!this.#kv.delete(k)) {
      throw new WebAssembly.RuntimeError(`freeJSVal(${k})`);
    }
  }
}

// The actual setImmediate() to be used. This is a ESM module top
// level binding and doesn't pollute the globalThis namespace.
//
// To benchmark different setImmediate() implementations in the
// browser, use https://github.com/jphpsf/setImmediate-shim-demo as a
// starting point.
const setImmediate = (() => {
  // node, deno, bun, or other scripts might have set this up in the
  // browser
  if (globalThis.setImmediate) {
    return globalThis.setImmediate;
  }

  // https://developer.mozilla.org/en-US/docs/Web/API/Scheduler/postTask
  if (globalThis.scheduler) {
    return (cb, ...args) => scheduler.postTask(() => cb(...args));
  }

  // Cloudflare workers doesn't support MessageChannel
  if (globalThis.MessageChannel) {
    // A simple & fast setImmediate() implementation for browsers. It's
    // not a drop-in replacement for node.js setImmediate() because:
    // 1. There's no clearImmediate(), and setImmediate() doesn't return
    //    anything
    // 2. There's no guarantee that callbacks scheduled by setImmediate()
    //    are executed in the same order (in fact it's the opposite lol),
    //    but you are never supposed to rely on this assumption anyway
    class SetImmediate {
      #fs = [];
      #mc = new MessageChannel();

      constructor() {
        this.#mc.port1.addEventListener("message", () => {
          this.#fs.pop()();
        });
        this.#mc.port1.start();
      }

      setImmediate(cb, ...args) {
        this.#fs.push(() => cb(...args));
        this.#mc.port2.postMessage(undefined);
      }
    }

    const sm = new SetImmediate();
    return (cb, ...args) => sm.setImmediate(cb, ...args);
  }

  return (cb, ...args) => setTimeout(cb, 0, ...args);
})();

export default (__exports) => {
const __ghc_wasm_jsffi_jsval_manager = new JSValManager();
const __ghc_wasm_jsffi_finalization_registry = globalThis.FinalizationRegistry ? new FinalizationRegistry(sp => __exports.rts_freeStablePtr(sp)) : { register: () => {}, unregister: () => true };
return {
newJSVal: (v) => __ghc_wasm_jsffi_jsval_manager.newJSVal(v),
getJSVal: (k) => __ghc_wasm_jsffi_jsval_manager.getJSVal(k),
freeJSVal: (k) => __ghc_wasm_jsffi_jsval_manager.freeJSVal(k),
scheduleWork: () => setImmediate(__exports.rts_schedulerLoop),
ZC0ZCtrelliszm0zi1zi5zi0zminplacezmtrellisZCLiveziInZC: ($1,$2,$3,$4,$5) => {const subscribeFn = $1, bufPtr = $2, bufCap = $3, onNext = $4, onDone = $5; const obs = torc.shift((publish) => subscribeFn(publish)); const inner = obs.subscribe({ next: (value) => {   const bytes = new TextEncoder().encode(String(value));   const n = Math.min(bytes.length, bufCap);   new Uint8Array(__exports.memory.buffer, bufPtr, n).set(bytes.subarray(0, n));   onNext(n); } }); let done = false; return { finish: () => { if (done) return; done = true; inner.finish(); onDone(0); } };},
ZC2ZCtrelliszm0zi1zi5zi0zminplacezmtrellisZCLiveziInZC: ($1) => ((...args) => __exports.ghczuwasmzujsffiZC1ZCtrelliszm0zi1zi5zi0zminplacezmtrellisZCLiveziInZC($1, ...args)),
ZC3ZCtrelliszm0zi1zi5zi0zminplacezmtrellisZCLiveziInZC: ($1,$2) => ($1.subscribe({ next: $2 })),
ZC4ZCtrelliszm0zi1zi5zi0zminplacezmtrellisZCLiveziInZC: () => (torc.pure(1)),
ZC5ZCtrelliszm0zi1zi5zi0zminplacezmtrellisZCLiveziInZC: ($1,$2,$3,$4,$5,$6,$7) => {const url = new TextDecoder().decode(new Uint8Array(__exports.memory.buffer, $1, $2)); const bufPtr = $3, bufCap = $4, intervalMs = $5, onNext = $6, onDone = $7; const obs = torc.shift((publish) => {   const tick = () => {     fetch(url).then((r) => r.text()).then((text) => {       const bytes = new TextEncoder().encode(text);       const n = Math.min(bytes.length, bufCap);       new Uint8Array(__exports.memory.buffer, bufPtr, n).set(bytes.subarray(0, n));       publish(n);     }).catch(() => publish(-1));   };   tick();   const id = setInterval(tick, intervalMs);   return () => { clearInterval(id); onDone(0); }; }); return obs.subscribe({ next: onNext });},
ZC0ZCtrelliszm0zi1zi5zi0zminplacezmtrellisZCLiveziOutZC: ($1,$2,$3,$4) => {const row = $1, col = $2, ptr = $3, len = $4; const value = new TextDecoder().decode(new Uint8Array(__exports.memory.buffer, ptr, len)); if (window.__trellisOutCallback) window.__trellisOutCallback(row, col, value);},
ZC1ZCtrelliszm0zi1zi5zi0zminplaceZCTrellisziUIziScreenZC: ($1) => ($1.preventDefault()),
ZC2ZCtrelliszm0zi1zi5zi0zminplaceZCTrellisziUIziScreenZC: ($1) => (window.trellisHost.mouseMotion($1)),
ZC3ZCtrelliszm0zi1zi5zi0zminplaceZCTrellisziUIziScreenZC: ($1) => (window.trellisHost.mouseKey($1)),
ZC4ZCtrelliszm0zi1zi5zi0zminplaceZCTrellisziUIziScreenZC: ($1) => (window.trellisHost.mouseCellY($1)),
ZC5ZCtrelliszm0zi1zi5zi0zminplaceZCTrellisziUIziScreenZC: ($1) => (window.trellisHost.mouseCellX($1)),
ZC6ZCtrelliszm0zi1zi5zi0zminplaceZCTrellisziUIziScreenZC: ($1) => (window.trellisHost.mods($1)),
ZC7ZCtrelliszm0zi1zi5zi0zminplaceZCTrellisziUIziScreenZC: ($1) => (window.trellisHost.charCode($1)),
ZC8ZCtrelliszm0zi1zi5zi0zminplaceZCTrellisziUIziScreenZC: ($1) => (window.trellisHost.namedKey($1)),
ZC9ZCtrelliszm0zi1zi5zi0zminplaceZCTrellisziUIziScreenZC: ($1) => ((() => { const el = window.trellisHost.hiddenInputEl(); if (!el) return; let composing = false; el.addEventListener('compositionstart', () => { composing = true; }); el.addEventListener('compositionend', () => { composing = false; }); el.addEventListener('input', (e) => { if (composing) return; const text = el.value; el.value = ''; if (e.inputType && e.inputType.indexOf('delete') === 0) return; for (const ch of text) { $1(ch.codePointAt(0)); } if (window.__trellisTick) window.__trellisTick(); }); })()),
ZC10ZCtrelliszm0zi1zi5zi0zminplaceZCTrellisziUIziScreenZC: ($1) => ((() => { const el = window.trellisHost.hiddenInputEl(); if (!el) return; el.addEventListener('keydown', (e) => { $1(e); if (window.__trellisTick) window.__trellisTick(); }); })()),
ZC11ZCtrelliszm0zi1zi5zi0zminplaceZCTrellisziUIziScreenZC: ($1) => (window.trellisHost.canvasEl().addEventListener('wheel', (e) => { $1(e); if (window.__trellisTick) window.__trellisTick(); })),
ZC12ZCtrelliszm0zi1zi5zi0zminplaceZCTrellisziUIziScreenZC: ($1) => (window.trellisHost.canvasEl().addEventListener('mousemove', (e) => { if (e.buttons === 0) return; $1(e); if (window.__trellisTick) window.__trellisTick(); })),
ZC13ZCtrelliszm0zi1zi5zi0zminplaceZCTrellisziUIziScreenZC: ($1) => (window.trellisHost.canvasEl().addEventListener('mouseup', (e) => { $1(e); if (window.__trellisTick) window.__trellisTick(); })),
ZC14ZCtrelliszm0zi1zi5zi0zminplaceZCTrellisziUIziScreenZC: ($1) => (window.trellisHost.canvasEl().addEventListener('mousedown', (e) => { $1(e); if (window.__trellisTick) window.__trellisTick(); })),
ZC15ZCtrelliszm0zi1zi5zi0zminplaceZCTrellisziUIziScreenZC: ($1) => (window.trellisHost.canvasEl().addEventListener('keydown', (e) => { $1(e); if (window.__trellisTick) window.__trellisTick(); })),
ZC17ZCtrelliszm0zi1zi5zi0zminplaceZCTrellisziUIziScreenZC: ($1) => ((...args) => __exports.ghczuwasmzujsffiZC16ZCtrelliszm0zi1zi5zi0zminplaceZCTrellisziUIziScreenZC($1, ...args)),
ZC19ZCtrelliszm0zi1zi5zi0zminplaceZCTrellisziUIziScreenZC: ($1) => ((...args) => __exports.ghczuwasmzujsffiZC18ZCtrelliszm0zi1zi5zi0zminplaceZCTrellisziUIziScreenZC($1, ...args)),
ZC20ZCtrelliszm0zi1zi5zi0zminplaceZCTrellisziUIziScreenZC: ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11) => (window.trellisHost.drawCell($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11)),
ZC21ZCtrelliszm0zi1zi5zi0zminplaceZCTrellisziUIziScreenZC: ($1,$2,$3) => (window.trellisHost.clear($1,$2,$3)),
ZC22ZCtrelliszm0zi1zi5zi0zminplaceZCTrellisziUIziScreenZC: () => (window.trellisHost.rows()),
ZC23ZCtrelliszm0zi1zi5zi0zminplaceZCTrellisziUIziScreenZC: () => (window.trellisHost.cols()),
ZC0ZCtrelliszm0zi1zi5zi0zminplaceZCTrellisziOrcZC: ($1) => ($1.finish()),
ZC23ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: ($1,$2) => (__ghc_wasm_jsffi_finalization_registry.register($1, $2, $1)),
ZC0ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC: ($1) => (`${$1.stack ? $1.stack : $1}`),
ZC2ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC: ($1,$2,$3) => ((new TextEncoder()).encodeInto($1, new Uint8Array(__exports.memory.buffer, $2, $3)).written),
ZC3ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC: ($1) => ($1.length),
ZC4ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC: ($1) => {try { __ghc_wasm_jsffi_finalization_registry.unregister($1); } catch {}},
ZC18ZCghczminternalZCGHCziInternalziWasmziPrimziImportsZC: ($1,$2) => ($1.then(() => __exports.rts_promiseResolveUnit($2), err => __exports.rts_promiseReject($2, err))),
ZC0ZCghczminternalZCGHCziInternalziWasmziPrimziConcziInternalZC: async ($1) => (new Promise(res => setTimeout(res, $1 / 1000))),
};
};
