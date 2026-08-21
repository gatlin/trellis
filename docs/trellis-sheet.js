// <trellis-sheet>: packages the wasm/canvas Trellis backend (see
// wasm/index.html/main.mjs for the original bare-page bring-up this is
// based on - left untouched, since the existing e2e suite already
// verifies it and there's no reason to risk that working, tested path)
// as a Web Component. Public API is platform-standard - DOM
// CustomEvents out, duck-typed plain callbacks in - torc (the reactive-
// streams library actually driving live cells under the hood) never
// appears in it, so nothing embedding this needs to know or care it
// exists.
//
// Each instance does its own independent WebAssembly.instantiate, which
// gives it its own independent linear memory and thus independent
// Haskell-side state (Trellis.UI.Screen's stepRef/eventQueueRef,
// app-wasi/Main.hs's own root/mailbox/outsRef, etc.) - multiple
// <trellis-sheet>s on one page don't share any Haskell state. What they
// DO share is a handful of `window.*` globals the compiled wasm
// module's JSFFI code references by fixed name (window.trellisHost,
// window.__trellisTick, window.__trellisOutCallback - see
// src-wasi/Trellis/UI/Screen.hs and app-wasi/Live/Out.hs) - `_activate()`
// below is what makes that safe with multiple instances: it repoints
// those globals at *this* instance's own state immediately before any
// call into this instance's wasm exports. JS is single-threaded and
// every such call is synchronous from the caller's point of view until
// it returns (this whole design deliberately never uses an async `safe`
// JSFFI import - see the plan file's "Event loop" note), so there's no
// window where another instance's activation could interleave and
// corrupt this one's call.
//
// This copy lives under docs/ (a static, self-contained mirror of
// wasm/trellis-sheet.js) specifically so GitHub Pages - which only
// serves committed files, no npm install/build step - can host a live
// demo straight from this directory. Only the import/fetch paths below
// differ from wasm/trellis-sheet.js (./dist -> ./app, since "dist" is
// gitignored repo-wide and ./node_modules -> ./vendor/browser_wasi_shim,
// since node_modules is never committed); keep both copies in sync by
// hand if the component itself changes.

import { WASI, File, OpenFile, ConsoleStdout, PreopenDirectory } from "./vendor/browser_wasi_shim/index.js";
import ghc_wasm_jsffi from "./app/trellis.js";
import createTrellisHost from "./trellis-host.mjs";
import * as torc from "./vendor/torc.js";

const TICK_INTERVAL_MS = 250;

class TrellisSheet extends HTMLElement {
  constructor() {
    super();
    this._host = null;
    this._exports = null;
    this._heartbeat = null;
    this._ready = null; // Promise, resolves once trellisSetup has run
  }

  connectedCallback() {
    if (this._ready) return; // already connected once - see disconnectedCallback
    const shadow = this.attachShadow({ mode: "open" });
    const canvas = document.createElement("canvas");
    canvas.width = this.hasAttribute("width") ? Number(this.getAttribute("width")) : 1000;
    canvas.height = this.hasAttribute("height") ? Number(this.getAttribute("height")) : 600;
    canvas.tabIndex = 0; // focusable - a real keydown only reaches an element that can hold focus
    canvas.style.outline = "none";
    shadow.appendChild(canvas);
    this._canvas = canvas;

    this._host = createTrellisHost(canvas, 14);

    // Registered before _instantiate() below (which is what triggers
    // Haskell's own listener registration, via
    // Trellis.UI.Screen.registerListeners, on this same canvas) - DOM
    // listeners on the same element/event fire in registration order,
    // so this always runs first, and the globals are correctly pointed
    // at *this* instance by the time Haskell's own listener (and
    // whatever it calls) runs.
    for (const type of ["keydown", "mousedown", "mouseup", "mousemove", "wheel"]) {
      canvas.addEventListener(type, () => this._activate());
    }

    this._ready = this._instantiate();
  }

  disconnectedCallback() {
    if (this._heartbeat) clearInterval(this._heartbeat);
    this._heartbeat = null;
    // Full re-instantiation on reconnect is an acceptable simple answer
    // for now - there's no shutdown export in the reactor-mode design
    // to hook a real teardown into (see app-wasi/Main.hs's own note on
    // why), and repeated connect/disconnect isn't an expected usage
    // pattern here yet.
    this._ready = null;
    this._exports = null;
  }

  // Repoints the globals the compiled wasm module's JSFFI code
  // references by fixed name at this instance's own state - call this
  // immediately before any call into this._exports. Safe to call
  // repeatedly/early (e.g. before this._exports exists yet) - the
  // closures below only actually read this._exports when invoked, not
  // when defined.
  _activate() {
    window.trellisHost = this._host;
    window.__trellisTick = () => this._exports && this._exports.trellisTick();
    window.__trellisOutCallback = (row, col, value) => {
      this.dispatchEvent(new CustomEvent("cell-out", { detail: { row, col, value } }));
    };
  }

  async _instantiate() {
    this._activate();
    window.torc = torc; // shared across instances - torc itself holds no per-sheet state

    const fds = [
      new OpenFile(new File([])),
      ConsoleStdout.lineBuffered((msg) => console.log(`[trellis-sheet stdout] ${msg}`)),
      ConsoleStdout.lineBuffered((msg) => console.error(`[trellis-sheet stderr] ${msg}`)),
      new PreopenDirectory(".", []),
    ];
    const wasi = new WASI(["trellis.wasm"], [], fds);

    let exportsObj = {};
    const resp = await fetch("./app/trellis.wasm");
    const wasmBuf = await resp.arrayBuffer();
    const { instance } = await WebAssembly.instantiate(wasmBuf, {
      ghc_wasm_jsffi: ghc_wasm_jsffi(exportsObj),
      wasi_snapshot_preview1: wasi.wasiImport,
    });
    Object.assign(exportsObj, instance.exports);
    this._exports = exportsObj;

    wasi.initialize(instance);

    this._activate();
    await this._exports.trellisSetup();

    this._heartbeat = setInterval(() => {
      this._activate();
      this._exports.trellisTick();
    }, TICK_INTERVAL_MS);

    this.dispatchEvent(new CustomEvent("ready"));
  }

  async _whenReady() {
    if (!this._ready) throw new Error("<trellis-sheet> not connected");
    await this._ready;
  }

  /**
   * Binds cell (row, col) to an external live source. `subscribeFn` is
   * accepted duck-typed: a plain function `(next) => cleanupFn`,
   * matching torc's own `shift` callback shape exactly (see
   * app-wasi/Live/In.hs's declareExternalSubscription) - any
   * Observable-shaped library (RxJS, torc, a hand-rolled callback
   * registrar) adapts with at most a one-line wrapper, e.g. for an
   * RxJS Observable: `sheet.bindIn(r, c, next => { const sub =
   * obs.subscribe(next); return () => sub.unsubscribe(); })`. Every
   * published value is coerced to text via String(value) before it
   * becomes the cell's content, same as any other live cell.
   * Re-binding the same cell replaces whatever was bound there before.
   */
  async bindIn(row, col, subscribeFn) {
    await this._whenReady();
    this._activate();
    this._exports.trellisBindIn(row, col, subscribeFn);
  }

  /** Drops whatever bindIn bound to (row, col), if anything. */
  async unbindIn(row, col) {
    await this._whenReady();
    this._activate();
    this._exports.trellisUnbindIn(row, col);
  }

  /**
   * Watches cell (row, col): from now on, a `cell-out` CustomEvent
   * (`detail: {row, col, value}`) fires on this element whenever that
   * cell's value actually changes (real change-detection against the
   * previous value, not fired on every tick regardless).
   */
  async watchOut(row, col) {
    await this._whenReady();
    this._activate();
    this._exports.trellisWatchOut(row, col);
  }
}

customElements.define("trellis-sheet", TrellisSheet);

export default TrellisSheet;
