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
    // Defaults to filling the viewport when the host page doesn't ask
    // for a specific size - explicit width/height attributes still win,
    // so existing fixed-size embeddings (e2e tests included, which rely
    // on deterministic pixel dimensions) are unaffected.
    const canvas = document.createElement("canvas");
    canvas.width = this.hasAttribute("width") ? Number(this.getAttribute("width")) : window.innerWidth;
    canvas.height = this.hasAttribute("height") ? Number(this.getAttribute("height")) : window.innerHeight;
    canvas.tabIndex = 0; // focusable - a real keydown only reaches an element that can hold focus
    canvas.style.outline = "none";
    shadow.appendChild(canvas);
    this._canvas = canvas;

    // A real (invisible) <input>, solely to summon a mobile on-screen
    // keyboard - a bare <canvas> can hold keyboard focus and receive
    // real keydown events just fine on desktop, but mobile browsers
    // only ever show their virtual keyboard for a genuine text-input-
    // capable element, full stop, regardless of the canvas's own
    // focus/keydown handling. opacity:0 (not display:none/
    // visibility:hidden, both of which make an element unfocusable) +
    // 1px size keeps it out of the way visually; font-size:16px is a
    // deliberate, well-known iOS Safari workaround - a focused input
    // with a *smaller* font size makes Safari auto-zoom the page when
    // its keyboard appears, which this avoids entirely.
    const hiddenInput = document.createElement("input");
    hiddenInput.type = "text";
    hiddenInput.autocomplete = "off";
    hiddenInput.autocapitalize = "off";
    hiddenInput.setAttribute("autocorrect", "off");
    hiddenInput.spellcheck = false;
    hiddenInput.style.position = "absolute";
    hiddenInput.style.opacity = "0";
    hiddenInput.style.width = "1px";
    hiddenInput.style.height = "1px";
    hiddenInput.style.fontSize = "16px";
    hiddenInput.style.pointerEvents = "none"; // never steal a tap meant for the canvas underneath
    shadow.appendChild(hiddenInput);
    this._hiddenInput = hiddenInput;

    this._host = createTrellisHost(canvas, 14, hiddenInput);

    // Registered before _instantiate() below (which is what triggers
    // Haskell's own listener registration, via
    // Trellis.UI.Screen.registerListeners, on this same canvas/input) -
    // DOM listeners on the same element/event fire in registration
    // order, so this always runs first, and the globals are correctly
    // pointed at *this* instance by the time Haskell's own listener
    // (and whatever it calls) runs.
    for (const type of ["keydown", "mousedown", "mouseup", "mousemove", "wheel"]) {
      canvas.addEventListener(type, () => this._activate());
    }
    for (const type of ["keydown", "input", "compositionstart", "compositionend"]) {
      hiddenInput.addEventListener(type, () => this._activate());
    }

    // Tapping/clicking the canvas moves keyboard focus onto the hidden
    // input instead - synchronously, within the same event handler, not
    // e.g. a microtask or timeout later. That's not just tidiness: iOS
    // Safari only honours a programmatic .focus() call as "summon the
    // keyboard" if it happens synchronously inside a real user gesture's
    // own call stack. pointerdown (not click) is used specifically
    // because it's the unified event covering mouse, touch and pen
    // alike, and because it still fires before the browser's own
    // default focus-handling for the tap has settled.
    // preventDefault matters here beyond convention: without it, the
    // browser's own default "focus the tapped/clicked element" handling
    // (canvas has tabIndex=0, so it's a valid focus target) runs *after*
    // this handler and silently steals focus back from the hidden input
    // - confirmed empirically, not a defensive guess.
    canvas.addEventListener("pointerdown", (e) => {
      e.preventDefault();
      hiddenInput.focus();
    });

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
