// Canvas/DOM host support for the wasm32-wasi Trellis.UI.Screen backend
// (src-wasi/Trellis/UI/Screen.hs). Holds all the mutable JS-side state
// (canvas 2d context, measured cell size) and the event-translation
// tables the Haskell side's `unsafe` JSFFI imports call into by name -
// kept here, not inline in Haskell source strings, so it's readable and
// so no string data ever needs to cross the FFI boundary (this GHC
// snapshot, 9.14.1.20260731, can't marshal JSString at all - see
// wasm/spike/Tick.hs's own note on the same bug). Every Haskell-facing
// function here takes/returns only numbers.
//
// A factory, not a singleton: <trellis-sheet> (wasm/trellis-sheet.js)
// needs one independent host per instance, each owning its own canvas,
// so two sheets on one page don't draw into or read cell-geometry from
// each other. The Haskell side's JSFFI imports always call through
// `window.trellisHost` though - the trick trellis-sheet.js uses to make
// that safe with multiple instances is swapping *what* window.trellisHost
// points to immediately before calling into a given instance's wasm
// export, since JS is single-threaded and every such call is synchronous
// from that point until it returns (see that file's own comment).

// Backend.hs's own numeric encoding - kept in exact sync by hand, both
// sides documented as mirroring one another.
const KEY = {
  ArrowUp: 1, ArrowDown: 2, ArrowLeft: 3, ArrowRight: 4,
  CtrlEnter: 40, CtrlEsc: 41, CtrlTab: 42, BackTab: 43,
  Delete: 50, Home: 51, End: 52, PgUp: 53, PgDn: 54, F2: 55, Space: 56,
  Backspace: 60,
  MouseLeft: 70, MouseRight: 71, MouseMiddle: 72, MouseRelease: 73,
  MouseWheelUp: 74, MouseWheelDown: 75,
};
const MOD = { Alt: 1, Ctrl: 2, Shift: 4, Motion: 8 };

function ctrlLetterCode(key) {
  if (key.length !== 1) return 0;
  const c = key.toLowerCase().charCodeAt(0);
  if (c >= 97 && c <= 122) return 10 + (c - 97); // a..z -> keyCtrlA..Z (10..35)
  return 0;
}

// Creates one independent host - its own canvas 2d context and measured
// cell size - bound to `canvas`. `canvas` should already be in the DOM
// (e.g. in a <trellis-sheet>'s shadow root) with real layout dimensions
// before this is called, since cell size is measured once here, not
// re-measured on resize (a known simplification, not a bug - resizing
// after the fact isn't supported yet).
//
// `hiddenInput`, if given, is a real (invisible) <input> element used
// solely to summon a mobile on-screen keyboard - a bare <canvas> can
// hold keyboard focus and receive real keydown events just fine on
// desktop, but mobile browsers only ever show their virtual keyboard
// for a genuine text-input-capable element, no matter how the canvas's
// own focus/keydown handling is set up (see wasm/trellis-sheet.js's own
// note on the tap-to-focus wiring). Optional and defaults to null -
// wasm/main.mjs's bare-page demo doesn't have one, and every function
// below that touches it is written to no-op gracefully when it's null.
function createTrellisHost(canvas, fontPx, hiddenInput = null) {
  const ctx = canvas.getContext("2d");
  ctx.font = `${fontPx}px monospace`;
  // A monospace font's own advance width is the same for any ASCII
  // char - measure once, reuse for every cell.
  const cellW = Math.max(1, Math.round(ctx.measureText("M").width));
  const cellH = Math.max(1, Math.round(fontPx * 1.3));
  ctx.textBaseline = "top";

  return {
    canvasEl() {
      return canvas;
    },
    hiddenInputEl() {
      return hiddenInput;
    },

    cols() {
      return Math.max(1, Math.floor(canvas.clientWidth / cellW));
    },
    rows() {
      return Math.max(1, Math.floor(canvas.clientHeight / cellH));
    },

    clear(r, g, b) {
      ctx.fillStyle = `rgb(${r},${g},${b})`;
      ctx.fillRect(0, 0, canvas.width, canvas.height);
    },

    // One glyph cell: background fill, then the glyph itself (a single
    // Unicode codepoint, passed as a plain Int - never a JS string on
    // the Haskell side).
    drawCell(x, y, codepoint, bold, underline, fgR, fgG, fgB, bgR, bgG, bgB) {
      const px = x * cellW;
      const py = y * cellH;
      ctx.fillStyle = `rgb(${bgR},${bgG},${bgB})`;
      ctx.fillRect(px, py, cellW, cellH);
      if (codepoint === 0) return;
      ctx.font = `${bold ? "bold " : ""}${fontPx}px monospace`;
      ctx.fillStyle = `rgb(${fgR},${fgG},${fgB})`;
      ctx.fillText(String.fromCodePoint(codepoint), px, py);
      if (underline) {
        ctx.fillRect(px, py + cellH - 2, cellW, 1);
      }
    },

    // ---- input translation: DOM event -> plain Ints only ----

    // 0 if this key isn't in trellis's named-key vocabulary at all -
    // matches src-native/Trellis/UI/Screen.hs's toBackendKey's own
    // "unnamed becomes keyNone, inert" contract.
    namedKey(e) {
      if (e.ctrlKey) {
        const c = ctrlLetterCode(e.key);
        if (c) return c;
      }
      switch (e.key) {
        case "ArrowUp": return KEY.ArrowUp;
        case "ArrowDown": return KEY.ArrowDown;
        case "ArrowLeft": return KEY.ArrowLeft;
        case "ArrowRight": return KEY.ArrowRight;
        case "Enter": return e.ctrlKey ? KEY.CtrlEnter : 0; // plain Enter -> charCode 13 instead
        case "Escape": return KEY.CtrlEsc;
        case "Tab": return e.shiftKey ? KEY.BackTab : KEY.CtrlTab;
        case "Delete": return KEY.Delete;
        case "Home": return KEY.Home;
        case "End": return KEY.End;
        case "PageUp": return KEY.PgUp;
        case "PageDown": return KEY.PgDn;
        case "F2": return KEY.F2;
        case "Backspace": return KEY.Backspace;
        default: return 0;
      }
    },

    // Unicode codepoint of a plain typed character (already shift-adjusted
    // by the browser), 0 if this key isn't a single printable character.
    // Plain Enter deliberately reports as charCode 13 here (matching
    // termbox2's own convention, and Update.Core's proven ch==13 check)
    // rather than through namedKey.
    charCode(e) {
      if (e.key === "Enter" && !e.ctrlKey) return 13;
      if (e.ctrlKey) return 0; // a ctrl combo is never also a printable char
      if (e.key.length === 1) return e.key.codePointAt(0);
      return 0;
    },

    mods(e) {
      let m = 0;
      if (e.altKey) m |= MOD.Alt;
      if (e.ctrlKey || e.metaKey) m |= MOD.Ctrl; // metaKey folded into ctrl for macOS-hosted browsers
      if (e.shiftKey) m |= MOD.Shift;
      return m;
    },

    mouseCellX(e) {
      const rect = canvas.getBoundingClientRect();
      return Math.floor((e.clientX - rect.left) / cellW);
    },
    mouseCellY(e) {
      const rect = canvas.getBoundingClientRect();
      return Math.floor((e.clientY - rect.top) / cellH);
    },
    // 0 (matching namedKey's own "unnamed becomes keyNone, inert"
    // contract) for a bare hover-move with no button held - e.button is
    // only meaningful for button-press-related events (mousedown,
    // click, etc.); for mousemove it's always 0 regardless of whether
    // any button is actually down, so without this check every hover
    // over the canvas fell through to the switch below and was
    // indistinguishable from a genuine left-click-press at that cell.
    mouseKey(e) {
      if (e.type === "wheel") return e.deltaY < 0 ? KEY.MouseWheelUp : KEY.MouseWheelDown;
      if (e.type === "mouseup") return KEY.MouseRelease;
      if (e.type === "mousemove" && e.buttons === 0) return 0;
      switch (e.button) {
        case 0: return KEY.MouseLeft;
        case 1: return KEY.MouseMiddle;
        case 2: return KEY.MouseRight;
        default: return KEY.MouseLeft;
      }
    },
    mouseMotion(e) {
      return e.type === "mousemove" && e.buttons !== 0 ? MOD.Motion : 0;
    },
  };
}

export default createTrellisHost;
