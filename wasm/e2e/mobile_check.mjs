// Playwright verification of the hidden-input mobile-keyboard-summon
// mechanism (wasm/trellis-sheet.js's tap-to-focus wiring,
// src-wasi/Trellis/UI/Screen.hs's registerHiddenInput{Keydown,Text}).
//
// Honest ceiling of what this actually proves: Playwright does not
// drive a real iOS/Android virtual keyboard, so nothing here confirms
// the OS actually pops the keyboard up, or exercises real-device
// quirks (autocomplete UI timing, exact IME behaviour per platform,
// Safari's specific synchronous-focus-in-gesture requirement beyond
// "does calling .focus() synchronously inside the pointerdown handler
// work at all in this browser engine"). What IS proven for real: (1)
// tapping the canvas moves real keyboard focus onto the hidden input,
// not just that some function was called: (2) once focused there,
// Playwright's own keyboard simulation - which dispatches genuine
// keydown/input/keyup sequences to whatever element has real focus,
// same as a physical keyboard would - correctly drives navigation and
// text entry through the new dual keydown+input path with no
// double-insertion; (3) synthetic IME composition events (which
// Playwright's high-level keyboard API doesn't simulate on its own,
// dispatched directly here instead) are correctly gated by the
// composing flag - not inserted early, not inserted twice.
//
// Serve wasm/ first, e.g.:
//   python3 -m http.server 8935 &   (from inside wasm/)
//   node e2e/mobile_check.mjs
import { chromium, devices } from "playwright";

const PAGE = `<!doctype html>
<html><head><style>trellis-sheet { display: block; }</style></head><body>
<trellis-sheet id="sheet" width="800" height="400"></trellis-sheet>
<script type="module">
  import "./trellis-sheet.js";
  window.__ready = new Promise((r) => document.getElementById("sheet").addEventListener("ready", r));
</script>
</body></html>`;

async function run(runIndex) {
  const browser = await chromium.launch();
  // A real mobile emulation profile - touch enabled, mobile UA - not
  // load-bearing for the mechanism itself (which only cares about real
  // DOM events, not the UA string), but the honest context to test
  // this feature under.
  const context = await browser.newContext({ ...devices["Pixel 7"] });
  const page = await context.newPage();
  const pageErrors = [];
  page.on("pageerror", (err) => pageErrors.push(String(err)));

  await page.route("**/mobile-test", (route) =>
    route.fulfill({ contentType: "text/html", body: PAGE })
  );
  await page.goto("http://127.0.0.1:8935/mobile-test");
  await page.evaluate(() => window.__ready);
  await page.waitForTimeout(200);

  const sheet = page.locator("#sheet");
  const box = await sheet.boundingBox();

  // --- 1. Tapping the canvas moves real focus onto the hidden input ---
  await page.touchscreen.tap(box.x + box.width / 2, box.y + box.height / 2);
  await page.waitForTimeout(50);
  const focusedIsHiddenInput = await sheet.evaluate((el) => {
    const active = el.shadowRoot.activeElement;
    return active && active.tagName === "INPUT";
  });

  // --- 2. Real navigation + typing, driven by Playwright's keyboard
  // API against whatever now has real focus (the hidden input, per
  // step 1) - proves keydown-for-navigation and input-events-for-text
  // work together, and specifically that no character gets inserted
  // twice (the exact risk the keydown/input split was designed to
  // avoid - a bug here would show up as e.g. "4402" instead of "42").
  let editOut = null;
  await sheet.evaluate((el) => {
    el.addEventListener("cell-out", (e) => { window.__lastOut = e.detail.value; });
  });
  // (1, 1) - deliberately a *symmetric* position (row === col). While
  // building this feature a pre-existing, position-dependent
  // watchOut/cell-out reporting gap turned up for asymmetric positions
  // (e.g. (3, 2), (1, 0)) - confirmed to reproduce identically via the
  // plain canvas path with zero involvement of anything built here
  // (isolated by hand, outside this file), so it's an existing bug,
  // not something introduced here. Out of scope for this directive
  // ("tapping a cell brings up a keyboard and typing/backspace/enter
  // work"); flagged in the report instead of chased.
  await sheet.evaluate((el) => el.watchOut(1, 1));
  await page.keyboard.press("ArrowRight");
  await page.keyboard.press("ArrowDown");
  await page.keyboard.press("Enter"); // begin editing (named key, via hidden input's keydown path)
  await page.keyboard.type("40+2");   // printable chars, via hidden input's input-event path
  await page.keyboard.press("Enter"); // confirm
  await page.waitForTimeout(400); // cell-out only fires on the next heartbeat, not same-frame
  editOut = await page.evaluate(() => window.__lastOut);

  // --- 3. IME composition: Playwright's keyboard API doesn't simulate
  // this, so dispatch the real event sequence directly. An input event
  // *during* composition must be ignored (would otherwise insert
  // not-yet-confirmed text); the final input event after
  // compositionend must land exactly once.
  await sheet.evaluate((el) => el.watchOut(2, 2));
  await page.keyboard.press("ArrowRight");
  await page.keyboard.press("ArrowDown");
  await page.keyboard.press("Enter"); // begin editing cell (2,2), from (1,1)
  const hiddenInput = sheet.locator("input");
  await hiddenInput.dispatchEvent("compositionstart");
  await hiddenInput.evaluate((el) => { el.value = "n"; }); // mid-composition, not yet confirmed
  await hiddenInput.dispatchEvent("input", { inputType: "insertCompositionText" });
  await hiddenInput.evaluate((el) => { el.value = "42"; }); // IME resolves to a final value
  await hiddenInput.dispatchEvent("compositionend");
  await hiddenInput.dispatchEvent("input", { inputType: "insertCompositionText" }); // fires after compositionend, per spec
  await page.keyboard.press("Enter"); // confirm
  await page.waitForTimeout(400);
  const imeOut = await page.evaluate(() => window.__lastOut);

  await browser.close();

  const bindOk = focusedIsHiddenInput === true;
  const editOk = editOut === "42";
  const imeOk = imeOut === "42";
  console.log(
    `[run ${runIndex}] focusedIsHiddenInput=${focusedIsHiddenInput} editOut=${JSON.stringify(editOut)} imeOut=${JSON.stringify(imeOut)}`,
    `bindOk=${bindOk} editOk=${editOk} imeOk=${imeOk} pageErrors=${JSON.stringify(pageErrors)}`
  );
  return bindOk && editOk && imeOk && pageErrors.length === 0;
}

let allPass = true;
for (let i = 1; i <= 3; i++) {
  const ok = await run(i);
  console.log(`[run ${i}] ${ok ? "PASS" : "FAIL"}`);
  allPass = allPass && ok;
}
console.log(`OVERALL: ${allPass ? "PASS" : "FAIL"} (${allPass ? 3 : "<3"}/3 runs green)`);
process.exit(allPass ? 0 : 1);
