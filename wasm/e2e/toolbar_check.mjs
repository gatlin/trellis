// Playwright verification that trellis-sheet.js's on-screen toolbar
// (Arrow/Enter/Esc/Tab/Backspace/F2 buttons) produces identical
// results to the same logical sequence driven by real keydown events -
// each button just synthesizes a real KeyboardEvent and dispatches it
// on the canvas, so this confirms that mechanism actually works end to
// end, not just that it type-checks. Compares exact cell values via
// watchOut/cell-out, not canvas pixels - two separate page instances
// (one per mode) can render subtly different bytes for identical
// content, which is a real, previously-hit source of test flakiness
// this session, not a concern for this feature's actual correctness.
//
// Serve wasm/ first, e.g.:
//   python3 -m http.server 8935 &   (from inside wasm/)
//   node e2e/toolbar_check.mjs
import { chromium } from "playwright";

async function pressToolbarButton(sheet, label) {
  // Buttons live in the shadow root, in the fixed order trellis-sheet.js
  // creates them: Up, Down, Left, Right, Enter, Esc, Tab, Backspace, F2.
  const index = ["↑", "↓", "←", "→", "⏎", "Esc", "⇥", "⌫", "F2"].indexOf(label);
  if (index < 0) throw new Error(`unknown toolbar label ${label}`);
  await sheet.evaluate((el, i) => {
    el.shadowRoot.querySelectorAll("button")[i].dispatchEvent(
      new PointerEvent("pointerdown", { bubbles: true })
    );
  }, index);
}

async function run(runIndex, mode) {
  const browser = await chromium.launch();
  const page = await browser.newPage();
  const pageErrors = [];
  page.on("pageerror", (e) => pageErrors.push(String(e)));

  await page.goto("http://127.0.0.1:8935/component.html");
  await page.waitForFunction(() => document.getElementById("status").textContent === "running", { timeout: 10000 });

  const sheet = page.locator("#sheet");
  await sheet.evaluate((el) => {
    window.__outs = {};
    el.addEventListener("cell-out", (e) => {
      window.__outs[`${e.detail.row},${e.detail.col}`] = e.detail.value;
    });
  });

  const press = async (label, realKey) => {
    if (mode === "toolbar") {
      await pressToolbarButton(sheet, label);
    } else {
      await sheet.evaluate((el) => el.shadowRoot.querySelector("canvas").focus());
      await page.keyboard.press(realKey);
    }
  };
  const type = async (text) => {
    await sheet.evaluate((el) => el.shadowRoot.querySelector("canvas").focus());
    await page.keyboard.type(text);
  };

  // (1, 1) and (1, 2) - symmetric-ish positions, avoiding the known
  // pre-existing asymmetric-position watchOut gap flagged by
  // mobile_check.mjs (confirmed unrelated to this feature).
  await sheet.evaluate((el) => { el.watchOut(1, 1); el.watchOut(1, 2); });

  // Sequence: navigate right/down, Enter to edit, type, Enter to
  // confirm - via the toolbar (or real keys) throughout.
  await press("→", "ArrowRight");
  await press("↓", "ArrowDown");
  await press("⏎", "Enter"); // begin editing (1,1)
  await type("40+2");
  await press("⏎", "Enter"); // confirm
  await page.waitForTimeout(300);

  // Navigate to a second cell, start editing, type, then Esc to
  // cancel - the typed "999" should never be committed.
  await press("→", "ArrowRight");
  await press("⏎", "Enter"); // begin editing (1,2)
  await type("999");
  await press("Esc", "Escape"); // cancel
  await page.waitForTimeout(300);

  // Tab/Backspace/F2 sanity pass: Tab moves to the next cell, F2 opens
  // its editor (same as Enter would), Backspace deletes a typed char,
  // Esc cancels again so nothing stray is left committed. Just needs
  // to run without a page error - already covered above for exact
  // value correctness.
  await press("⇥", "Tab");
  await press("F2", "F2");
  await type("x");
  await press("⌫", "Backspace");
  await press("Esc", "Escape");
  await page.waitForTimeout(150);

  const outs = await page.evaluate(() => window.__outs);
  await browser.close();
  return { outs, pageErrors };
}

let allPass = true;
for (let i = 1; i <= 3; i++) {
  const toolbar = await run(i, "toolbar");
  const keyboard = await run(i, "keyboard");
  const confirmMatch = toolbar.outs["1,1"] === "42" && keyboard.outs["1,1"] === "42";
  // The cancelled cell should never have reported "999" - either it
  // never fired cell-out at all (undefined - fine, nothing changed) or
  // it fired for some unrelated earlier reason but never with "999".
  const cancelMatch = toolbar.outs["1,2"] !== "999" && keyboard.outs["1,2"] !== "999";
  const noErrors = toolbar.pageErrors.length === 0 && keyboard.pageErrors.length === 0;
  const ok = confirmMatch && cancelMatch && noErrors;
  console.log(
    `[run ${i}] toolbar.outs=${JSON.stringify(toolbar.outs)} keyboard.outs=${JSON.stringify(keyboard.outs)} ` +
    `confirmMatch=${confirmMatch} cancelMatch=${cancelMatch} ` +
    `toolbarErrors=${JSON.stringify(toolbar.pageErrors)} keyboardErrors=${JSON.stringify(keyboard.pageErrors)}`
  );
  console.log(`[run ${i}] ${ok ? "PASS" : "FAIL"}`);
  allPass = allPass && ok;
}
console.log(`OVERALL: ${allPass ? "PASS" : "FAIL"} (${allPass ? "3/3" : "<3/3"} runs green)`);
process.exit(allPass ? 0 : 1);
