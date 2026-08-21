// Playwright verification of two things registerHiddenInputText's
// prevValue-diffing (src-wasi/Trellis/UI/Screen.hs) needs to get right
// that mobile_check.mjs's shorter sequences don't exercise:
//
// 1. A typing session long enough to cross RESET_THRESHOLD (200 chars)
//    mid-session - confirms the occasional full reset doesn't drop or
//    duplicate any characters across that boundary.
// 2. Backspacing mid-string, then continuing to type - confirms the
//    isDelete branch correctly resyncs prevValue to the (now shorter)
//    real value, so the next insertion's diff starts from the right
//    baseline rather than the pre-backspace one.
//
// Same honest ceiling as mobile_check.mjs: Playwright's keyboard
// simulation dispatches genuine keydown/input sequences (confirmed
// there to actually flow through the input-event path, not just
// keydown), but doesn't drive a real device's virtual keyboard/IME.
//
// Serve wasm/ first, e.g.:
//   python3 -m http.server 8935 &   (from inside wasm/)
//   node e2e/mobile_reset_check.mjs
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
  const context = await browser.newContext({ ...devices["Pixel 7"] });
  const page = await context.newPage();
  const pageErrors = [];
  page.on("pageerror", (err) => pageErrors.push(String(err)));

  await page.route("**/mobile-reset-test", (route) =>
    route.fulfill({ contentType: "text/html", body: PAGE })
  );
  await page.goto("http://127.0.0.1:8935/mobile-reset-test");
  await page.evaluate(() => window.__ready);
  await page.waitForTimeout(200);

  const sheet = page.locator("#sheet");
  const box = await sheet.boundingBox();
  await page.touchscreen.tap(box.x + box.width / 2, box.y + box.height / 2);
  await page.waitForTimeout(50);
  await sheet.evaluate((el) => {
    el.addEventListener("cell-out", (e) => { window.__lastOut = e.detail.value; });
  });

  // --- 1. A long string literal, well past RESET_THRESHOLD (200) ---
  // A repeating-but-position-identifiable pattern (not just "aaaa...")
  // so a dropped/duplicated/reordered character anywhere would be
  // caught by an exact string match, not masked by repetition.
  const longBody = "0123456789".repeat(25); // 250 chars
  await sheet.evaluate((el) => el.watchOut(0, 0));
  await page.keyboard.press("Enter"); // begin editing (0,0)
  await page.keyboard.type(`"${longBody}"`); // typed WITH quotes (string-literal syntax)...
  await page.keyboard.press("Enter"); // confirm
  await page.waitForTimeout(400);
  const longOut = await page.evaluate(() => window.__lastOut);
  const longOk = longOut === longBody; // ...but reported back unquoted, like any string value

  // --- 2. Backspace mid-string, then keep typing ---
  // Types "hello xxx", backspaces the "xxx", types "world" - a real
  // mobile-keyboard-shaped correction, not just append-only typing.
  // (1, 1) - symmetric, like mobile_check.mjs's own positions, to
  // sidestep the pre-existing asymmetric-position watchOut gap noted
  // there rather than accidentally re-triggering it here.
  await sheet.evaluate((el) => el.watchOut(1, 1));
  await page.keyboard.press("ArrowRight");
  await page.keyboard.press("ArrowDown"); // (0,0) -> (1,1)
  await page.keyboard.press("Enter"); // begin editing (1,1)
  await page.keyboard.type('"hello xxx');
  await page.keyboard.press("Backspace");
  await page.keyboard.press("Backspace");
  await page.keyboard.press("Backspace");
  await page.keyboard.type('world"');
  await page.keyboard.press("Enter"); // confirm
  await page.waitForTimeout(400);
  const backspaceOut = await page.evaluate(() => window.__lastOut);
  const backspaceOk = backspaceOut === "hello world"; // unquoted, same reasoning as above

  await browser.close();

  console.log(
    `[run ${runIndex}] longOut.length=${longOut ? longOut.length : 0} longOk=${longOk} ` +
    `backspaceOut=${JSON.stringify(backspaceOut)} backspaceOk=${backspaceOk} pageErrors=${JSON.stringify(pageErrors)}`
  );
  return longOk && backspaceOk && pageErrors.length === 0;
}

let allPass = true;
for (let i = 1; i <= 3; i++) {
  const ok = await run(i);
  console.log(`[run ${i}] ${ok ? "PASS" : "FAIL"}`);
  allPass = allPass && ok;
}
console.log(`OVERALL: ${allPass ? "PASS" : "FAIL"} (${allPass ? "3/3" : "<3/3"} runs green)`);
process.exit(allPass ? 0 : 1);
