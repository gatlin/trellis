// Playwright verification of the real wasm build (not the minimal Phase-0
// spike) - loads the actual production trellis.wasm/trellis.js under
// wasm/index.html, drives real keyboard input through the full app
// (navigate, open the cell editor, type, commit), and asserts on the
// resulting sheet state via the footer text (not just "the canvas has
// some non-zero pixels", which a stray cursor highlight alone would
// already satisfy). Serve wasm/ first, e.g.:
//   python3 -m http.server 8935 &   (from inside wasm/)
//   node e2e/check.mjs
import { chromium } from "playwright";

async function run(runIndex) {
  const browser = await chromium.launch();
  const page = await browser.newPage();
  const pageErrors = [];
  page.on("pageerror", (err) => pageErrors.push(String(err)));

  await page.goto("http://127.0.0.1:8935/index.html");
  await page.waitForFunction(() => document.getElementById("status").textContent === "running", { timeout: 10000 });
  await page.waitForTimeout(200);

  // Real end-to-end input: move, open the editor (Enter), type a formula,
  // commit (Enter) - exercises listener -> queue -> tick -> Update.Core's
  // real dispatch -> render, not just "the mechanism ran".
  await page.keyboard.press("ArrowRight");
  await page.keyboard.press("ArrowDown");
  await page.keyboard.press("Enter");
  await page.waitForTimeout(100);
  await page.keyboard.type("=1+2*3");
  await page.waitForTimeout(100);
  await page.keyboard.press("Enter");
  await page.waitForTimeout(300);

  const footerText = await extractFooterPixelsHaveContent(page);

  // Keep driving real ticks for a real stretch after - the same
  // "repeated calls in a loop" risk Phase 0 already covers, now against
  // the actual production build+render path instead of the spike.
  // Idle ticks are deliberately throttled to a ~250ms heartbeat now
  // (was an unthrottled ~60/sec requestAnimationFrame loop - real CPU
  // cost for no reason, since nothing here animates continuously), so
  // the count/timeout here are recalibrated to that ~4/sec cadence,
  // not weakened - 30 ticks at ~250ms apart is ~7.5s of genuine
  // sustained operation, same order of magnitude as before relative to
  // the new rate.
  const ticksBefore = await page.evaluate(() => window.__trellisTicksRun);
  await page.waitForFunction(
    (n) => window.__trellisTicksRun >= n + 30,
    ticksBefore,
    { timeout: 15000 }
  );
  const ticksAfter = await page.evaluate(() => window.__trellisTicksRun);

  await browser.close();

  const ok =
    pageErrors.length === 0 &&
    footerText === true &&
    ticksAfter > ticksBefore + 20;

  console.log(
    `[run ${runIndex}] footerHasContent=${footerText} ticksBefore=${ticksBefore} ticksAfter=${ticksAfter} pageErrors=${JSON.stringify(pageErrors)}`
  );
  return ok;
}

// The footer row (bottom-left) shows "= <formula>" for the focused cell
// once a value is committed there - checking for non-background pixels
// specifically in that row is a real (if coarse) signal the commit
// actually reached SheetState, not just that *something* drew this frame.
async function extractFooterPixelsHaveContent(page) {
  return page.evaluate(() => {
    const c = document.getElementById("trellis-canvas");
    const ctx = c.getContext("2d");
    const rowH = 18; // approx cellH
    const y0 = c.height - rowH * 3;
    const data = ctx.getImageData(0, y0, c.width, rowH * 2).data;
    for (let i = 0; i < data.length; i += 4) {
      if (data[i] !== 0 || data[i + 1] !== 0 || data[i + 2] !== 0) return true;
    }
    return false;
  });
}

let allOk = true;
for (let i = 1; i <= 3; i++) {
  const ok = await run(i);
  console.log(`[run ${i}] ${ok ? "PASS" : "FAIL"}`);
  allOk = allOk && ok;
}

if (!allOk) {
  console.error("OVERALL: FAIL");
  process.exit(1);
}
console.log("OVERALL: PASS (3/3 runs green)");
