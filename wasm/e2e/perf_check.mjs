// Playwright verification that the tick-throttling fix (see main.mjs's
// own comment) actually holds, both halves: idle ticking genuinely
// dropped from an unthrottled ~60/sec requestAnimationFrame loop down to
// a ~4/sec (250ms) heartbeat, AND real input still redraws essentially
// immediately rather than waiting on that heartbeat. Measures real
// numbers rather than asserting intent - this is exactly the kind of
// regression that could silently creep back in.
// Serve wasm/ first, then run this from inside wasm/:
//   python3 -m http.server 8935 &
//   node e2e/perf_check.mjs
import { chromium } from "playwright";

async function run(runIndex) {
  const browser = await chromium.launch();
  const page = await browser.newPage();
  const pageErrors = [];
  page.on("pageerror", (err) => pageErrors.push(String(err)));

  await page.goto("http://127.0.0.1:8935/index.html");
  await page.waitForFunction(() => document.getElementById("status").textContent === "running", { timeout: 10000 });
  await page.waitForTimeout(200);

  // --- Half 1: idle rate. No input at all for a real 4s stretch. At the
  // old unthrottled rAF rate (~60/sec) this would be on the order of
  // 240 ticks; at the new 250ms heartbeat it should be ~16 (4000/250).
  // Generous bounds (8-30) to absorb real scheduling jitter without
  // being able to pass at anything close to the old rate.
  const idleBefore = await page.evaluate(() => window.__trellisTicksRun);
  await page.waitForTimeout(4000);
  const idleAfter = await page.evaluate(() => window.__trellisTicksRun);
  const idleTicks = idleAfter - idleBefore;
  const idleThrottled = idleTicks >= 8 && idleTicks <= 30;

  // --- Half 2: input latency. A real keypress should trigger a tick
  // essentially immediately (same task, via the listener's own
  // window.__trellisTick() call) - not wait out the next 250ms
  // heartbeat boundary. Check very shortly after dispatch.
  const beforeInput = await page.evaluate(() => window.__trellisTicksRun);
  await page.keyboard.press("ArrowRight");
  await page.waitForTimeout(30); // real wall-clock, well under the 250ms heartbeat
  const afterInput = await page.evaluate(() => window.__trellisTicksRun);
  const inputWasImmediate = afterInput > beforeInput;

  await browser.close();

  const ok = pageErrors.length === 0 && idleThrottled && inputWasImmediate;

  console.log(
    `[run ${runIndex}] idleTicks(4s)=${idleTicks} idleThrottled=${idleThrottled} ` +
    `inputWasImmediate=${inputWasImmediate} (ticksRun ${beforeInput}->${afterInput} within 30ms) ` +
    `pageErrors=${JSON.stringify(pageErrors)}`
  );
  return ok;
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
