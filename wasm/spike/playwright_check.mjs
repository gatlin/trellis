import { chromium } from "playwright";

async function run(runIndex) {
  const browser = await chromium.launch({
    executablePath: "/home/gcj/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome",
  });
  const page = await browser.newPage();
  // Only genuine JS exceptions count as failures - console.error also
  // catches this module's own WASI-stderr passthrough logging and
  // browser noise (e.g. a favicon 404), neither of which indicate the
  // tick/event mechanism actually broke.
  const pageErrors = [];
  page.on("pageerror", (err) => pageErrors.push(String(err)));

  await page.goto("http://127.0.0.1:8934/index.html");
  await page.waitForFunction(() => document.getElementById("status").textContent === "running", { timeout: 10000 });

  // Drive ~300 animation frames with no input first - the specific
  // "repeated calls in a loop" risk from the prior attempt's documented
  // failure mode.
  await page.waitForFunction(() => window.__trellisTicksRun >= 300, { timeout: 15000 });
  const ticksBefore = await page.evaluate(() => window.__trellisTicksRun);
  const tickCountBefore = await page.evaluate(() => document.getElementById("tickCount").textContent);

  // Synthesize a real keydown event on the page.
  await page.keyboard.press("a");

  // Drive ~300 more frames after the keydown.
  await page.waitForFunction(
    (n) => window.__trellisTicksRun >= n + 300,
    ticksBefore,
    { timeout: 15000 }
  );
  const lastKey = await page.evaluate(() => document.getElementById("lastKey").textContent);
  const ticksAfter = await page.evaluate(() => window.__trellisTicksRun);
  const tickCountAfter = await page.evaluate(() => document.getElementById("tickCount").textContent);

  await browser.close();

  const ok =
    pageErrors.length === 0 &&
    Number(tickCountBefore) >= 300 &&
    lastKey === "65" && // 'a' keyCode
    ticksAfter > ticksBefore &&
    Number(tickCountAfter) > Number(tickCountBefore);

  console.log(`[run ${runIndex}] ticksBefore=${ticksBefore} tickCountBefore=${tickCountBefore} lastKey=${lastKey} ticksAfter=${ticksAfter} tickCountAfter=${tickCountAfter} pageErrors=${JSON.stringify(pageErrors)}`);
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
