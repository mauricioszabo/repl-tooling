const Application = require('spectron').Application
const assert = require('assert')
const electronPath = require('electron')
const path = require('path')

const newApp = () => {
  const app = new Application({
    path: electronPath,
    args: ["integration.js"]
  })
  return app.start()
}

const runTestAndCollectResult = async (client, idx, numTests, numFailures) => {
  const selector = `.com-rigsomelight-devcard:nth-child(${idx})`
  const testName = await client.$(selector + " a").getText()
  const failures = await client.$$(selector + ' .com-rigsomelight-devcards-fail')

  console.log(`  ${testName}`)
  await client.$(selector + ' .com-rigsomelight-devcards-fail').getText()
    .then(console.log)
    .catch(() => console.log("   No errors on test"))
  const totalFailures = failures.length + numFailures
  if(idx >= numTests) {
    return totalFailures
  } else {
    return runTestAndCollectResult(client, idx + 1, numTests, totalFailures)
  }
}

const collectTest = async (client, idx, numTests, totalFailures) => {
  const selector = `.com-rigsomelight-devcards-list-group-item:nth-child(${idx})`
  await client.waitForText(selector)
  const span = `${selector} span`
  const txt = await client.$(span).getText()
  const numAsserts = parseInt(txt)
  console.log(`\n Collecting ${numAsserts} tests`)
  await client.$(selector).click()
  const failures = await runTestAndCollectResult(client, 1, numAsserts, 0)
  const total = failures + totalFailures

  if(idx >= numTests) {
    return total
  } else {
    await client.back()
    return collectTest(client, idx + 1, numTests, total)
  }
}

const runAllTests = async (app) => {
  await app.client.waitForText('a.com-rigsomelight-devcards-list-group-item')
  const tsts = await app.client.$$('a.com-rigsomelight-devcards-list-group-item')
  console.log(`Running ${tsts.length} testcases`)
  const failures = await collectTest(app.client, 1, tsts.length, 0)
  console.log(`\nTOTAL Failures:`, failures)

  await app.stop()
  if(failures == 0) {
    process.exit(0)
  } else {
    process.exit(1)
  }
}

newApp().then(runAllTests).catch(e => {
  console.log("ERROR:", e)
  process.exit(2)
})
