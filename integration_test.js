const Application = require('spectron').Application
const assert = require('assert')
const electronPath = require('electron')
const path = require('path')

const newApp = () => {
  const app = new Application({
    waitTimeout: 15000,
    path: electronPath,
    args: ["integration.js"]
  })
  return app.start()
}

const sleep = num => new Promise(resolve => {
  setTimeout(resolve, num)
})

const runTestAndCollectResult = async (client, idx, numTests, numFailures) => {
  const selector = `.com-rigsomelight-devcard:nth-child(${idx})`
  const testName = await client.$(selector + " a").getText()
  const failSelector = ' .com-rigsomelight-devcards-fail,.com-rigsomelight-devcards-error'

  console.log(`  ${testName}`)

  let numPasses = 0, i = 0
  for(; i < 250; i++) {
    const num = await client.$(`${selector} button`).getText().catch(_ => "")
    numPasses = parseInt(num)
    if(numPasses != 0) { break; } else { await sleep(100) }
  }
  await client.$(selector + failSelector).getText()
    .then(console.log)
    .catch(() => console.log("   No errors on test"))
  const failures = await client.$$(selector + failSelector)


  let totalFailures = failures.length + numFailures
  if(numPasses == 0) {
    console.log("   Nothing passed on this testcase")
    totalFailures += 1
  }
  if(idx >= numTests) {
    if(!isNaN(numPasses)) console.log(`   ${numPasses} test(s) passed`)
    return totalFailures
  } else {
    return runTestAndCollectResult(client, idx + 1, numTests, totalFailures)
  }
}

const collectTest = async (client, idx, numTests, totalFailures) => {
  const selector = `.com-rigsomelight-devcards-list-group-item:nth-child(${idx})`
  await client.waitForText(selector)
  const description = await client.$(`${selector} span:nth-child(2)`).getText()
  const num = await client.$(`${selector} span`).getText()
  const numAsserts = parseInt(num)
  console.log(`\n [testcase] ${description} - collecting ${numAsserts} tests`)
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
  try {
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
  } catch(e) {
    console.log("ERRORS ocurred when running tests")
    console.log(e)
    app.stop()
    process.exit(2)
  }
}

newApp().then(runAllTests).catch(e => {
  console.log("ERROR:", e)
  process.exit(2)
})
