const Application = require('spectron').Application
const assert = require('assert')
const electronPath = require('electron')
const path = require('path')

newApp = () => {
  app = new Application({
    path: electronPath,
    args: ["integration.js"]
  })
  return app.start()
}
app2 = newApp()
// app2.then(e => app3 = e)
// app2.then(console.log)

runTestAndCollectResult = async (client, idx, numTests, numFailures) => {
  const selector = `.com-rigsomelight-devcard:nth-child(${idx})`
  const testName = await client.$(selector + " a").getText()
  const failures = await client.$$(selector + ' .com-rigsomelight-devcards-fail')

  console.log("\n", testName)
  client.$(selector + ' .com-rigsomelight-devcards-fail').getText()
    .then(console.log)
    .catch(() => console.log("No errors on test"))
  const totalFailures = failures.length + numFailures
  if(idx >= numTests) {
    return totalFailures
  } else {
    return runTestAndCollectResult(client, idx + 1, numTests, totalFailures)
  }
}
runTestAndCollectResult(client, 1, 4, 0).then(console.log)

collectTest = async (client, idx, numTests, totalFailures) => {
  const selector = `.com-rigsomelight-devcards-list-group-item:nth-child(${idx})`
  await client.waitForText(selector)
  const span = `${selector} span`
  const txt = await client.$(span).getText()
  const numAsserts = parseInt(txt)
  console.log(`Collecting ${numAsserts} tests`)
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
collectTest(client, 1, 4, 0)

collectTests = (client, tests) => {
  const numTests = 1 //tests.length
  collectTest(client, 1, 1)
  numTests.forEach((_, i) => {
    collectTest(
  })
}

runAllTests = (app) =>
  app.client.$$('a.com-rigsomelight-devcards-list-group-item').then(tsts =>
    collectTests(app.client, tsts)
  )

app2.then(runAllTests)
