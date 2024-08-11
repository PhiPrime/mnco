test_that("saveTemplates() runs", {
  skip("updated size 0")
  expect_no_error(suppressMessages(saveTemplates(as.Date("2024-07-29"))))
})

test_that("getTemplate() runs", {
  expect_no_error(getTemplate())
})

test_that("templatesNeedUpdated() runs", {
  expect_no_error(templatesNeedUpdated(as.Date("2024-07-29")))
})

test_that("asMessageTxtFile() runs", {
  expect_no_error(asMessageTxtFile("1618033", "Luke Coughlin"))
})
