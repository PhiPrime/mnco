test_that("saveCenterData() runs", {
  date = as.Date("2024-08-05")
  expect_no_error(suppressMessages(saveCenterData(date)))
})

test_that("saveAllCenterData() runs", {
  expect_no_error(suppressMessages(saveAllCenterData(promptDelete = F)))
})
