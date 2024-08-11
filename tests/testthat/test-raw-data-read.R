test_that("readRawData() runs", {
  expect_no_error(readRawData("student", as.Date("2024-08-05")))
})

test_that("readRawData.old() runs", {
  expect_no_error(readRawData.old("Students Export", as.Date("2024-08-05")))
})
