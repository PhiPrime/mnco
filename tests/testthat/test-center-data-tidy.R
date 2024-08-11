local_mocked_bindings(
  Sys.Date = function() as.Date("2024-08-05")
)

test_that("tidyRawData() runs", {
  type <- "student"
  dat <- readRawData(type)
  expect_no_error(tidyRawData(dat, type))
})

test_that("tidyRawData.student() runs", {
  dat <- readRawData("student")
  expect_no_error(tidyRawData.student(dat))
})

test_that("tidyRawData.account() runs", {
  dat <- readRawData("account")
  expect_no_error(tidyRawData.account(dat))
})

test_that("tidyRawData.progress() runs", {
  dat <- readRawData("progress")
  expect_no_error(tidyRawData.progress(dat))
})

test_that("tidyRawData.enrollment() runs", {
  dat <- readRawData("enrollment")
  expect_no_error(tidyRawData.enrollment(dat))
})

test_that("removeRawCols() runs", {
  dat <- readRawData("student")
  expect_no_error(removeRawCols(dat, c("Account_Id")))
})
