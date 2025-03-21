test_that("as.rawFilePath() works", {
  dir <- "Raw_Data"
  root <- "Current Batch Detail Export"
  date <- as.Date("2022-12-22")

  expect_equal(as.rawFilePath(dir, root, date),
               "Raw_Data/Current Batch Detail Export  12_22_2022.xlsx")
})

test_that("as.rawFileName() works", {
  root <- "Students Export"
  date <- as.Date("2021-05-30")

  expect_equal(as.rawFileName(root, date), "Students Export  5_30_2021.xlsx")
})

test_that("as.radiusDate() works for single digit month and day", {
  date <- as.Date("2022-04-08")
  expect_equal(as.radiusDate(date), "4_8_2022")
})

test_that("as.radiusDate() works for double digit month and day", {
  date <- as.Date("2022-10-14")
  expect_equal(as.radiusDate(date), "10_14_2022")
})

test_that("get_raw_na_cols() runs", {
  expect_no_error(get_raw_na_cols(as.Date("2024-08-05")))
})

test_that("print_raw_na_cols() runs", {
  expect_no_error(suppressMessages(print_raw_na_cols(as.Date("2024-08-05"))))
})
