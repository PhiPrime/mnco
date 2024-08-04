test_that("as.rawFileName() works", {
  rawFileRoot <- "Students Export"
  date <- as.Date("2023-08-23")
  expect_equal(as.rawFileName(rawFileRoot, date),
               "Students Export  8_23_2023.xlsx")
})

test_that("as.radiusDate() works for single digit month and day", {
  date <- as.Date("2022-04-08")
  expect_equal(as.radiusDate(date), "4_8_2022")
})

test_that("as.radiusDate() works for double digit month and day", {
  date <- as.Date("2022-10-14")
  expect_equal(as.radiusDate(date), "10_14_2022")
})
