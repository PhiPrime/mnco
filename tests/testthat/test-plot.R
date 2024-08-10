test_that("plotHighestCor() runs", {
  date <- as.Date("2024-08-05")
  dat <- getCenterData("progress", date)
  expect_no_error(plotHighestCor(dat))
})
