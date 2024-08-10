test_that("plotHighestCor() runs", {
  date <- as.Date("2024-08-05")
  dat <- getCenterData("progress", date)
  expect_no_error(plotHighestCor(dat))
})

test_that("expGradedifPest() runs", {
  local_mocked_bindings(
    Sys.Date = function() as.Date("2024-08-05")
  )
  expect_no_error(expGradedifPest())
})
