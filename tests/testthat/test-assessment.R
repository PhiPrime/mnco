test_that("getAssessments() runs", {
  dir <- "raw-data"
  date <- as.Date("2024-08-05")
  expect_no_error(getAssessments(dir, date = date))
})

test_that("needsDeckBasedOnAssessment() runs", {
  dir <- "raw-data"
  date <- as.Date("2024-08-05")
  expect_no_error(needsDeckBasedOnAssessment(dir, date = date))
})
