test_that("getAssessments() runs", {
  date <- as.Date("2024-08-05")
  expect_no_error(getAssessments(date))
})

test_that("getHistoricAssessments() runs", {
  expect_no_error(getHistoricAssessments())
})

test_that("tidyAssessments() runs", {
  date <- as.Date("2024-08-05")
  dat <- readRawData.old(paste0("Assessment Report from [0-9]+_[0-9]+_[0-9]+ ",
                                "to [0-9]+_[0-9]+_[0-9]+"),
                         date, ignoreMissing, regExFile = TRUE)
  expect_no_error(tidyAssessments(dat))
})

test_that("getMostRecentAssessments() runs", {
  expect_no_error(getMostRecentAssessments())
})

test_that("needsDeckBasedOnAssessment() runs", {
  date <- as.Date("2024-08-05")
  expect_no_error(needsDeckBasedOnAssessment(date))
})
