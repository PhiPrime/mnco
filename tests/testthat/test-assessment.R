test_that("getAssessments() runs", {
  expect_no_error(getAssessments(as.Date("2024-08-05")))
})

test_that("getHistoricAssessments() runs", {
  expect_no_error(getHistoricAssessments())
})
