test_that("getAssessments() runs", {
  expect_no_error(getAssessments(as.Date("2024-08-05")))
})
