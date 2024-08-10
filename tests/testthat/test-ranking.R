test_that("getStudentRanking() runs", {
  date = as.Date("2024-08-05")
  expect_no_error(getStudentRanking(date))
})

test_that("addDifferentDurationStudent() runs", {
  expect_no_error(addDifferentDurationStudent("Kristina Kelchner", 90))
})

test_that("removeDifferentDurationStudent() runs", {
  expect_no_error(removeDifferentDurationStudent("Test Student"))
})
