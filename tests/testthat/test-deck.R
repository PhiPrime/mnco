local_mocked_bindings(
  Sys.Date = function() as.Date("2024-08-05")
)

test_that("needsNewDeck() runs", {
  expect_no_error(needsNewDeck())
})

test_that("suppressDeckWarning() runs", {
  expect_no_error(suppressDeckWarning())
})

test_that("getSuppressedStudents() runs", {
  expect_no_error(getSuppressedStudents())
})

test_that("setSuppressedStudents() runs", {
  expect_no_error(setSuppressedStudents())
})

test_that("removeDeckSuppression() runs", {
  expect_no_error(removeDeckSuppression())
})

test_that("showcaseRegularizeScore() runs", {
  dat <- merge(getStudentRanking(),
               getMostRecentAssessments())
  expect_no_error(regularizeScore(dat,"Level", 4))
})

test_that("showcaseRegularizeScore() runs", {
  expect_no_error(suppressMessages(showcaseRegularizeScore()))
})
