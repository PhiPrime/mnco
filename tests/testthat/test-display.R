local_mocked_bindings(Sys.Date = function() as.Date("2024-08-05"))

test_that("kablize() runs", {
  expect_no_error(kablize(needsNewDeck()))
})

test_that("displayMostProductiveStudents() runs", {
  expect_no_error(displayMostProductiveStudents())
})
