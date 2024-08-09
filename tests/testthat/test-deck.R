local_mocked_bindings(
  Sys.Date = function() as.Date("2024-08-05")
)

test_that("needsNewDeck() runs", {
  expect_no_error(needsNewDeck())
})
