test_that("getPricing() runs", {
  local_mocked_bindings(Sys.Date = function() as.Date("2024-08-05"))
  expect_no_error(getPricing())
})

test_that("getPricingGrid() runs", {
  local_mocked_bindings(Sys.Date = function() as.Date("2024-08-05"))
  expect_no_error(getPricingGrid())
})
