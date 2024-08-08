test_that("getCenterData(type = \"student\") runs", {
  date = as.Date("08-05-2024", format = "%m-%d-%Y")
  expect_no_error(getCenterData(type = "student", date = date))
})

test_that("getCenterData(type = \"account\") runs", {
  date = as.Date("08-05-2024", format = "%m-%d-%Y")
  expect_no_error(getCenterData(type = "account", date = date))
})

test_that("getCenterData(type = \"progress\") runs", {
  date = as.Date("08-05-2024", format = "%m-%d-%Y")
  expect_no_error(getCenterData(type = "progress", date = date))
})

test_that("getCenterData(type = \"enrollment\") runs", {
  date = as.Date("08-05-2024", format = "%m-%d-%Y")
  expect_no_error(getCenterData(type = "enrollment", date = date))
})
