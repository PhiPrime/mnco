test_that("getCenterData(type = \"student\") runs", {
  dir = test_path("raw-data")
  date = as.Date("08-05-2024", format = "%m-%d-%Y")
  expect_no_error(getCenterData(dir, type = "student", date = date))
})

test_that("getCenterData(type = \"account\") runs", {
  dir = test_path("raw-data")
  date = as.Date("08-05-2024", format = "%m-%d-%Y")
  expect_no_error(getCenterData(dir, type = "account", date = date))
})

test_that("getCenterData(type = \"progress\") runs", {
  dir = test_path("raw-data")
  date = as.Date("08-05-2024", format = "%m-%d-%Y")
  expect_no_error(getCenterData(dir, type = "progress", date = date))
})

test_that("getCenterData(type = \"enrollment\") runs", {
  dir = test_path("raw-data")
  date = as.Date("08-05-2024", format = "%m-%d-%Y")
  expect_no_error(getCenterData(dir, type = "enrollment", date = date))
})
