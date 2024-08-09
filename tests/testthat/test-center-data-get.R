test_that("getCenterData(type = \"all\") runs", {
  date = as.Date("2024-08-05")
  expect_no_error(getCenterData(type = "all", date = date))
})

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

test_that("getPaymentData() runs", {
  date = as.Date("2024-08-04")
  expect_no_error(getPaymentData(date))
})

test_that("getCurriculumData() runs", {
  date = as.Date("2024-07-11")
  expect_no_error(getCurriculumData(date))
})

test_that("getAttendanceTrainingSet() runs", {
  expect_no_error(getAttendanceTrainingSet())
})

test_that("getAttendanceData() runs", {
  date = as.Date("2024-07-11")
  expect_no_error(getAttendanceData(date = date))
})
