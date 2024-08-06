test_that("attendanceCheck() runs", {
  rawDir <- "raw-data"
  cacheDir <- "cache"
  date <- as.Date("2024-08-05")
  expect_no_error(attendanceCheck(rawDir, cacheDir, date = date))
})

test_that("sendOnVacation() runs", {
  rawDir <- "raw-data"
  cacheDir <- "cache"
  date <- as.Date("2024-08-05")
  who <- "Abraham Torres"
  expect_no_error(sendOnVacation(who, rawDir, cacheDir, date))
})

test_that("getStudentsOnVacation() runs", {
  rawDir <- "raw-data"
  cacheDir <- "cache"
  date <- as.Date("2024-08-05")
  expect_no_error(getStudentsOnVacation(rawDir, cacheDir, date))
})

test_that("setStudentsOnVacation() runs", {
  rawDir <- "raw-data"
  cacheDir <- "cache"
  date <- as.Date("2024-08-05")
  expect_no_error(setStudentsOnVacation(rawDir, cacheDir, date))
})

test_that("returnStudentFromVacation() runs", {
  rawDir <- "raw-data"
  cacheDir <- "cache"
  date <- as.Date("2024-08-05")
  who <- "Abraham Torres"
  expect_no_error(returnStudentFromVacation(who, rawDir, cacheDir, date))
})
