test_that("attendanceCheck() runs", {
  today <- the$CURRENT_DATE
  setCurrentDate(as.Date("2024-08-05"))
  withr::defer(setCurrentDate(today))

  expect_no_error(attendanceCheck())
})

test_that("sendOnVacation() runs", {
  today <- the$CURRENT_DATE
  setCurrentDate(as.Date("2024-08-05"))
  withr::defer(setCurrentDate(today))

  who <- "Abraham Torres"
  expect_no_error(sendOnVacation(who))
})

test_that("getStudentsOnVacation() runs", {
  today <- the$CURRENT_DATE
  setCurrentDate(as.Date("2024-08-05"))
  withr::defer(setCurrentDate(today))

  expect_no_error(getStudentsOnVacation())
})

test_that("setStudentsOnVacation() runs", {
  today <- the$CURRENT_DATE
  setCurrentDate(as.Date("2024-08-05"))
  withr::defer(setCurrentDate(today))

  expect_no_error(setStudentsOnVacation())
})

test_that("returnStudentFromVacation() runs", {
  today <- the$CURRENT_DATE
  setCurrentDate(as.Date("2024-08-05"))
  withr::defer(setCurrentDate(today))

  who <- "Abraham Torres"
  expect_no_error(returnStudentFromVacation(who))
})
