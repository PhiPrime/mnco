local_mocked_bindings(Sys.Date = function() as.Date("2024-08-05"))

test_that("attendanceCheck() runs", {

  expect_no_error(attendanceCheck())
})

test_that("sendOnVacation() runs", {
    who <- "Abraham Torres"
  expect_no_error(sendOnVacation(who))
})

test_that("getStudentsOnVacation() runs", {
    expect_no_error(getStudentsOnVacation())
})

test_that("setStudentsOnVacation() runs", {
   expect_no_error(setStudentsOnVacation())
})

test_that("returnStudentFromVacation() runs", {
  who <- "Abraham Torres"
  expect_no_error(returnStudentFromVacation(who))
})
