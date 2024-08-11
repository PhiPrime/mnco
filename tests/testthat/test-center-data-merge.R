test_that("mergeWithFill() runs", {
  date <- as.Date("2024-08-05")
  stu <- getCenterData("student", date)
  acc <- getCenterData("account", date)
  expect_no_error(mergeWithFill(stu, acc, "Account_Id"))
})
