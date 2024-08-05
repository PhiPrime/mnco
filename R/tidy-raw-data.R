tidyRawData <- function(data, type) {
  switch (type,
    "student" = tidyStudentData(data),
    "account" = tidyAccountData(data),
    "progress" = tidyProgressData(data),
    "enrollment" = tidyEnrollment(data),
    stop("`type` is not a valid argument: \'", type, "\'")
  )
}

tidyStudentData <- function(data) {

}

tidyAccountData <- function(data) {

}

tidyProgressData <- function(data) {

}

tidyEnrollmentData <- function (data) {

}
