#' Get students on vacation
#'
#' @return A data frame
#' @export
#'
#' @examples
#' getStudentsOnVacation()
getStudentsOnVacation <- function(){
  fileLoc <- file.path(cacheDir(), "vacations.rds")
  if(!file.exists(fileLoc)) {
    vac <- data.frame(
      Student = character(0),
      Last_Attendance = as.Date(integer(0)),
      returnDate = as.Date(integer(0))
    )
    saveRDS(vac, fileLoc)
  }

  # Update before returning (to check for exp)
  returnFromVacation()

  # Order the values
  vac <- readRDS(fileLoc) %>%
    dplyr::arrange(.data$returnDate, .data$Last_Attendance)

  return(vac)
}

#' Send student on vacation
#'
#' @param who Student name
#' @param returnDate Date of return
#'
#' @return None (invisible `NULL`)
#' @export
#'
#' @examples
#' sendOnVacation("John Doe", returnDate = "2024-07-31")
sendOnVacation <- function(..., returnDate = NULL) {
  # Allow multiple students to be passed in
  students <- unlist(list(...))

  # Handle returnDate argument
  if (is.null(returnDate)) {
    # Default argument: set to end of month
    returnDate <- lubridate::rollforward(Sys.Date())
  } else {
    # Test if in Date format when returnDate is given
    returnDate <- tryCatch(
      as.Date(returnDate),
      error = function(e) {
        stop("`returnDate` cannot be converted into a Date object: ", returnDate)
      }
    )
  }

  # Read vacation cache file, create if it doesn't exist
  vacFilePath <- file.path(cacheDir(), "vacations.rds")

  if(!file.exists(vacFilePath)) {
    vac <- data.frame(
      Student = character(0),
      Last_Attendance = as.Date(integer(0)),
      returnDate = as.Date(integer(0))
    )
    saveRDS(vac, vacFilePath)
  }
  vac <- readRDS(vacFilePath)

  # Check students exists and grab last attendance date
  stu <- getCenterData("student") %>%
    select("Student", "Last_Attendance_Date") %>%
    # UNRENAME THIS LATER
    dplyr::rename(Last_Attendance = "Last_Attendance_Date") %>%
    filter(tolower(.data$Student) %in% tolower(students)) %>%
    mutate(returnDate = returnDate)

  # Append students to vacation data frame
  newVac <- vac %>%
    dplyr::rows_upsert(stu, by = "Student")

  # Update .rds file if something has changed
  studentSent <- FALSE
  if (!identical(newVac, vac)) {
    saveRDS(newVac, vacFilePath)
    studentSent <- TRUE
  }

  return(studentSent)
}

#' Remove student from vacation file
#'
#' @param who Student to remove
#'
#' @return None (invisible `NULL`)
#' @export
#'
#' @examples
#' returnFromVacation("John Doe")
returnFromVacation <- function(studentName = NULL) {
  vacFilePath <- file.path(cacheDir(), "vacations.rds")
  vac <- readRDS(vacFilePath)

  studentReturned <- FALSE

  if (is.null(studentName)) {
    # Default behavior: remove students whose return dates have passed OR
    #   if they've attended a session since going on vacation
    stu <- getCenterData("student") %>%
      transmute(
        Student = .data$Student,
        stu_last_attendance = .data$Last_Attendance_Date
      )
    newVac <- vac %>%
      dplyr::left_join(stu, by = "Student") %>%
      filter(Sys.Date() <= .data$returnDate) %>%
      filter(.data$stu_last_attendance <= .data$Last_Attendance) %>%
      select(-"stu_last_attendance")

    if (!identical(newVac, vac)) {
      saveRDS(newVac, vacFilePath)
      studentReturned <- TRUE
    }
  } else if (studentName %in% vac$Student) {
    # Remove only student if name passed into function
    vac %>%
      filter(.data$Student != studentName) %>%
      saveRDS(vacFilePath)

    studentReturned <- TRUE
  }

  return(studentReturned)
}
