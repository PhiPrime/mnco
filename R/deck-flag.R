#' Check which students need skills assigned
#'
#' @param minAllowed Threshold for number of assigned skills. Students under
#'  this threshold are returned.
#' @param date Date to use for data
#'
#' @return A data frame
#' @export
#'
#' @examples
#' # write later
needsNewDeck <- function(minAllowed = retrieve_variable("Deck_Minimum_Threshold"),
                         activeAllowed = 2) {
  # Remove suppressions for students with sufficient number of skills assigned
  removeSup <- getSuppressedStudents() %>%
    filter(.data$Skills_Assigned >= minAllowed)
  removeDeckSuppression(students = removeSup$Student)

  # Get flags from recent assessment and remove suppression if needed
  assessmentFlags <- needsDeck.assessment()
  removeDeckSuppression(students = assessmentFlags$Student)

  # Get suppressions
  sup <- getSuppressedStudents()

  # Main logic for deck flags
  flagged <- getCenterData() %>%
    dplyr::left_join(assessmentFlags, by = "Student") %>%
    filter(
      .data$Student %in% getActiveStudents() &
      !(.data$Student %in% sup$Student)
    ) %>%
    # No learning plan, assigned < allowed, recent assessment, or no last attendance
    # More than 1 learning plan
    filter(
      .data$Active_LPs == 0 |
      .data$Skills_Assigned < minAllowed |
      .data$Student %in% assessmentFlags$Student |
      is.na(.data$Last_Attendance_Date) |
      .data$Active_LPs > activeAllowed
    ) %>%
    mutate(
      Pest = round(.data$Pest, 4),
      Assessment = dplyr::if_else(.data$Student %in% assessmentFlags$Student, .data$Date_Taken, NA),
      LP_Link = paste0(
        "\\href{https://radius.mathnasium.com/Student/Details/",
        .data$Student_Id,
        "#learningPlanGrid}{Link}"
      )
    ) %>%
    select(
      "Student",
      "Skills_Assigned",
      "Assessment",
      "Active_LPs",
      "Last_Attendance_Date",
      "Pest",
      "Attendances",
      "Delivery",
      "LP_Link"
    ) %>%
    dplyr::arrange(
      !is.na(.data$Skills_Assigned),
      .data$Skills_Assigned,

      !is.na(.data$Assessment),
      .data$Assessment,

      desc(.data$Pest),
      .data$Student
    )

  return(flagged)
}

#' Needs deck based on assessment
#'
#' @return A [`character`] vector of students
#' @export
#'
#' @examples
#' needsDeckBasedOnAssessment()
needsDeck.assessment <- function() {
  ## Ways to tell if a deck needs made based on assessments:
  ### 1) Assessment Date is between Last_Attendance_Date and today
  ### 2) No last attendance date (new student)
  ### 3) Save data in a cache and keep track of all assessments for each student
  stu <- getCenterData("student") %>%
    select("Student", "Last_Attendance_Date")

  ## We will use option 1 and 2
  flagged <- getCenterData("assessment") %>%
    dplyr::left_join(stu, by = "Student") %>%
    filter(
      .data$Date_Taken >= .data$Last_Attendance_Date |
      is.na(.data$Last_Attendance_Date)
    ) %>%
    dplyr::group_by(.data$Student) %>%
    dplyr::slice_max(.data$Date_Taken, with_ties = F) %>%
    dplyr::ungroup() %>%
    select("Student", "Date_Taken")

  return(flagged)
}

### getSuppressedStudents
#' Retrieve list of suppressed students
#'
#' @return A data frame
#' @export
#'
#' @examples
#' # write later
getSuppressedStudents <- function() {
  supFilePath <- file.path(cacheDir(), "suppressions.rds")
  if(!file.exists(supFilePath)){
    sup <- data.frame(
      Student = character(0),
      Skills_Assigned = integer(0),
      Reason = character(0),
      creation = as.Date(integer(0)),
      expDate = as.Date(integer(0)),
      Student_Id = integer(0)
    )
    saveRDS(sup, supFilePath)
  }

  # Remove expired suppressions
  removeDeckSuppression()

  # Update Skills_Assigned
  skillsAssigned <- getCenterData() %>%
    select("Student", "Skills_Assigned", "Student_Id")

  # Order the values
  sup <- readRDS(supFilePath) %>%
    dplyr::rows_update(skillsAssigned, by = "Student_Id", unmatched = "ignore") %>%
    dplyr::arrange(
      .data$expDate,
      .data$creation,
      .data$Student
    )

  return(sup)
}

# Add readline() commands and a while loop to make friendly UI to quickly
#   suppress students
#' Exclude student from deck warnings
#'
#' @param ... Character string or vectors of students
#' @param duration Number of days
#'
#' @return None (invisible `NULL`)
#' @export
#'
#' @examples
#' # write later
suppressDeckWarning <- function(students, duration = retrieve_variable("Deck_Warning_Duration"), reason = NA_character_) {
  # Don't allow suppression longer than `maxTime` days
  maxDuration <- retrieve_variable("Deck_Suppression_Maximum_Time")
  if (duration > maxDuration) duration <- maxDuration

  expDate <- Sys.Date() + lubridate::days(duration)

  # Read suppression cache file, create if it doesn't exist
  supFilePath <- file.path(cacheDir(), "suppressions.rds")

  if(!file.exists(supFilePath)) {
    sup <- data.frame(
      Student = character(0),
      Skills_Assigned = integer(0),
      Reason = character(0),
      creation = as.Date(integer(0)),
      expDate = as.Date(integer(0))
    )
    saveRDS(sup, supFilePath)
  }
  sup <- readRDS(supFilePath)

  newSup <- getCenterData() %>%
    filter(.data$Student %in% students) %>%
    select(
      "Student",
      "Skills_Assigned"
    ) %>%
    mutate(
      Reason = reason,
      creation = Sys.Date(),
      expDate = expDate
    ) %>%
    dplyr::rows_upsert(sup, ., by = "Student")

  # Update .rds file if something has changed
  suppressed <- FALSE
  if (!identical(newSup, sup)) {
    saveRDS(newSup, supFilePath)
    suppressed <- TRUE
  }

  return(suppressed)
}

#' Remove deck warning suppression from student
#'
#' @param student Name of student to remove
#'
#' @return None (invisible(`NULL`))
#' @export
#'
#' @examples
#' # write later
removeDeckSuppression <- function(students = NULL) {
  # Read suppression cache file, create if it doesn't exist
  supFilePath <- file.path(cacheDir(), "suppressions.rds")

  if(!file.exists(supFilePath)) {
    sup <- data.frame(
      Student = character(0),
      Skills_Assigned = integer(0),
      Reason = character(0),
      creation = as.Date(integer(0)),
      expDate = as.Date(integer(0))
    )
    saveRDS(sup, supFilePath)
  }
  sup <- readRDS(supFilePath)

  if (is.null(students)) {
    # Default behavior: remove students whose expiration dates have passed
    newSup <- sup %>% filter(Sys.Date() <= .data$expDate)
  } else {
    # Remove only student if name passed into function
    newSup <- sup %>% filter(!(.data$Student %in% students))
  }

  removed <- FALSE
  if (!identical(newSup, sup)) {
    saveRDS(newSup, supFilePath)

    if (is.null(students)) {
      removed <- TRUE
    }
  }
  if (!is.null(students)) removed <- (students %in% sup$Student & !(students %in% newSup$Student))

  return(removed)
}
