#' Get enrolled students that are not pseudo-inactive
#'
#' @return A data frame
#' @export
getActiveStudents <- function() {
  getCenterData("student") %>%
    filter(
      .data$Enrollment_Status == "Enrolled" &
      !(.data$Student %in% getPseudoInactiveStudents()$Student)
    ) %>%
    dplyr::pull("Student") %>%
    sort()
}

#' Get pseudo-inactive students from cache
#'
#' @return A data frame
#' @export
getPseudoInactiveStudents <- function() {
  pseuFilePath <- cacheDir("pseudoInactive.rds")
  if(!file.exists(pseuFilePath)) {
    pseu <- tibble::tibble(
      Student = character(0),
      Enrollment_Status = character(0),
      Date_Added = as.Date(integer(0))
    )
    saveRDS(pseu, pseuFilePath)
  }

  # Order the values
  pseu <- readRDS(pseuFilePath) %>%
    dplyr::arrange(.data$Date_Added, .data$Student)

  return(pseu)
}

#' Set student as pseudo-inactive
#'
#' @param students Character string or vector
#'
#' @return Whether student was successfully added
#' @export
setPseudoInactiveStudent <- function(students) {
  # Allow multiple students to be passed in
  students <- unlist(list(students))

  # Read pseudo-inactive cache file, create if it doesn't exist
  pseuFilePath <- cacheDir("pseudoInactive.rds")

  if(!file.exists(pseuFilePath)) getPseudoInactiveStudents()
  pseu <- readRDS(pseuFilePath)

  # Check students exists
  stu <- getCenterData("student") %>%
    select("Student", "Enrollment_Status") %>%
    filter(.data$Student %in% students) %>%
    mutate(Date_Added = Sys.Date())

  # Append students to pseudo-inactive data frame
  newPseu <- pseu %>%
    dplyr::rows_upsert(stu, by = "Student")

  # Update .rds file if something has changed
  changed <- FALSE
  if (!identical(newPseu, pseu)) {
    saveRDS(newPseu, pseuFilePath)
    changed <- TRUE
  }
  changed <- changed & !(students %in% pseu$Student) & (students %in% newPseu$Student)

  return(changed)
}

#' Remove pseudo-inactive student
#'
#' @param who Student to remove
#'
#' @return Whether student was removed successfully
#' @export
removePseudoInactiveStudent <- function(students) {
  # Allow multiple students to be passed in
  students <- unlist(list(students))

  # Read pseudo-inactive cache file, create if it doesn't exist
  pseuFilePath <- cacheDir("pseudoInactive.rds")

  if(!file.exists(pseuFilePath)) getPseudoInactiveStudents()
  pseu <- readRDS(pseuFilePath)

  # Remove students from data frame
  newPseu <- pseu %>% filter(!(.data$Student %in% students))

  # Update .rds file if something has changed
  changed <- FALSE
  if (!identical(newPseu, pseu)) {
    saveRDS(newPseu, pseuFilePath)
    changed <- TRUE
  }
  changed <- changed & (students %in% pseu$Student) & !(students %in% newPseu$Student)

  return(changed)
}
