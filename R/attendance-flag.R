#' Check which students have not attended
#'
#' @param allowedBdays Minimum number of days not attended to flag
#'
#' @return A data frame
#' @export
#'
#' @examples
#' attendanceCheck(allowedBdays = 10)
attendanceCheck <- function(days = mnco::retrieve_variable("Attendance_Allowed_Days")) {
  # Get students on vacation and remove if past return date
  vac <- getStudentsOnVacation()

  # Get list of dates any student attended in past week, plus today
  #   Students are flagged if last attended exactly 1 week ago (usually)
  acceptableDates <- c(attendanceDates(days), Sys.Date())

  flaggedStudents <-
    getCenterData(c("student", "account")) %>%
    filter(
      .data$Student %in% getActiveStudents() &
      !(.data$Last_Attendance_Date %in% acceptableDates) &
      !(.data$Student %in% vac$Student)
    ) %>%
    transmute(
      Student = .data$Student,
      Last_Attendance = .data$Last_Attendance_Date,
      Days = as.integer(Sys.Date() - .data$Last_Attendance_Date),
      Account = .data$Account,
      # Select phone in this order: Mobile, Home, Other
      Phone = dplyr::coalesce(
        .data$Mobile_Phone,
        .data$Home_Phone,
        .data$Other_Phone
      )
    ) %>%
    createTextMessageFiles() %>%
    dplyr::arrange(
      !is.na(.data$Last_Attendance),
      .data$Last_Attendance,

      .data$Account,
      .data$Student
    )

  newStudentAssessmentDates <- getCenterData("assessment") %>%
    dplyr::group_by(.data$Student) %>%
    dplyr::slice_max(.data$Date_Taken, with_ties = F) %>%
    dplyr::ungroup() %>%
    select("Student", "Date_Taken") %>%
    dplyr::rename(Last_Attendance = .data$Date_Taken)

  flaggedStudents <- flaggedStudents %>%
    dplyr::rows_patch(newStudentAssessmentDates, by = "Student", unmatched = "ignore")

  # Group students by account
  output <- NULL
  for (account in unique(flaggedStudents$Account)) {
    students <- flaggedStudents %>% filter(.data$Account == account)

    link1 <- dplyr::pull(students, "Link_1")
    if (!stringr::str_detect(link1[1], "phantom")) {
      link1 <- c(link1[1], rep("\\^{}", length(link1) - 1))
    }
    students$Link_1 <- link1

    link2 <- dplyr::pull(students, "Link_2")
    if (!stringr::str_detect(link2[1], "phantom")) {
      link2 <- c(link2[1], rep("\\^{}", length(link2) - 1))
    }
    students$Link_2 <- link2

    if (is.null(output)) {
      output <- students
      next
    }
    output <- output %>% dplyr::rows_insert(students, by = "Student")
  }

  output <- output %>%
    mutate(across(
      c("Account", "Phone"),
      ~ifelse(is.na(.data$Link_1) | is.na(.data$Link_2), "\\^{}", .x)
    ))


  return(output)
}

#' Format and store attendance text messages
#'
#' The text messages is stored in .txt files and named using the date and
#'  account name. All students under a single account are put into the
#'  same text message.
#'
#' @param flaggedStudents Data frame of students
#' @param date Date for file name
#'
#' @return Character string containing the name of the file
#' @noRd
createTextMessageFiles <- function(flaggedStudents) {
  # Get a copy of flaggedStudents with first names
  flaggedFirstNames <- getCenterData(c("student", "account")) %>%
    select("Student", "Student_First", "Account_Id", "Account_First") %>%
    dplyr::right_join(flaggedStudents, by = "Student")

  # Get templates to be filled in
  templates <- data.frame(
    message1 = getTemplate(name = "!TEXT - Attendance Reminder"),
    message2 = getTemplate(name = "!TEXT - Attendance Reminder 2")
  )

  # Create message columns to be filled in
  flaggedStudents <- flaggedStudents %>%
    mutate(Link_1 = NA_character_, Link_2 = NA_character_)

  # Cache -----------------
  # Create cache if it doesn't exist
  cachePath <- file.path(cacheDir(), "attendanceFlags.rds")
  if (!file.exists(cachePath)) {
    cache <- tibble::tibble(
      Student = character(0),
      Date_Added = as.Date(integer(0))
    )
    saveRDS(cache, cachePath)
  }

  # Remove students not flagged
  oldCache <- readRDS(cachePath)
  cache <- oldCache %>% filter(.data$Student %in% flaggedStudents$Student)

  # Add new flagged students using today for Date_Added
  newFlags <- flaggedStudents %>%
    filter(!(.data$Student %in% cache$Student)) %>%
    transmute(
      Student = .data$Student,
      Date_Added = Sys.Date()
    )
  cache <- cache %>%
    dplyr::rows_insert(newFlags, by = "Student")

  # Change Date_Added dates that were not business days to today
  #   This prevents being assigned text 2 if they were added to cache for
  #   testing or other reasons on a closed day.
  allAttendanceDates <- attendanceDates("all")
  cache <- cache %>%
    mutate(
      Date_Added = dplyr::case_when(
        !(.data$Date_Added %in% allAttendanceDates) ~ Sys.Date(),
        .default = .data$Date_Added
      )
    )

  # Save cache
  if (!identical(oldCache, cache)) saveRDS(cache, cachePath)

  # Join cache to flags to process
  flaggedFirstNames <- flaggedFirstNames %>%
    dplyr::left_join(cache, by = "Student")

  # Iterate through each account ------------------------------
  for (accountID in unique(flaggedFirstNames$Account_Id)) {
    # Filter students for each account
    flaggedAccount <- flaggedFirstNames %>%
      filter(.data$Account_Id == accountID)

    # Get student and account first names to put in message
    studentFirstNames <- flaggedAccount %>%
      dplyr::pull("Student_First") %>%
      pluralizeNames()
    accountFirstName <- flaggedAccount$Account_First[1]

    # Create file name root for this account using phone number
    fileRoot <- flaggedFirstNames %>%
      filter(.data$Account_Id == accountID) %>%
      select("Account", "Phone") %>%
      head(1) %>%
      mutate(root = paste0(.data$Phone, "__", .data$Account)) %>%
      dplyr::pull("root") %>%
      stringr::str_replace_all("[()]", "") %>%
      stringr::str_replace_all("[ -]", "_")

    # Fill in templates and create hyperlinks to text files
    # NEED TO PULL CENTER NAME TO FILL IN
    messages <- templates %>%
      mutate(across(everything(), ~stringr::str_replace_all(
        .x, "\\[Center\\]", "Colonial Park"
      ))) %>%
      mutate(across(everything(), ~stringr::str_replace_all(
        .x, "\\[StudentFirstName\\]", studentFirstNames
      ))) %>%
      mutate(across(everything(), ~stringr::str_replace_all(
        .x, "\\[AccountFirstName\\]", accountFirstName
      ))) %>%
      mutate(
        # Use earlier last attendance date student for links
        Student = flaggedAccount %>%
          dplyr::arrange(.data$Last_Attendance, .data$Student) %>%
          dplyr::pull("Student") %>%
          head(1),

        # Create path to message text files
        path1 = file.path(
          getwd(), cacheDir(), "messages", paste0(fileRoot, "-1.txt")
        ),
        path2 = file.path(
          getwd(), cacheDir(), "messages", paste0(fileRoot, "-2.txt")
        ),

        # Create latex hyperlinks to files
        Link_1 = paste0("\\href{", .data$path1, "}{Text 1}"),
        Link_2 = paste0("\\href{", .data$path2, "}{Text 2}"),

        # If new flag, show link 1, else show link 2
        Link_1 = dplyr::case_when(
          Sys.Date() %in% flaggedAccount$Date_Added ~ .data$Link_1,
          .default = stringr::str_replace(
            .data$Link_1,
            "Text 1",
            "\\\\phantom{Text 1}"
          )
        ),
        Link_2 = dplyr::case_when(
          Sys.Date() %in% flaggedAccount$Date_Added ~ stringr::str_replace(
            .data$Link_2,
            "Text 2",
            "\\\\phantom{Text 2}"
          ),
          .default = .data$Link_2
        )
      )

    # Write messages to text files
    file.create(messages$path1)
    file.create(messages$path2)
    writeLines(messages$message1, messages$path1)
    writeLines(messages$message2, messages$path2)

    # Add links to students to return back to attendanceCheck()
    # Only the first student alphabetically for each account is given links
    flaggedStudents <- messages %>%
      select("Student", "Link_1", "Link_2") %>%
      dplyr::rows_patch(flaggedStudents, ., by = "Student")
  }

  return(flaggedStudents)
}

#' Format character strings in sentence list form
#'
#' @param ... Character strings or vectors
#'
#' @return A character string
#'
#' @examples
#' pluralizeNames("Justin")
#' pluralizeNames("Justin", "Luke")
#' pluralizeNames("Justin", "Luke", "Pat")
pluralizeNames <- function(...) {
  names <- unlist(list(...)) %>%
    sort()

  names <- switch(as.character(length(names)),
    "0" = NA_character_,
    "1" = names,
    "2" = paste0(names, collapse = " and "),
    # 3 or mores names
    paste0(names, collapse = ", ") %>%
      stringr::str_replace("(^.+, )", "\\1and ")
  )
  if (any(is.na(names))) warning("Vector `names` is empty or contains NA.")

  return(names)
}

#' Read training data for attendance
#'
#' @return A data frame
#' @export
getAttendanceTrainingSet <- function() {
  readRDS(file.path(cacheDir(), "attendance-training-data.rds"))
}
