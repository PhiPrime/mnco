#' Check which students have not attended
#'
#' @param allowedBdays Minimum number of days not attended to flag
#'
#' @return A data frame
#' @export
#'
#' @examples
#' attendanceCheck(allowedBdays = 10)
attendanceCheck <- function(allowedBdays = retrieve_variable("Attendance_Allowed_Days")) {
  stu <- getCenterData("student")
  acc <- getCenterData("account")

  # Get students on vacation and remove if past return date
  vac <- getStudentsOnVacation()

  # Get list of dates any student attended
  acceptableDates <- stu %>%
    dplyr::filter(Last_Attendance_Date != Sys.Date()) %>%
    dplyr::pull("Last_Attendance_Date") %>%
    unique() %>%
    sort() %>%
    tail(5) %>%
    c(Sys.Date())

  flaggedStudents <-
    mergeWithFill(stu, acc, .by = "Account_Id") %>%
    dplyr::filter(
      .data$Enrollment_Status == "Enrolled" &
      !(.data$Last_Attendance_Date %in% acceptableDates) &
      !(.data$Student %in% vac$Student)
    ) %>%
    dplyr::transmute(
      Last_Attendance_Date = format(.data$Last_Attendance_Date, "%m/%d/%Y"),
      Student = .data$Student,
      Account = .data$Account %>%
        stringr::str_replace("(?:(.+?), (.+))", "\\2 \\1"),
      # Select phone in this order: Mobile, Home, Other
      Phone = dplyr::coalesce(
        .data$Mobile_Phone,
        .data$Home_Phone,
        .data$Other_Phone
      ),
      # For first name lookups, should be selected out before kable output
      Student_Id = .data$Student_Id,
      Account_Id = .data$Account_Id
    ) %>%
    createTextMessageFiles() %>%
    dplyr::select(-"Student_Id", -"Account_Id") %>%
    dplyr::mutate(across(
      c("Account", "Phone"),
      ~ifelse(is.na(.data$Link_1), NA_character_, .x)
    )) %>%
    dplyr::arrange(
      lubridate::mdy(.data$Last_Attendance_Date), .data$Account, .data$Student
    )

  return(flaggedStudents)
}

#' Format and store attendance text messages
#'
#' The text messages is stored in .txt files and named using the date and
#'  account name. All students under a single account are put into the
#'  same text message.
#'
#' @param flaggedStudents
#' @param date
#'
#' @return Character string containing the name of the file
#'
#' @examples
createTextMessageFiles <- function(flaggedStudents, date = Sys.Date()) {
  # Get a copy of flaggedStudents with first names
  studentRawData <-
    readRawData("student") %>%
    dplyr::transmute(
      Student_Id = .data$Student_Id,
      Student_First_Name = First_Name
    )
  accountRawData <-
    readRawData("account") %>%
    dplyr::transmute(
      Account_Id = .data$Account_Id,
      Account_First_Name = First_Name
    )
  flaggedFirstNames <- flaggedStudents %>%
    dplyr::left_join(studentRawData, by = "Student_Id") %>%
    dplyr::left_join(accountRawData, by = "Account_Id")

  # Get templates to be filled in
  templates <- data.frame(
    message1 = getTemplate(name = "!TEXT - Attendance Reminder"),
    message2 = getTemplate(name = "!TEXT - Attendance Reminder 2")
  )

  # Create message columns to be filled in
  flaggedStudents <- flaggedStudents %>%
    dplyr::mutate(Link_1 = NA_character_, Link_2 = NA_character_)

  # Iterate through each account ------------------------------
  for (accountID in unique(flaggedStudents$Account_Id)) {
    # Filter students for each account
    flaggedAccount <- flaggedFirstNames %>%
      dplyr::filter(.data$Account_Id == accountID)

    # Get student and account first names to put in message
    studentFirstNames <- flaggedAccount %>%
      dplyr::pull("Student_First_Name") %>%
      pluralizeNames()
    accountFirstName <- flaggedAccount$Account_First_Name[1]

    # Create file name root for this account
    accountName <- flaggedStudents %>%
      dplyr::filter(.data$Account_Id == accountID) %>%
      magrittr::extract(1, "Account")
    fileRoot <- paste0(Sys.Date(), "__", accountName) %>%
      stringr::str_replace_all("[ -]", "_")

    # Fill in templates and create hyperlinks to text files
    # NEED TO PULL CENTER NAME TO FILL IN
    messages <- templates %>%
      dplyr::mutate(across(everything(), ~stringr::str_replace_all(
        .x, "\\[Center\\]", "Colonial Park"
      ))) %>%
      dplyr::mutate(across(everything(), ~stringr::str_replace_all(
        .x, "\\[StudentFirstName\\]", studentFirstNames
      ))) %>%
      dplyr::mutate(across(everything(), ~stringr::str_replace_all(
        .x, "\\[AccountFirstName\\]", accountFirstName
      ))) %>%
      dplyr::mutate(
        # Use first student alphabetically for links
        Student_Id = flaggedFirstNames %>%
          dplyr::filter(.data$Account_Id == accountID) %>%
          dplyr::arrange(.data$Student) %>%
          magrittr::extract(1, "Student_Id"),

        path1 = file.path(
          getwd(), cacheDir(), "Messages", paste0(fileRoot, "-1.txt")
        ),
        path2 = file.path(
          getwd(), cacheDir(), "Messages", paste0(fileRoot, "-2.txt")
        ),

        Link_1 = paste0("\\href{", .data$path1, "}{Text 1}"),
        Link_2 = paste0("\\href{", .data$path2, "}{Text 2}")
      )

    # Write messages to text files
    file.create(messages$path1)
    file.create(messages$path2)
    writeLines(messages$message1, messages$path1)
    writeLines(messages$message2, messages$path2)

    # Add links to students to return back to attendanceCheck()
    # Only the first student alphabetically for each account is given links
    flaggedStudents <- messages %>%
      dplyr::select("Student_Id", "Link_1", "Link_2") %>%
      dplyr::rows_patch(flaggedStudents, ., by = "Student_Id")
  }

  return(flaggedStudents)
}

#' Title
#'
#' @param ...
#'
#' @return
#'
#' @examples
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
#'
#' @examples
#' getAttendanceTrainingSet()
getAttendanceTrainingSet <- function() {
  readRDS(file.path(cacheDir(), "prior2024.rds"))
}
