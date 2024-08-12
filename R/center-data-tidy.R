#' Tidy Radius data
#'
#' @param data Data frame to tidy
#' @param type Radius data file type
#'
#' @return A data frame
#' @export
#'
#' @examples
tidyRawData <- function(data, type) {
  switch (type,
    "student" = tidyRawData.student(data),
    "account" = tidyRawData.account(data),
    "progress" = tidyRawData.progress(data),
    "enrollment" = tidyRawData.enrollment(data),
    stop("`type` is not a valid argument: \'", type, "\'")
  )
}

#' Tidy Radius student data
#'
#' @inheritParams tidyRawData
#'
#' @return A data frame
#'
#' @examples
tidyRawData.student <- function(data) {
  # Rename columns
  names(data)[names(data) == "Lead_Id...2"] <- "Lead_Id"

  # Reformat columns
  data <- dplyr::mutate(data, Last_Attendance_Date = as.Date(.data$Last_Attendance_Date, format = "%m/%d/%Y"))

  # Create new columns
  data <- dplyr::mutate(data, Student = paste(.data$First_Name, .data$Last_Name), .before = "Student_Id")

  # Columns to be removed
  # ORGANIZE rm_cols
  # COULD MAKE FLAG FOR CARD LEVEL UPGRADE
  rm_cols <- c("First_Name", "Last_Name", "School_Year", "Lead_Id...24",
               "Created_By", "Stars_on_Current_Card", "Last_Modified_By",
               "Cards_Available", "Student_Notes", "Consent_to_Media_Release",
               "Consent_to_Contact_Teacher", "Consent_to_Leave_Unescorted",
               "Last_Modified_On", "Center_Id", "Virtual_Center",
               "Emergency_Contact", "Emergency_Phone")

  na_cols <- c("Billing_Street_1", "Billing_Street_2", "Billing_City",
               "Billing_State", "Billing_Country", "Billing_Zip_Code",
               "Scholarship", "School_[WebLead]", "Teacher_[WebLead]")
  # REXAMINE COLUMNS AFTER ABOVE ARE REVIEWED

  # Remove columns
  data <- removeRawCols(data, rm_cols)
  data <- removeRawCols(data, na_cols, test_na = T)

  return (data)
}

#' Tidy Radius account data
#'
#' @inheritParams tidyRawData
#'
#' @return A data frame
#'
#' @examples
tidyRawData.account <- function(data) {
  # Create new columns
  data <- dplyr::mutate(data, Account = paste0(.data$Last_Name, ", ", .data$First_Name), .before = "Account_Id")

  # Columns to be removed
  rm_cols <- c("First_Name", "Last_Name", "Last_Modified_By...20",
               "Last_Modified_By...33", "Description", "Customer_Comments",
               "Last_Modified_Date", "Emergency_Phone_Number",
               "Emergency_Contact", "Account_Relation")
  na_cols <- c("Date_of_Birth", "Last_TriMathlon_Reg._Date")
  # REXAMINE COLUMNS AFTER ABOVE ARE REVIEWED

  # Remove columns
  # data <- removeRawCols(data, rm_cols)
  data <- removeRawCols(data, na_cols, test_na = T)

  return(data)
}

#' Tidy Radius progress data
#'
#' @inheritParams tidyRawData
#'
#' @return A data frame
#'
#' @examples
tidyRawData.progress <- function(data) {
  # PROCESS COLUMNS HERE
  rm_cols <- c("Guardian")
  na_cols <- c("BPR_Comment")

  maybe_cols <- c("Email_Opt_Out")

  # Remove columns
  data <- removeRawCols(data, rm_cols)
  data <- removeRawCols(data, na_cols, test_na = T)

  return(data)
}

#' Tidy Radius enrollment data
#'
#' @inheritParams tidyRawData
#'
#' @return A data frame
#'
#' @examples
tidyRawData.enrollment <- function (data) {
  # Rename columns
  names(data)[names(data) == "Account_Name"] <- "Account"
  names(data)[names(data) == "Total_Sessions"] <- "Monthly_Sessions"
  names(data)[names(data) == "Primary_Enrollment_End"] <- "Contract_End_Date"

  # Reformat columns
  data <- data %>%
    dplyr::mutate(Membership_Type =
             gsub("^\\* ", "", .data$Membership_Type)) %>%
    dplyr::mutate(Enrollment_Contract_Length =
             gsub(" months?", "", .data$Enrollment_Contract_Length)) %>%
    dplyr::mutate(Enrollment_Length_of_Stay =
             gsub(" months?", "", .data$Enrollment_Length_of_Stay)) %>%
    dplyr::mutate(Student_Length_of_Stay =
             gsub(" months?", "", .data$Student_Length_of_Stay)) %>%

    dplyr::mutate(Monthly_Sessions = as.numeric(.data$Monthly_Sessions)) %>%
    dplyr::mutate(Delivery = as.factor(.data$Delivery))

  # Create new columns
  data <- dplyr::mutate(data, Student = paste(.data$Student_First_Name, .data$Student_Last_Name),
                .before = "Student_First_Name")

  # Columns to be removed
  # Session_Length is handled as Duration in getProgressData()
  rm_cols <- c("Student_First_Name", "Student_Last_Name", "Session_Length",
               "Virtual_Center", "Status")
  na_cols <- c()
  # REXAMINE COLUMNS AFTER ABOVE ARE REVIEWED

  # Remove columns
  data <- data %>%
    removeRawCols(rm_cols)
  removeRawCols(na_cols, test_na = T)

  return(data)
}

#' Tidy Radius assessment data
#'
#' @param dat Data frame to tidy
#'
#' @return A data frame
#'
#' @examples
tidyAssessments <- function(dat){
  #Arbitrarily Tidy up
  tdat <- dplyr::transmute(dat,
                           Lead_Id = as.character(.data$Lead_Id),
                           Account_Id = .data$Account_Id,
                           Student= paste(.data$Student_First_Name, .data$Student_Last_Name),
                           Enrollment_Status = as.factor(.data$Enrollment_Status),
                           Grade = dplyr::case_when(
                             .data$Grade == "Pre K" ~ "-1",
                             .data$Grade == "K" ~ "0",
                             .data$Grade == "College" ~ "13",
                             grepl("[0-9]", .data$Grade) ~ .data$Grade,
                             .default = "NaN"),
                           Assessment = .data$Assessment_Title,
                           Level = dplyr::case_when(
                             !(grepl("[A-Z]", toupper(.data$Assessment_Level))&
                                 !is.na(Assessment_Level)) ~ .data$Assessment_Level,
                             grepl("Readiness|Middle",.data$Assessment_Level) ~ "8", #Alg or Geo Readiness is considered 8th
                             grepl("Algebra I A|ACT",.data$Assessment_Level) ~ "9",#Algebra 1 is 9th, 10 & 11 are coded in
                             grepl("SAT Advanced|HMM", .data$Assessment_Level) ~ "12",
                             .default = "NaN"),
                           Percent = .data$Score*100,
                           Date = strptime(.data$Date_Taken, format = "%m/%e/%Y"),
                           Pre = .data$`Pre/Post`=="Pre",
                           Group = .data$Group=="Yes",
                           Center = as.factor(.data$Center)) %>%
    #Avoid NA warning when none is needed
    dplyr::mutate(Grade = as.numeric(.data$Grade),
                  Level = as.numeric(.data$Level))
  return(tdat)
}

#' Delete columns from data frame
#'
#' @param df A data frame
#' @param ... Character strings or vectors of columns to remove
#' @param test_na If `TRUE`, signal error if column to remove in `df` does
#'  not contain only `NA` values.
#'
#' @return A data frame
#'
#' @examples
removeRawCols <- function(df, ..., test_na = F) {
  # Iterate through column names
  for (col_name in unlist(list(...))) {
    # Test columns
    if (!(col_name %in% names(df))) {
      # Column doesn't exist
      stop("Column \'", col_name, "\' does not exist in data frame.")
    } else if ( test_na && !all(is.na(df[[col_name]])) ) {
      # Column not NA but should be
      stop("Column \'", col_name, "\' is expected to be NA but isn't.",
           "\n  Run print_raw_na_cols() to get today's list of NA columns.")
    }

    # Delete column
    df[[col_name]] <- NULL
  }
  return(df)
}
