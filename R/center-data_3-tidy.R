#' Tidy Radius data
#'
#' @param data Data frame to tidy
#' @param type Radius data file type
#'
#' @return A tidied data frame
#' @keywords internal
tidyRawData <- function(data, type) {
  switch (type,
    "student"    = tidyRawData.student(data),
    "account"    = tidyRawData.account(data),
    "progress"   = tidyRawData.progress(data),
    "enrollment" = tidyRawData.enrollment(data),
    "assessment" = tidyRawData.assessment(data),
    "payment"    = tidyRawData.payment(data),
    "curriculum" = tidyRawData.curriculum(data),
    "attendance" = tidyRawData.attendance(data),
    "template"   = tidyRawData.template(data),
    "lead"       = tidyRawData.lead(data),
    "student2"   = tidyRawData.student2(data),
    stop("`type` is not a valid argument: \'", type, "\'")
  )
}

# Individual data tidying functions
tidyRawData.student <- function(data) {
  # Create new columns
  data <- mutate(data, Student = paste(.data$First_Name, .data$Last_Name), .before = "Student_Id")

  # Rename columns
  data <- data %>%
    dplyr::rename(
      Student_First = .data$First_Name,
      Student_Last  = .data$Last_Name,
      Lead_Id       = .data$Lead_Id...2,
      Student_Created_Date = Created_Date
    )

  # Reformat columns
  data <- data %>%
    mutate(
      Last_Attendance_Date = as.Date(.data$Last_Attendance_Date, format = "%m/%d/%Y"),
      Date_of_Birth = as.Date(.data$Date_of_Birth, format = "%m/%d/%Y"),
      Card_Level = as.integer(.data$Card_Level),
      Student_Created_Date = as.Date(.data$Student_Created_Date),
      Grade = dplyr::case_when(
        .data$Grade == "Pre K"   ~ "-1",
        .data$Grade == "K"       ~ "0",
        .data$Grade == "College" ~ "13",
        .default = .data$Grade
      ),
      Grade = as.integer(.data$Grade)
    )

  # Columns to be removed
  # ORGANIZE rm_cols
  # COULD MAKE FLAG FOR CARD LEVEL UPGRADE
  rm_cols <- c("Account", "School_Year",
               #Next line is from new items as of 11/11/2025 that are not
               # accessible from radius
               "Preferred_Name", "Lead_Id...25",   "Pronouns",
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

  invisible(data)
}

tidyRawData.account <- function(data) {
  # Create new columns
  data <- mutate(data, Account = paste(.data$First_Name, .data$Last_Name), .before = "Account_Id")

  data <- data %>%
    dplyr::rename(
      Account_First = .data$First_Name,
      Account_Last  = .data$Last_Name,
      Account_Created_Date = Created_Date
    ) %>%
    mutate(
      Account_Created_Date = as.Date(Account_Created_Date)
    )

  # Columns to be removed
  rm_cols <- c("Last_Modified_By...20",
               "Last_Modified_By...33", "Description", "Customer_Comments",
               "Last_Modified_Date", "Emergency_Phone_Number",
               "Emergency_Contact", "Account_Relation")
  na_cols <- c("Date_of_Birth", "Last_TriMathlon_Reg._Date")
  # REXAMINE COLUMNS AFTER ABOVE ARE REVIEWED

  # Remove columns
  # data <- removeRawCols(data, rm_cols)
  data <- removeRawCols(data, na_cols, test_na = T)

  invisible(data)
}

tidyRawData.progress <- function(data) {
  {#The following is a hot fix to make the updated export be converted into
    # the old output so production can continue without a full fix being
    # implemented

    # Get These:
    #
    # [1] "Student"                   "Guardian"
    # [3] "Account"                   "LP Name"
    # [5] "Attendances"               "Skills Mastered"
    # [7] "Skills Currently Assigned" "Total Skills Mastered"
    # [9] "Total Skills"              "% Skills Mastered"
    # [11] "Enrollment Status"         "BPR Comment"
    # [13] "BPR Include LP Report"     "Last Student PR Send Date"
    # [15] "Last PR Send Date"         "Exclude from This Batch"
    # [17] "Email Opt Out"

    # And make them into These:

    # [1] "Student"                  "Guardian"                 "Account"
    # [4] "Active Learning Plans"    "Attendances"              "Skills Mastered"
    # [7] "Total LP Skills"          "Total LP Skills Mastered" "Enrollment Status"
    # [10] "BPR Comment"              "Last PR Send Date"        "Email Opt Out"
    sret <- t(sapply(unique(data$Student), function(s){
      sdat <- filter(data, Student == s)
      ret <- data.frame(Student = sdat$Student[1],
                        Guardian = sdat$Guardian[1],
                        Account = sdat$Account[1],
                        Active_Learning_Plans = dim(sdat)[1],
                        Attendances = sdat$Attendances[1],
                        Skills_Mastered = sum(sdat$Skills_Mastered),
                        Total_LP_Skills = sum(sdat$Total_Skills),
                        Total_LP_Skills_Mastered = sum(sdat$Total_Skills_Mastered),
                        Enrollment_Status = sdat$Enrollment_Status[1],
                        BPR_Comment = sdat$BPR_Comment[1],
                        Last_PR_Send_Date = sdat$Last_PR_Send_Date[1],
                        Email_Opt_Out = sdat$Email_Opt_Out[1])
      return(ret)}))
    data <- sret
    }

  data <- data %>%
    mutate(dplyr::across(
      "Skills_Mastered",
      ~tidyr::replace_na(.x, 0)
    ))


  rm_cols <- c("Guardian")
  na_cols <- c("BPR_Comment")

  maybe_cols <- c("Email_Opt_Out")

  # Remove columns
  data <- removeRawCols(data, rm_cols)
  data <- removeRawCols(data, na_cols, test_na = T)

  invisible(data)
}

tidyRawData.enrollment <- function (data) {
  # Rename columns
  names(data)[names(data) == "Account_Name"] <- "Account"
  names(data)[names(data) == "Total_Sessions"] <- "Monthly_Sessions"
  names(data)[names(data) == "Primary_Enrollment_End"] <- "Contract_End_Date"

  # Reformat columns
  data <- data %>% mutate(
    Membership_Type = gsub("^\\* ", "", .data$Membership_Type),
    Enrollment_Contract_Length = gsub(" months?", "", .data$Enrollment_Contract_Length) %>% as.numeric(),
    Enrollment_Length_of_Stay  = gsub(" months?", "", .data$Enrollment_Length_of_Stay)  %>% as.numeric(),
    Student_Length_of_Stay     = gsub(" months?", "", .data$Student_Length_of_Stay)     %>% as.numeric(),

    Monthly_Sessions = as.numeric(.data$Monthly_Sessions),
    Delivery = as.factor(.data$Delivery),

    Grade = dplyr::case_when(
      .data$Grade == "Pre K"   ~ "-1",
      .data$Grade == "K"       ~ "0",
      .data$Grade == "College" ~ "13",
      .default = .data$Grade
    ),
    Grade = as.integer(.data$Grade)
  )

  # Create new columns
  data <- mutate(data, Student = paste(.data$Student_First_Name, .data$Student_Last_Name),
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

  invisible(data)
}

tidyRawData.assessment <- function(data) {
  data <-  data %>%
    transmute(
      Lead_Id    = .data$Lead_Id,
      Account_Id = .data$Account_Id,
      Student    = paste(.data$Student_First_Name, .data$Student_Last_Name),
      Enrollment_Status = as.factor(.data$Enrollment_Status),
      Grade = dplyr::case_when(
        .data$Grade == "Pre K"   ~ "-1",
        .data$Grade == "K"       ~ "0",
        .data$Grade == "College" ~ "13",
        .default = .data$Grade
      ),
      Grade = as.integer(.data$Grade),
      Assessment = .data$Assessment_Title,
      Level      = dplyr::case_when(
        !(grepl("[A-Z]", toupper(.data$Assessment_Level)) &
            !is.na(Assessment_Level)) ~ .data$Assessment_Level,
        grepl("Readiness|Middle",.data$Assessment_Level) ~ "8", #Alg or Geo Readiness is considered 8th
        grepl("Algebra I A|ACT",.data$Assessment_Level) ~ "9",#Algebra 1 is 9th, 10 & 11 are coded in
        grepl("SAT Advanced|HMM", .data$Assessment_Level) ~ "12",
        .default = "NaN"),
      Percent    = .data$Score * 100,
      # RENAME THIS LATER - WILL CONFLICT WITH SAVE FUNCTIONS
      Date_Taken = as.Date(.data$Date_Taken, format = "%m/%d/%Y"),
      Pre        = .data$`Pre/Post` == "Pre",
      Group      = .data$Group == "Yes",
      Center     = as.factor(.data$Center)) %>%
    #Avoid NA warning when none is needed
    mutate(
      Grade = as.numeric(.data$Grade),
      Level = as.numeric(.data$Level)
    ) %>%
    dplyr::arrange(desc(.data$Date_Taken))
  invisible(data)
}

tidyRawData.payment <- function(data) {
  # TIDYING GOES HERE

  invisible(data)
}

tidyRawData.curriculum <- function(data) {
  # TIDYING GOES HERE

  invisible(data)
}

tidyRawData.attendance <- function(data) {
  # Reformat columns
  data <- data %>%
    mutate(
      .keep = "unused",
      .after = "Account_Id",
      Student = paste(.data$First_Name, .data$Last_Name)
    ) %>%
    mutate(
      Attendance_Date   = as.Date(.data$Attendance_Date, format = "%m/%d/%Y"),
      Arrival_Time      = strptime(.data$Arrival_Time, "%I:%M %p"),
      Departure_Time    = strptime(.data$Departure_Time, "%I:%M %p")
    )

  # Remove columns
  rm_cols <- c("Total_Hours")

  data <- data %>%
    removeRawCols(rm_cols)

  invisible(data)
}

tidyRawData.template <- function(data) {
  # TIDYING GOES HERE

  invisible(data)
}

tidyRawData.lead <- function(data) {
  ret <- dplyr::transmute(data,
                          Lead_Id = as.character(.data$Lead_Id),
                          Lead_Name = .data$Lead_Name,
                          Lead_First_Name = stringr::str_extract(.data$Lead_Name,
                                                                 "[^ ]+"),
                          Lead_Last_Name =
                            stringr::str_sub(stringr::str_extract(.data$Lead_Name,
                                                                 " .+"),2),
                          Lead_Creation_Date = as.Date(.data$Created_Date,
                                                       format = "%m/%d/%Y"),
                          Phone = .data$Mobile_Phone,
                          Email = .data$Email,
                          Status = as.factor(.data$Lead_Status),
                          Student_Name = .data$Student_Name,
                          Student_First_Name = stringr::str_extract(.data$Student_Name,
                                                                 "[^ ]+"),
                          Student_Last_Name =
                            stringr::str_sub(stringr::str_extract(.data$Student_Name,
                                                                 " .+"),2),
                          Student_Gender = .data$Student_Gender,
                          Grade = as.factor(.data$Grade),
                          Source = as.factor(.data$Lead_Source),
                          Radius_Page = paste0(
                            "https://radius.mathnasium.com/Leads/Details/",
                            .data$Lead_Id))
  return(ret)
}

tidyRawData.student2 <- function(data) {
  data <- data %>%
    dplyr::rename(
      Student = .data$Student_Name,
      Active_Skills_Mastered = .data$Skills_Mastered
    ) %>%
    mutate(
      Grade = dplyr::case_when(
        .data$Grade == "Pre K"   ~ "-1",
        .data$Grade == "K"       ~ "0",
        .data$Grade == "College" ~ "13",
        .default = .data$Grade
      ),
      Grade = as.integer(.data$Grade)
    )

  invisible(data)
}

#' Delete columns from data frame
#'
#' @param df A data frame
#' @param ... Character strings or vectors of columns to remove
#' @param test_na If `TRUE`, signal error if column to remove in `df` does
#'  not contain only `NA` values.
#'
#' @return A data frame
#' @noRd
#'
#' @examples
#' stu <- readRawData("student")
#' stu <- stu %>% removeRawCols("Scholarship", "Center")
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

# MODIFY TO ALLOW ITERATION THROUGH MULTIPLE DATES?
#' Get NA column names
#'
#' @param date Date to read data for
#'
#' @return A vector of column names
#'
#' @examples
#' get_raw_na_cols()
get_raw_na_cols <- function(date = Sys.Date()) {
  rootType <- c("student", "account", "progress", "enrollment")

  # Iterate through each raw data file for given date
  na_col_list <- list()
  for (i in 1:4) {
    dat <- readRawData(rootType[i], date)

    # Get and append names of NA columns to list
    na_col <- sapply(dat, function(x) all(is.na(x)))
    na_col_names <- names(na_col)[na_col]
    na_col_list[[i]] <- na_col_names
  }
  names(na_col_list) <- fileRoots

  return(na_col_list)
}

#' Prints names of NA columns in raw data files
#'
#' The column names are formatted for copy/paste into a vector in code
#'
#' @param date Date to read data for
#'
#' @return None (invisible `NULL`)
#'
#' @examples
#' print_raw_na_cols
print_raw_na_cols <- function(date = Sys.Date()) {
  na_col_list = get_raw_na_cols(date)

  for (col_name in names(na_col_list)) {
    message(col_name, ": \"", paste0(na_col_list[[col_name]], collapse = "\", \""), "\"")
  }

  invisible(NULL)
}
