#' Read historic assessment data
#'
#' @param Assessments_Prior_to_ Radius style date
#'
#' @return A data frame
#' @export
#'
#' @examples
#' getHistoricAssessments()
#' getHistoricAssessments(Assessments_Prior_to_ = "7_31_2024")
getHistoricAssessments <- function(Assessments_Prior_to_ = "8_5_2024") {
  relPath <- file.path(rawDataDir(), paste0("Assessments_Prior_to_",
                    Assessments_Prior_to_,
                    ".xlsx"))

  #Checks for file and throws descriptive error
  if(!file.exists(relPath)){
    stop(paste0("Historic Assessment data for ", gsub("_","-", Assessments_Prior_to_),
                " not found at\n\t\t", getwd(),
                "\n\t\t", gsub("^.", "", relPath), "\n\t",
                "Please rename the relevant data to that location, then try again.\n"))
  }
  dat <- readxl::read_excel(relPath, .name_repair = "unique_quiet")
  names(dat) <- gsub(" ", "_", names(dat))

  tdat <- tidyAssessments(dat)
  return(tdat)
}

#' Get most recent assessment
#'
#' @param Assessments_Prior_to_ Radius style date
#'
#' @return A data frame
#' @export
#'
#' @examples
#' getMostRecentAssessments()
#' getMostRecentAssessments(Assessments_Prior_to_ = "8_5_2024")
getMostRecentAssessments <- function(Assessments_Prior_to_ = "8_5_2024"){
  dat <- getHistoricAssessments(Assessments_Prior_to_)
  dat <- filter(dat, !is.na(.data$Level))
  dat <- dat[order(dat$Student, dat$Level, decreasing = TRUE),]
  ldat <- lapply(unique(dat$Student),
                 with(dat, function(x)filter(dat, .data$Student == x)[1,]))
  dat <- data.frame(Reduce(rbind, ldat))
  dat <- mutate(dat, yearsSince = as.numeric(round(
    (Sys.time()-.data$Date)/365.25)))
  ret <- mutate(dat,
                gradeDif = as.numeric(.data$Level)-as.numeric(.data$Grade))
  return(ret)
}

#' Needs deck based on assessment
#'
#' @param date Date to look at data for
#'
#' @return A [`character`] vector of students
#' @export
#'
#' @examples
#' needsDeckBasedOnAssessment()
needsDeckBasedOnAssessment <- function(date = Sys.time()){

  ## Ways to tell if a deck needs made based on assessments:
  ### 1) Assessment Date is between Last_Attendance_Date and today
  ### 2) Active_Learning_Plans... == 0 & Pre | == 1 & Post | <2 & NF
  ### 3) Save data in a cache and keep track of all assessments for each student

  ## We will use options 1&2
  ret <- NA_character_
  assessments <- getCenterData("assessment", date)

  #Option 1
  stus <- select(getCenterData("student", date),
                 "Student", "Last_Attendance_Date")
  prog <- select(getCenterData("progress", date),
                 "Student", "Active_Learning_Plans")
  assessments <- merge(assessments, stus) %>%
    merge(prog)

  #Option 1
  ret <- c(ret,
           assessments$Student[with(assessments,
            {Date>=Last_Attendance_Date | is.na(Last_Attendance_Date)})])

  #Option 2
  ret <- c(ret,
           assessments$Student[with(assessments,
                                    (Active_Learning_Plans==0)|
                                      (Active_Learning_Plans==1)&!Pre)])

  return(ret)

}
