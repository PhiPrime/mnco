#' Title
#'
#' @param date
#' @param ignoreMissing
#'
#' @return
#' @export
#'
#' @examples
getAssessments <- function(date = Sys.Date(), ignoreMissing = F) {
  #Get using regex since filename depends on range of dates in report
  dat <- readRawData.old(paste0("Assessment Report from [0-9]+_[0-9]+_[0-9]+ ",
                            "to [0-9]+_[0-9]+_[0-9]+"),
                     date, ignoreMissing, regExFile = TRUE)
  tdat <- tidyAssessments(dat)

  tdat <- tdat[order(tdat$Date, decreasing = TRUE),]

  return(tdat)
}

#' Title
#'
#' @param Assessments_Prior_to_
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param Assessments_Prior_to_
#'
#' @return
#' @export
#'
#' @examples
getMostRecentAssessments <- function(Assessments_Prior_to_ = "8_5_2024"){
  dat <- getHistoricAssessments(Assessments_Prior_to_)
  dat <- dplyr::filter(dat, !is.na(.data$Level))
  dat <- dat[order(dat$Student, dat$Level, decreasing = TRUE),]
  ldat <- lapply(unique(dat$Student),
                 with(dat, function(x)dplyr::filter(dat, .data$Student == x)[1,]))
  dat <- data.frame(Reduce(rbind, ldat))
  dat <- dplyr::mutate(dat, yearsSince = as.numeric(round(
    (Sys.time()-.data$Date)/365.25)))
  ret <- dplyr::mutate(dat,
                gradeDif = as.numeric(.data$Level)-as.numeric(.data$Grade))
  return(ret)
}

## Returns vector of names of students need a new deck due to an assessment
#' Title
#'
#' @param date
#'
#' @return
#' @export
#'
#' @examples
needsDeckBasedOnAssessment <- function(date = Sys.time()){

  ## Ways to tell if a deck needs made based on assessments:
  ### 1) Assessment Date is between Last_Attendance_Date and today
  ### 2) Active_Learning_Plans... == 0 & Pre | == 1 & Post | <2 & NF
  ### 3) Save data in a cache and keep track of all assessments for each student

  ## We will use options 1&2
  ret <- NA_character_
  assessments <- getAssessments(date)

  #Option 1
  stus <- dplyr::select(getCenterData("student", date),
                 "Student", "Last_Attendance_Date")
  prog <- dplyr::select(getCenterData("progress", date),
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

}#eof
