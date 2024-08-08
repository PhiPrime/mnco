#######################     ASSESSMENT FUNCTIONS     ########################

getAssessments <- function(date = Sys.Date(), ignoreMissing = F) {
  #Get using regex since filename depends on range of dates in report
  dat <- readRawData(paste0("Assessment Report from [0-9]+_[0-9]+_[0-9]+ ",
                            "to [0-9]+_[0-9]+_[0-9]+"),
                     date, ignoreMissing, regExFile = TRUE)
  tdat <- tidyAssessments(dat)

  tdat <- tdat[order(tdat$Date, decreasing = TRUE),]

  return(tdat)
}#eof

getHistoricAssessments <- function(Assessments_Prior_to_ = "8_5_2024") {
  relPath <- paste0("./Raw_Data/Assessments_Prior_to_",
                    Assessments_Prior_to_,
                    ".xlsx")

  #Checks for file and throws descriptive error
  if(!file.exists(relPath)){
    stop(paste0("Historic Assessment data for ", gsub("_","-", Assessments_Prior_to_),
                " not found at\n\t\t", getwd(),
                "\n\t\t", gsub("^.", "", relPath), "\n\t",
                "Please rename the relevant data to that location, then try again.\n"))
  }
  dat <- read_excel(relPath, .name_repair = "unique_quiet")
  names(dat) <- gsub(" ", "_", names(dat))

  tdat <- tidyAssessments(dat)
  return(tdat)
}

tidyAssessments <- function(dat){
  #Arbitrarily Tidy up
  tdat <- transmute(dat,
                    Lead_Id = as.character(Lead_Id),
                    Account_Id = Account_Id,
                    Student= paste(Student_First_Name, Student_Last_Name),
                    Enrollment_Status = as.factor(Enrollment_Status),
                    Grade = as.numeric(ifelse(grepl("[0-9]", Grade),
                                              as.numeric(Grade),
                                              ifelse(Grade == "Pre K", -1,
                                                     ifelse(Grade == "K", 0,
                                                            ifelse(Grade == "College", 13, NaN))))),
                    Assessment = Assessment_Title,
                    Level = ifelse(grepl("[A-Z]", toupper(Assessment_Level))&!is.na(Assessment_Level),
                                   ifelse(grepl("Readiness|Middle",Assessment_Level),
                                          8, #Alg or Geo Readiness is considered 8th
                                          ifelse(grepl("Algebra I A|ACT",Assessment_Level),
                                                 9,#Algebra 1 is 9th, 10 & 11 are coded in
                                                 ifelse(grepl("SAT Advanced|HMM", Assessment_Level),
                                                        12, NaN))),
                                   as.numeric(Assessment_Level)),
                    Percent = Score*100,
                    Date = strptime(Date_Taken, format = "%m/%e/%Y"),
                    Pre = `Pre/Post`=="Pre",
                    Group = Group=="Yes",
                    Center = as.factor(Center))
  return(tdat)
}

getMostRecentAssessments <- function(Assessments_Prior_to_ = "8_5_2024"){
  dat <- getHistoricAssessments(Assessments_Prior_to_)
  dat <- filter(dat, !is.na(dat$Level))
  dat <- dat[order(dat$Student, dat$Level, decreasing = TRUE),]
  ldat <- lapply(unique(dat$Student),
                 with(dat, function(x)filter(dat, Student == x)[1,]))
  dat <- data.frame(Reduce(rbind, ldat))
  dat <- mutate(dat, yearsSince = as.numeric(round(
    (Sys.time()-dat$Date)/365.25)))
  ret <- mutate(dat,
                gradeDif = as.numeric(Level)-as.numeric(Grade))
  return(ret)
}

## Returns vector of names of students need a new deck due to an assessment
needsDeckBasedOnAssessment <- function(date = Sys.time()){

  ## Ways to tell if a deck needs made based on assessments:
  ### 1) Assessment Date is between Last_Attendance_Date and today
  ### 2) Active_Learning_Plans... == 0 & Pre | == 1 & Post | <2 & NF
  ### 3) Save data in a cache and keep track of all assessments for each student

  ## We will use options 1&2
  ret <- NA_character_
  assessments <- getAssessments(date)

  #Option 1
  stus <- select(getStudentData(date),
                 Student, Last_Attendance_Date)
  prog <- select(getProgressData(date),
                 Student, Active_Learning_Plans)
  assessments <- merge(assessments, stus) %>%
    merge(prog)

  #Option 1
  ret <- c(ret,
           assessments$Student[
             assessments$Date>=assessments$Last_Attendance_Date])

  #Option 2
  ret <- c(ret,
           assessments$Student[with(assessments,
                                    (Active_Learning_Plans==0)|
                                      (Active_Learning_Plans==1)&!Pre)])

  return(ret)

}#eof


###########################     OTHER SOURCES     ###########################
source("./Scripts/Update.R")
source("./Scripts/Misc.R")
