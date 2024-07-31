#######################     ASSESSMENT FUNCTIONS     ########################

getAssessments <- function(date = Sys.Date(), ignoreMissing = F) {
  #Get using regex since filename depends on range of dates in report
  dat <- readRawData(paste0("Assessment Report from [0-9]+_[0-9]+_[0-9]+ ",
                            "to [0-9]+_[0-9]+_[0-9]+"), 
                     date, ignoreMissing, regExFile = TRUE)
  
  #Arbitrarily Tidy up
  tdat <- transmute(dat,
                    Lead_Id = as.character(Lead_Id),
                    Account_Id = Account_Id,
                    Student= paste(Student_First_Name, Student_Last_Name),
                    Enrollment_Status = as.factor(Enrollment_Status),
                    Grade = as.factor(Grade),
                    Assessment = Assessment_Title,
                    Level = as.factor(Assessment_Level),
                    Percent = Score*100,
                    Date = strptime(Date_Taken, format = "%m/%e/%Y"),
                    Pre = `Pre/Post`=="Pre",
                    Group = Group=="Yes",
                    Center = as.factor(Center))
  tdat <- tdat[order(tdat$Date, decreasing = TRUE),]
  
  return(tdat)
}#eof

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
             assessments$Date>assessments$Last_Attendance_Date])
  
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
