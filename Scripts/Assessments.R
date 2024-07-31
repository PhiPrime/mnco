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
  
  
  return(tdat)
}#eof



###########################     OTHER SOURCES     ###########################
source("./Scripts/Update.R")
source("./Scripts/Misc.R")