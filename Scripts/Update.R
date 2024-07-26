### Libraries
library(tidyverse)
library(lubridate)
library(readxl)

#### Update functions:

### Update.All
Update.All <- function(get = FALSE, date = Sys.Date(), ignoreMissing) {
  # Read excel files
  students <- Update.Students(TRUE, date, ignoreMissing)
  accounts <- Update.Accounts(TRUE, date, ignoreMissing)
  progress <- Update.Progress(TRUE, date, ignoreMissing)
  enrollments <- Update.Enrollments(TRUE, date, ignoreMissing)
  #payments <- Update.Payments(TRUE, date, ignoreMissing)
  #payments <- mutate(payments, Account = Account_Name)
  
  # Prepare students to merge
  students <- mutate(students, Student = paste(First_Name, Last_Name), .before = Student_Id)
  students$First_Name <- NULL
  students$Last_Name <- NULL
  
  # Prepare accounts to merge
  accounts$First_Name <- NULL
  accounts$Last_Name <- NULL
  
  # Merge into one data frame
  all <- mergeWithFill(students, accounts, .by = "Account_Id")
  all <- merge(all, progress, all.x = TRUE)
  all <- merge(all, enrollments, all.x = TRUE)
  #all <- merge(all, payments, all.x = TRUE)
  
  if(get) {
    # Return data frame as getter
    return(all)
  } else {
    # Split by enrollment status
    active <- filter(all, Enrollment_Status == "Enrolled")
    inactive <- filter(all, Enrollment_Status != "Enrolled")
    
    # Assign to global environment
    assign("active",
           filter(all, Enrollment_Status == "Enrolled"),
           envir = .GlobalEnv)
    assign("inactive",
           filter(all, Enrollment_Status != "Enrolled"),
           envir = .GlobalEnv)
  }
}

### mergeWithFill
# Merge columns from 'df2' into 'df1', matching observations based on 
#   the specified columns.
# Common columns not in '.by' are filled with values from 'df1'. 
#   If values are NA, they are filled from 'df2'.
mergeWithFill <- function(df1, df2, .by) {
  # Merge df1 and df2. Common columns are suffixed with .x and .y
  df <- merge(df1, df2, all.x = T, by = .by)
  
  # Iterate through common columns
  for (col in intersect(names(df1), names(df2))) {
    # Skip iteration if column was used to match
    if (col %in% .by) next
    
    # Suffixed column strings
    col.x <- paste0(col, ".x")
    col.y <- paste0(col, ".y")
    
    # Fill value for common column to col.x and rename to col
    df[[col.x]] <- coalesce(df[[col.x]], df[[col.y]])
    names(df)[names(df) == col.x] <- col
    # CHANGE TO THIS? MAYBE DOESN'T WORK
    #df <- rename(df, col = col.x)
    
    # Delete col.y
    df[[col.y]] <- NULL
  }
  
  return(df)
}

### Update.Init
Update.Init <- function(fileRoot, date, ignoreMissing = F, regExFile = FALSE) {
  fileName <- paste0(fileRoot, 
                     paste(month(date), day(date), year(date), sep = "_"), 
                     ".xlsx")
  #If regex find a match with fileRoot in either folder
  if(regExFile) {
    #Compare first to rawFiles,
    rawFiles <- list.files(file.path(getwd(), "Raw_Data"))
    fileOptions <- rawFiles[grepl(fileRoot, rawFiles)]
    
    if(length(fileOptions)==1){
      #If only one is found assign it
      filePath <- fileOptions
    }else if(length(fileOptions)>1){ 
      #If more than 1, error
      stop(paste0("\"",fileRoot, "\" matched with the following files...", 
                  paste("",fileOptions, sep = "\"\n\"", collapse = ""),
                  "\"\n...and does not know how to proceed, ",
                  "be more specific and try again."))
    } else if (length(fileOptions)==0){
      #If not found compare to downloads folder
      downloadPath <- file.path(regmatches(getwd(), 
                                           regexpr("^.*?[/].*?[/].*?(?=/)",
                                                   getwd(), perl = T)), 
                                "Downloads")
      downloadFiles <- list.files(downloadPath)
      fileOptions <- downloadFiles[grepl(fileRoot, downloadFiles)]
      
      
      if(length(fileOptions)==1){
        #If only one is found assign it
        filePath <- fileOptions
      }else if(length(fileOptions)>1){
        #If more than 1 match, error
        stop(paste0("\"",fileRoot, "\" matched with the following files...", 
                    paste("",fileOptions, sep = "\"\n\"", collapse = ""),
                    "\"\n...and does not know how to proceed, ",
                    "be more specific and try again."))
      } else if (length(fileOptions)==0){
        #If no matches stop and error
        stop(paste0("After searching both \"./Raw_Data\",",
                    "and \"./Downloads\" \"", fileRoot, "\" yielded no matches.",
                    " Please try again."))
      }
    }
    dat <- read_excel(filePath, .name_repair = "unique_quiet")
  } else {
    # Default behavior: not regex and filePath should be set or moved 
    filePath <- file.path(getwd(), "Raw_Data", fileName)
    
    fileMoved <- moveDataDownloads(fileName)
    
    if (!fileMoved && !file.exists(filePath)) {
      if (!ignoreMissing) {
        stop("\"", fileName, "\" not found in Raw_Data/ or Downloads/")
      } else {
        emptyFileName <- paste0(fileRoot, "EMPTY", ".xlsx")
        emptyFilePath <- file.path(getwd(), "Raw_Helper", emptyFileName)
        
        dat <- read_excel(emptyFilePath, .name_repair = "unique_quiet")
      }
    } else {
      dat <- read_excel(filePath, .name_repair = "unique_quiet")
    }
  }
  
  names(dat) <- gsub(" ", "_", names(dat))
  return(dat)
}#eof

as.dataFilePath <- function(fileName, date = Sys.Date()){
  #I made one big line  
  return(
    file.path(paste0(getwd(),"/Raw_Data"), 
              paste0(fileName, 
                     paste(lubridate::month(date), 
                           lubridate::day(date), 
                           lubridate::year(date), sep = "_"), 
                     ".xlsx")))
}

### Update.Students
Update.Students <- function(get = FALSE, date = Sys.Date(), ignoreMissing = F){
  #Update Initialize
  dat <- Update.Init("Students Export  ", date, ignoreMissing)
  
  dat <- mutate(dat, 
                Last_Attendance_Date = as.Date(Last_Attendance_Date, 
                                               format = "%m/%d/%Y"))
  #"failed to parse" warning gets thrown
  
  students <- filter(dat, Enrollment_Status == "Enrolled")
  inactiveStudents <- filter(dat, Enrollment_Status != "Enrolled")
  
  if(get){
    return(dat)
  } else {
    assign("students",students,envir = .GlobalEnv)
    assign("inactiveStudents",inactiveStudents,envir = .GlobalEnv)
  }
}#eof

### Update.Accounts
Update.Accounts <- function(get = FALSE, date = Sys.Date(), ignoreMissing = F){
  #Update Initialize
  dat <- Update.Init("Account Export  ", date, ignoreMissing)
  
  accounts <- filter(dat, Enrollment_Status == "Active")
  inactive <- filter(dat, Enrollment_Status == "Inactive")
  
  if(get) {
    return(dat)
  } else {
    assign("accounts",accounts,envir = .GlobalEnv)
    assign("inactiveAccounts",inactive,envir = .GlobalEnv)
  }
}#eof

### Update.Progress
Update.Progress <- function(get = FALSE, date = Sys.Date(), ignoreMissing = F) {
  # Update Initialize
  fileRoot <- "Current Batch Detail Export  "
  filePath <- as.dataFilePath(fileRoot)
  
  # NEED TO REORGANIZE THIS SOMEHOW
  if(!file.exists(filePath)){
    #Try to move from downloads
    moveDataDownloads(gsub(".*/", "", filePath))
    if(!file.exists(filePath)){
      #Check for "bootstrap" files
      fileRoot2 <- "Student Report  "
      dat <- Update.Init(fileRoot2, date, ignoreMissing)
        
      cat("Notice: ", as.dataFilePath(fileRoot2), 
                   "\n\t\tis being used instead of\n\t", filePath, sep="")
      dat <- mutate(dat,
             Student = dat$Student_Name,
             Guardian = dat$Guardians,
             Account = dat$Account_Name, #Unsure of formatting
             Active_Learning_Plans = dat$Active_LPs,
             Attendances = dat$Attendance,
             Skills_Mastered = dat$Skills_Mastered,
             Skills_Currently_Assigned = dat$Skills_Assigned,
             Enrollment_Status = dat$Enrollment_Status,
             BPR_Comment = NA,
             Last_PR_Send_Date = dat$Last_PR_Sent,
             Email_Opt_Out = NA,
      )
      }
    }#filePath should exist
  
  if(file.exists(filePath)) {
    dat <- Update.Init(fileRoot, date, ignoreMissing)
  }
  
  
  #merge qualifying students from tdat back into dat
  ###
  
  
  if(get) {
    return(dat)
  } else {
    tdat <- getStudentRanking(date)
    assign("studentProgress",dat,envir = .GlobalEnv)
    assign("selectStudentProgress", tdat, envir = .GlobalEnv)
    assign("studentRanking", select(tdat, 
                                    Rank, Student, fontsize, UB, Pest, LB, 
                                    Skills_Mastered, Attendances), envir = .GlobalEnv)
  }
}#eof

getStudentRanking <- function(date = Sys.Date()){
  dat <- Update.Progress(TRUE, date)
  
  #Merge in delivery record from Enrollments
  deliveryKey <- mutate(Update.Enrollments(TRUE, date), 
                        Student = paste(Student_First_Name, 
                                        Student_Last_Name),
                        Delivery = as.factor(Delivery),
                        Monthly_Sessions = as.numeric(Total_Sessions)) %>%
    select(Student, Delivery, Monthly_Sessions)
  
  dat <- merge(dat,deliveryKey)
  
  #Sort and select desired data
  # Add column for session duration
  differentDurationStudents <- read.csv("Cache/differentDurationStudents.csv")
  dat <- merge(dat, differentDurationStudents, all.x = T)
  dat$Duration <- coalesce(dat$Duration, 60)
  dat <- mutate(dat, Attendances = Attendances * Duration/60)
  
  #Subset valid contestants
  dat <- dat[which(dat$Attendances >= dat$Monthly_Sessions/2 & 
                     dat$Skills_Mastered>2 & 
                     dat$Delivery == "In-Center"),]
  
  #Create statistics for based on CI
  CI <- 95
  outlierThreshold <- 4
  roundingDig <- 4
  dat <- mutate(dat, Pest = Skills_Mastered/Attendances)
  
  #Outlier test
  dat <- mutate(dat, 
                zscore = (mean(dat$Pest)-Pest)/
                  (sd(dat$Pest)/sqrt(Attendances)))
  #Get sd without outliers
  samdev <- sd(dat$Pest[dat$zscore<outlierThreshold])
  dat <- mutate(dat, 
                UB = round(Pest-qnorm((1-CI/100)/2)*
                             samdev/sqrt(Attendances),roundingDig),
                LB = round(Pest+qnorm((1-CI/100)/2)*
                             samdev/sqrt(Attendances),roundingDig))
  dat <- mutate(dat, fontsize = round(32*LB/max(dat$LB), 1))
  dat <- dat[order(dat$LB, decreasing = TRUE),]
  dat <- mutate(dat, Rank = dim(dat)[1] +1 - rank(LB, ties.method = "max"))
  
  
  dat <- mutate(dat, rankSuffix = ifelse(grepl("[2-9]?1$", 
                                               as.character(Rank)), "st",
                                         ifelse(grepl("[2-9]?2$",
                                                      as.character(Rank)), "nd",
                                                ifelse(grepl("[2-9]?3$",
                                                             as.character(Rank)),
                                                       "rd","th")))) %>%
    mutate(rankDisplay = paste0(Rank, rankSuffix))
  
  #Select reasonable data
  # dat <- select(dat, )
  
  return(dat)
}#eof

### Update.Enrollments
Update.Enrollments <- function(get = FALSE, date = Sys.Date(), ignoreMissing = F) {
  #Update Initialize
  dat <- Update.Init("Enrolled Report  ", date, ignoreMissing)
  
  if (get) {
    return(dat)
  } else {
    assign("enrollments",dat,envir = .GlobalEnv)
  }
}#eof

### Update.Payments
Update.Payments <- function(get = FALSE, date = Sys.Date(), ignoreMissing = F){
  #Update Initialize
  dat <- Update.Init("Payments.xlsx  ", date)
  
  if(get) {
    return(dat)
  } else {
    assign("payments", dat,envir = .GlobalEnv)
  }
}

### Update.Curriculum
Update.Curriculum <- function(get = FALSE, date = Sys.Date()){
  #Update Initialize
  dat <- Update.Init("Curriculum Library Export  ", date)
  
  if(get) {
    return(dat)
  } else {
    assign("curriculumDatabase",dat,envir = .GlobalEnv)
  }
  
}#eof

getHistoricAttendance <- function(){
  return(readRDS(file.path(paste0(getwd(),
                                   "/Cache"), "prior2024.rds")))
}

### Update.Attendance
Update.Attendance <- function(get = FALSE, date = Sys.Date()) {
  #Update Initialize
  dat <- Update.Init("Student Attendance Report Export  ", date)
  
  logfile <- file.path(getwd(), "Cache", "studentAttendanceLog.csv")

  if(!file.exists(logfile)) {
    stop("While running Update.Attendance(), \"", logfile,
                "\" was not found.")
  } else {
    logdat <- read.csv(logfile)
  }
  
    #Mutate to tidy
    dat <- mutate(dat,
                  date = as.Date(Attendance_Date,
                                 format = "%m/%d/%y"),
                  accountID = Account_Id,
                  name = paste(First_Name,Last_Name),
                  startTime = strptime(Arrival_Time, "%I:%M %p"),
                  endTime = strptime(Departure_Time, "%I:%M %p"),
                  totalVisits = Total_Visits,
                  membershipType = as.factor(Membership_Type),
                  sessionsPerMonth = as.factor(Sessions_Per_Month),
                  sessionsRemaining = Sessions_Remaining,
                  delivery = as.factor(Delivery))
    
    dat <- mutate(dat,
                  line = paste(accountID,date,name, 
                               sep = ";"))
    
    dat <- select(dat, date:delivery, line)
  
  
  newdat <- dat[!(dat$line %in% logdat$line),]
  
  #Check for and notify if no new data is found
  if(dim(newdat)[1]==0){
    cat("Notice: Update.Attendance(get = ", get, ", date = ", as.character(date),
      ") found no new attendance when updating.", sep = "")
  } 
  
  else {
    # write.csv()
  }
  
  
  #Save to File
  write.csv(logdat,logfile, row.names=FALSE)
  
  if(get){
    return(read.csv(logfile))
  }
  
  ### Old code to check for two types of files. Could be useful to 
  ### convert stored data from xlsx to csv for better longterm storage.
  # if(grepl("xlsx$", fileLoc)) {
  #   newdat <- read_xlsx(fileLoc)
  # } else if (grepl("csv$", fileLoc)) {
  #   newdat <- read.csv(fileLoc)
  # } else {
  #   stop(paste0("While running Update.Attendance ", fileLoc, 
  #               "was not able to be read."))
  # }
  
  
}#eof

getAssessments <- function(updateGlobal = FALSE, 
                           date = Sys.Date(), ignoreMissing = F){
  #Update Initialize
  dat <- Update.Init("Students Export  ", date, ignoreMissing, regExFile = TRUE)
  
  dat <- mutate(dat, 
                Last_Attendance_Date = as.Date(Last_Attendance_Date, 
                                               format = "%m/%d/%Y"))
  #"failed to parse" warning gets thrown
  
  students <- filter(dat, Enrollment_Status == "Enrolled")
  inactiveStudents <- filter(dat, Enrollment_Status != "Enrolled")
  
  if(get){
    return(dat)
  } else {
    assign("students",students,envir = .GlobalEnv)
    assign("inactiveStudents",inactiveStudents,envir = .GlobalEnv)
  }
}#eof




moveDataDownloads <- function(fileNames) {
  downloadPath <- file.path(regmatches(getwd(), regexpr("^.*?[/].*?[/].*?(?=/)", 
                                          getwd(), perl = T)), "Downloads")
  filePaths <- file.path(downloadPath, fileNames)
  
  if (!file.exists(downloadPath)) stop("Downloads folder not found at \"", downloadPath, "\"")
  
  if(!grepl("Overview$", getwd())) {
    stop("While trying to moveDataDownloads, \"",
         getwd(),
         "\"\n\tis the working directory but does not end with \"Overview\"")
  }
  
  fileDests <- file.path(getwd(), "Raw_Data", fileNames)
  fileMoved <- F
  
  for(i in 1:length(filePaths)) {
    if (file.exists(filePaths[i])) {
      file.rename(filePaths[i], fileDests[i])
      cat("Notice: ", filePaths[i], "\n\t\t-- moved to -->\n\t", fileDests[i], sep="")
      fileMoved <- T
    }
  }
  
  return(fileMoved)
  
}#eof

# Put this into a function?
# write.csv(data.frame(Student = character(), Duration = numeric()), "Cache/differentDurationStudents.csv", row.names = F)

addDifferentDurationStudent <- function(student, duration) {
  # need to add file check
  filePath <- file.path(getwd(), "Cache/differentDurationStudents.csv")
  dat <- read.csv(filePath)
  
  dat <- rbind(dat, data.frame(Student = student, Duration = duration))
  write.csv(dat, filePath, row.names = F)
}

removeDifferentDurationStudent <- function(student) {
  # need to add file check
  filePath <- file.path(getwd(), "Cache/differentDurationStudents.csv")
  dat <- read.csv(filePath)
  
  dat <- dat[dat$Student != student,]
  write.csv(dat, filePath, row.names = F)
}
