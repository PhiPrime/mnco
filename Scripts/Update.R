### Libraries
library(tidyverse)
library(lubridate)
library(readxl)

#### Update functions:

### getCenterData
# Returns data frame containing center data for a given date
getCenterData <- function(date = Sys.Date(), ignoreMissing = F) {
  # Read and process raw data
  students <- getStudentData(date, ignoreMissing)
  accounts <- getAccountData(date, ignoreMissing)
  progress <- getProgressData(date, ignoreMissing)
  enrollments <- getEnrollmentData(date, ignoreMissing)
  
  # Merge into one data frame
  # NEED TO EXAMINE MERGING
  all <- mergeWithFill(students, accounts, .by = "Account_Id")
  all <- merge(all, progress, all.x = TRUE)
  all <- merge(all, enrollments, all.x = TRUE)
  
  return(invisible(all))
}

### mergeWithFill
# Merge columns from 'df2' into 'df1', matching rows based on common columns
#   in '.by'.
# Common columns not in '.by' are filled with values from 'df1'. If values 
#   are NA, they are filled from 'df2'.
mergeWithFill <- function(df1, df2, .by) {
  # Merge df1 and df2. Common columns are suffixed with .x and .y
  df <- merge(df1, df2, all.x = T, by = .by)
  
  # Iterate through common columns
  for (colName in intersect(names(df1), names(df2))) {
    # Skip iteration if column was used to match
    if (colName %in% .by) next
    
    # Suffixed column strings
    colName.x <- paste0(colName, ".x")
    colName.y <- paste0(colName, ".y")
    
    # Use NA if column is empty
    # DOES NOT WORK ON EMPTY DATA FRAMES YET
    # NEED TO PROPERLY MERGE DATA IN getCenterData() THEN FIND SOLUTION
    #col.x <- ifelse(!identical(df[[colName.x]], logical(0)), df[[colName.x]], NA)
    #col.y <- ifelse(!identical(df[[colName.y]], logical(0)), df[[colName.y]], NA)
    #col.x <- df[[colName.x]]
    #col.y <- df[[colName.y]]
    
    # Fill value for common column to col.x and rename to col
    df[[colName.x]] <- coalesce(df[[colName.x]], df[[colName.y]])
    names(df)[names(df) == colName.x] <- colName
    # CHANGE TO THIS? MAYBE DOESN'T WORK
    #df <- rename(df, col = col.x)
    
    # Delete col.y
    df[[colName.y]] <- NULL
  }
  
  return(df)
}

### removeRawCols
# Deletes columns from output of readRawData()
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

### readRawData
# Reads raw data excel file from Radius. If the file is in Downloads, it is
#   moved to Raw_Data directory.
readRawData <- function(fileRoot, date, ignoreMissing = F, regExFile = F) {
  # Format file root into Radius style file name
  fileName <- as.rawFileName(fileRoot, date)
  filePath <- NULL
  
  #If regex find a match with fileRoot in either folder
  #If regex find a match with fileRoot in either folder
  if(regExFile) {
    #Compare first to rawFiles,
    rawFiles <- list.files(file.path(getwd(), "Raw_Data"))
    fileOptions <- rawFiles[grepl(fileRoot, rawFiles)]
    
    if(length(fileOptions)==1){
      #If only one is found assign it
      fileRoot <- fileOptions
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
        fileRoot <- fileOptions
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
      }}
  }
    # Default behavior: attempt to move file from Downloads, then look
    #   in Raw_Data
    filePath <- file.path(getwd(), "Raw_Data", fileName)
    fileMoved <- moveDataDownloads(fileName)
    
    if (!fileMoved && !file.exists(filePath)) {
      # File not found in Downloads or Raw_Data
      if (!ignoreMissing) {
        stop("\"", fileName, "\" not found in Raw_Data or Downloads directories")
      } else {
        # Set filePath to read empty version of the raw data file
        emptyFileName <- paste0(fileRoot, "EMPTY", ".xlsx")
        filePath <- file.path(getwd(), "Raw_Helper", emptyFileName)
      }
    }
  
  
  # Read file and reformat column names to prevent bad behaviors
  dat <- read_excel(filePath, .name_repair = "unique_quiet")
  names(dat) <- gsub(" ", "_", names(dat))
  
  return(dat)
}#eof

### as.rawFileName
# Formats raw data file root as Radius style file name
as.rawFileName <- function(file_root, date = Sys.Date()){
  paste0(file_root, "  ", paste(month(date), day(date), year(date), sep = "_"), 
         ".xlsx")
}

### getStudentData
# Returns data processed from Radius's "Students Export" file
getStudentData <- function(date = Sys.Date(), ignoreMissing = F){
  dat <- readRawData("Students Export", date, ignoreMissing)
  
  # Rename columns
  names(dat)[names(dat) == "Lead_Id...2"] <- "Lead_Id"
  
  # Reformat columns
  dat <- mutate(dat, Last_Attendance_Date = as.Date(Last_Attendance_Date, format = "%m/%d/%Y"))
  
  # Create new columns
  dat <- mutate(dat, Student = paste(First_Name, Last_Name), .before = Student_Id)
  
  # Columns to be removed
  rm_cols <- c("First_Name", "Last_Name", "School_Year", "Lead_Id...24", 
               "Created_By", "Card_Level", "Stars_on_Current_Card",
               "Cards_Available")
  na_cols <- c("Billing_Street_1", "Billing_Street_2", "Billing_City",
               "Billing_State", "Billing_Country", "Billing_Zip_Code",
               "Scholarship", "School_[WebLead]", "Teacher_[WebLead]")
  
  # maybe needed?
  maybe_cols <- c( "Student_Notes")
  # not needed?
  maybe_cols3 <- c("Consent_to_Media_Release",
                   "Consent_to_Contact_Teacher", "Consent_to_Leave_Unescorted")
  # maybe_cols4 <- c("Emergency_Contact", "Emergency_Phone", "Medical_Information")
  maybe_cols5 <- c("Last_Modified_On")
  # all the same
  maybe_cols6 <- c("Center_Id", "Virtual_Center")
  # REXAMINE COLUMNS AFTER ABOVE ARE REVIEWED
  
  # Remove columns
  dat <- removeRawCols(dat, rm_cols)
  dat <- removeRawCols(dat, na_cols, test_na = T)
  
  return (dat)
}#eof

### getAccountData
# Returns data processed from Radius's "Account Export" file
getAccountData <- function(date = Sys.Date(), ignoreMissing = F){
  dat <- readRawData("Account Export", date, ignoreMissing)
  
  # Create new columns
  dat <- mutate(dat, Account = paste0(Last_Name, ", ", First_Name), .before = Account_Id)
  
  # Columns to be removed
  rm_cols <- c("First_Name", "Last_Name", "Created_By", "Last_Modified_By...20",
               "Last_Modified_By...33")
  na_cols <- c("Date_of_Birth", "Last_TriMathlon_Reg._Date")
  
  # not needed?
  maybe_cols = c("Description", "Customer_Comments",
                 "Referral_Accounts", "Account_Relation",
                 "Last_Modified_Date", "emer phon num", "emer_cont")
  # REXAMINE COLUMNS AFTER ABOVE ARE REVIEWED
  
  # Remove columns
  dat <- removeRawCols(dat, rm_cols)
  dat <- removeRawCols(dat, na_cols, test_na = T)
  
  return(dat)
}#eof

### getProgressData
# Returns data processed from Radius's "Current Batch Detail Export" file
# Contains rolling 30 days info on attendances and skills
getProgressData <- function(date = Sys.Date(), ignoreMissing = F) {
  fileRoot <- "Current Batch Detail Export"
  filePath <- file.path(getwd(), "Raw_Data", as.rawFileName(fileRoot))
  
  # NEED TO REORGANIZE THIS SOMEHOW
  if(!file.exists(filePath)){
    #Try to move from downloads
    moveDataDownloads(gsub(".*/", "", filePath))
    if(!file.exists(filePath)){
      #Check for "bootstrap" files
      fileRoot2 <- "Student Report"
      dat <- readRawData(fileRoot2, date, ignoreMissing)
        
      cat("NOTICE: ", file.path(getwd(), "Raw_Data", as.rawFileName(fileRoot2)), 
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
  
  if(!file.exists(filePath)) {
    stop(filePath, " does not exist :(")
  }
  dat <- readRawData(fileRoot, date, ignoreMissing)
  
  # PROCESS COLUMNS HERE

  # MERGE getStudentRanking() INTO dat
  return(dat)
}#eof

getStudentRanking <- function(date = Sys.Date()){
  dat <- getProgressData(date)
  
  #Merge in delivery record from Enrollments
  deliveryKey <- mutate(getEnrollmentData(date), 
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
  dat <- mutate(dat, Pest = Skills_Mastered/Attendances, .after = Student)
  
  #Outlier test
  dat <- mutate(dat, 
                zscore = (mean(dat$Pest)-Pest)/
                  (sd(dat$Pest)/sqrt(Attendances)), .after = Pest)
  #Get sd without outliers
  samdev <- sd(dat$Pest[dat$zscore<outlierThreshold])
  dat <- mutate(dat, 
                UB = round(Pest-qnorm((1-CI/100)/2)*
                             samdev/sqrt(Attendances),roundingDig),
                LB = round(Pest+qnorm((1-CI/100)/2)*
                             samdev/sqrt(Attendances),roundingDig),
                .after = zscore)
  dat <- mutate(dat, fontsize = round(32*LB/max(dat$LB), 1))
  dat <- dat[order(dat$LB, decreasing = TRUE),]
  dat <- mutate(dat, Rank = dim(dat)[1] +1 - rank(LB, ties.method = "max"),
                .before = Student)
  
  
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

### getEnrollmentData
getEnrollmentData <- function(date = Sys.Date(), ignoreMissing = F) {
  dat <- readRawData("Enrolled Report", date, ignoreMissing)
  
  # Rename columns
  names(dat)[names(dat) == "Account_Name"] <- "Account"
  
  # Reformat columns
  # NEED TO REFORMAT Membership_Type
  # remove asterisk
  
  # Create columns from other columns
  dat <- mutate(dat, Student = paste(Student_First_Name, Student_Last_Name),
                .before = Student_First_Name)
  
  # Columns to be removed
  # Session_Length is handled as Duration in getProgressData()
  rm_cols <- c("Student_First_Name", "Student_Last_Name", "Session_Length")
  na_cols <- c()
  
  # all the same
  maybe_cols <- c("Virtual_Center", "Status")
  # maybe needed?
  # prim enr end -> end of contract date
  maybe_cols2 <- c("Primary_Enrollment_Start",
                   "Expected_Monthly_Amount")
  # REXAMINE COLUMNS AFTER ABOVE ARE REVIEWED
  
  # remove "months" from 3 columns
  # rename total sessions to monthly sessions
  
  # Remove columns
  dat <- removeRawCols(dat, rm_cols)
  dat <- removeRawCols(dat, na_cols, test_na = T)
  
  return(dat)
}#eof

### getPaymentData
getPaymentData <- function(date = Sys.Date(), ignoreMissing = F) {
  dat <- readRawData("Payments.xlsx", date)
  
  return(dat)
}

### getCurriculumData
getCurriculumData <- function(date = Sys.Date(), ignoreMissing = F) {
  dat <- readRawData("Curriculum Library Export", date)
  
  return(dat)
}#eof

### getAttendanceHistory
getAttendanceTrainingSet <- function() {
  readRDS(file.path(getwd(), "Cache", "prior2024.rds"))
}

### getAttendanceData
getAttendanceData <- function(get = FALSE, date = Sys.Date()) {
  dat <- readRawData("Student Attendance Report Export", date)
  
  logfile <- file.path(getwd(), "Cache", "studentAttendanceLog.csv")

  if(!file.exists(logfile)) {
    stop("While running getAttendanceData(), \"", logfile,
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
    cat("NOTICE: getAttendanceData(get = ", get, ", date = ", as.character(date),
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
  #   stop(paste0("While running getAttendanceData ", fileLoc, 
  #               "was not able to be read."))
  # }
  
  
}#eof

getAssessments <- function(updateGlobal = FALSE, 
                           date = Sys.Date(), ignoreMissing = F) {
  dat <- readRawData("Students Export", date, ignoreMissing, regExFile = TRUE)
  
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




moveDataDownloads <- function(file_name) {
  download_path <- file.path(regmatches(getwd(), regexpr("^.*?[/].*?[/].*?(?=/)", 
                                          getwd(), perl = T)), "Downloads")
  file_path <- file.path(download_path, file_name)
  
  if (!file.exists(download_path)) {
    stop("Downloads folder not found at \"", download_path, "\"")
  }
  
  if(!grepl("Overview$", getwd())) {
    stop("While trying to moveDataDownloads, \"",
         getwd(),
         "\"\n\tis the working directory but does not end with \"Overview\"")
  }
  
  file_dest <- file.path(getwd(), "Raw_Data", file_name)
  file_moved <- F
  
  if (file.exists(file_path)) {
    file.rename(file_path, file_dest)
    # CHANGE TO NOT PRINT FULL PATH
    cat("NOTICE: ", file_path, "\n\t\t-- moved to -->\n\t", file_dest, "\n", sep="")
    file_moved <- T
  }
  
  return(file_moved)
  
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
