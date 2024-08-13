#############################     SETTINGS     #############################

settings_file <- "centerOverviewSettings.dat"

#Get file name
get_file_name <- function() {
  return(settings_file)
}

#Function to initialize the settings file if it needs to be reset or created
initialize_settings_file <- function() {
  
  #Establish a connection
  file_connection <- file(settings_file)
  
  #set up the data
  write(c("Recent_Assessments_Date=8_5_2024",
          "Attendance_Allowed_Days=5",
          "Deck_Minimum_Threshold=5",
          "Deck_Warning_Duration=2",
          "Deck_Suppression_Maximum_Time=30",
          "Standard_Contract_Length=7",
          "Standard_Number_Sessions=10",
          "Price_Upper_Bound=368",
          "Price_Lower_Bound=275",
          "Student_Upper_Bound=100",
          "Student_Lower_Bound=50",
          "Length_Modifier=20",
          "Contract_Adjustment_Lower_Bound=60",
          "Contract_Adjustment_Upper_Bound=-10",
          "Contract_Length_Upper_Bound=12",
          "Contract_Length_Lower_Bound=1",
          "Sample_File_Path=~Holder/File/Path"),
        settings_file, append = FALSE)
  
  #close the connection
  close(file_connection)
}

#Function for retrieving a value
#is_numeric is a boolean, FALSE for string return
retrieve_variable <- function(variable_name, is_numeric) {
  
  #Adjust parameters
  variable_name <- paste(variable_name, "=", sep="")
  value <- NA
  
  #Establish connection and read the lines
  file_connection <- file(settings_file)
  text_output  <- readLines(settings_file)
  
  #Brute force to find vector and retrieve the value
  for (i in 1:length(text_output)) {
    if (grepl(variable_name, text_output[i])) {
      value <- gsub(variable_name, "",text_output[i])
    }
  }
  
  #Close connection and return the value
  close(file_connection)
  if (!is_numeric)
    return(value)
  else
    return(strtoi(value))
}

#Function to edit a variable
#Arguments are a string and numeric value
edit_variable <- function(variable_name, value) {
  
  #Establish connection and read lines
  file_connection <- file(settings_file)
  text_output  <- readLines(settings_file)
  
  #Brute force search for variable
  for (i in 1:length(text_output)) {
    if (grepl(variable_name, text_output[i])) {
      #Separate the variable name and value before
      no_name_text <- gsub(variable_name, "",text_output[i])
      text_output[i] <- gsub(no_name_text, paste("=",value,sep=""), x = text_output[i])
      writeLines(text_output, con=settings_file)
    }
  }
  #Close connection
  close(file_connection)
}