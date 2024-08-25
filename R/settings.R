#Name of the settings file
settings_file <- "centerOverviewSettings.dat"
global <- globalenv()

#Basis for assignment, not for use, will be deleted later
base_settings <- data.frame(
  Recent_Assessments_Date="",
  Attendance_Allowed_Days=NA,
  Deck_Minimum_Threshold=NA,
  Deck_Warning_Duration=NA,
  Deck_Suppression_Maximum_Time=NA,
  Standard_Contract_Length=NA,
  Standard_Number_Sessions=NA,
  Price_Upper_Bound=NA,
  Price_Lower_Bound=NA,
  Student_Upper_Bound=NA,
  Student_Lower_Bound=NA,
  Length_Modifier=NA,
  Contract_Adjustment_Lower_Bound=NA,
  Contract_Adjustment_Upper_Bound=NA,
  Contract_Length_Upper_Bound=NA,
  Contract_Length_Lower_Bound=NA,
  Raw_Data_Directory="",
  Cache_Directory=""
)

#Create settings file if it exists upon the package being loaded
.onLoad <- function(libname,pkgname) {
  #create the settings file if none exists
  if(!file.exists(settings_file))
    initialize_settings_file()

  #Assign global environment to settings
  assign("global_settings",base_settings, envir = global)

  #Establish connection and read the lines into 'text_output'
  file_connection <- file(settings_file)
  text_output  <- readLines(settings_file)

  #read the value of each line into the data frame
  for (i in 1:length(text_output)) {
    #retrieve variable name
    variable_name <- colnames(global$global_settings)[i]

    #get match in the file
    if (grepl(variable_name, text_output[i])) {
      global$global_settings[i] <- gsub(paste0(variable_name,"="), "",text_output[i])
    }
  }

  #Close connection and return the value
  close(file_connection)

  #print message
  message("Settings successfully loaded")
}



#' Create Settings File
#'
#'
#' @return name of file
#' @export
get_file_name <- function() {
  return(settings_file)
}

#' Initializes Settings File
#'
#' @export
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
          "Raw_Data_Directory=UNINITIALIZED",
          "Cache_Directory=UNINITIALIZED"
  ),
  settings_file, append = FALSE)

  #close the connection
  close(file_connection)
}



#' Retrieve a variable from the settings file
#'
#' @param variable_name Name of the variable
#' @param is_numeric Boolean to determine how the variable is interpreted
#'
#' @return Value from the file
#' @export
retrieve_variable <- function(variable_name, is_numeric = TRUE) {

  #Brute force because of short list
  #Should fix this later
  for (i in 1:length(global$global_settings))
    if (colnames(global$global_settings)[i] == variable_name)
      if (is_numeric)
        return(strtoi(global$global_settings[i]))
  else
    return(global$global_settings[i])

  message("Did not find the value")
}

#' Edit a variable in the settings file
#'
#' @param variable_name Name of the variable
#' @param value Value being written to the variable
#'
#' @export
edit_variable <- function(variable_name, value) {

  #Force numeric values to be numeric
  if (!is.na(strtoi(value)))
    value <- strtoi(value)

  #Assign 'value' to global_settings
  #Brute force for short list
  for (i in 1:length(global$global_settings))
    if (colnames(global$global_settings)[i] == variable_name)
      global$global_settings[i] <- value

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

  #Will fix later but here to fix a bug in UI
  refresh_settings()
}

#' Print all variable names in terminal
#'
#' @export
print_all_variables <- function() {
  for (i in 1:dim(global_settings)[2])
    message(colnames(global_settings)[i])
}

refresh_settings <- function() {
  file_connection <- file(settings_file)
  text_output  <- readLines(settings_file)

  #read the value of each line into the data frame
  for (i in 1:length(text_output)) {
    #retrieve variable name
    variable_name <- colnames(global$global_settings)[i]

    #get match in the file
    if (grepl(variable_name, text_output[i])) {
      global$global_settings[i] <- gsub(paste0(variable_name,"="), "",text_output[i])
    }
  }

  #Close connection and return the value
  close(file_connection)
}
