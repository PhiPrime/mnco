#' Read Radius raw data excel file
#'
#' @param x File path or directory.
#' @param type Which radius data to use.
#' @param date What date to use.
#'
#' @return A data frame
#' @export
#'
#' @examples
readRawData <- function(x, type = NULL, date = Sys.Date()) {
  # MAYBE CHANGE TO OVERLOADING
  if (!is.null(type)) {
    # Use x as dir
    if (!(type %in% names(radiusFileRoots))) {
      stop("`type` is not a valid argument: \'", type, "\'")
    }

    dir <- x
    file <- as.rawFileName(radiusFileRoots[[type]], date)
    path <- file.path(dir, file)
  } else {
    # Use x as path
    path <- x
  }

  # Read and clean column names
  dat <- readxl::read_excel(path, .name_repair = "unique_quiet")
  names(dat) <- names(dat) %>%
    stringr::str_trim() %>%
    stringr::str_replace_all(" ", "_")

  return(dat)

}

readRawData.old <- function(dir, fileRoot, date,
                        ignoreMissing = F, regExFile = F) {
  # Format file root into Radius style file name
  fileName <- as.rawFileName(fileRoot, date)
  filePath <- NULL

  #If regex find a match with fileRoot in either folder
  #If regex find a match with fileRoot in either folder
  if(regExFile) {
    #Compare first to rawFiles,
    rawFiles <- list.files(file.path(dir))
    fileOptions <- rawFiles[grepl(fileName, rawFiles)]

    if(length(fileOptions)==1){
      #If only one is found assign it
      fileName <- fileOptions[1]
    }else if(length(fileOptions)>1){
      #If more than 1, error
      stop(paste0("\"",fileName, "\" matched with the following files...",
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
      fileOptions <- downloadFiles[grepl(fileName, downloadFiles)]


      if(length(fileOptions)==1){
        #If only one is found assign it
        fileName <- fileOptions
      }else if(length(fileOptions)>1){
        #If more than 1 match, error
        stop(paste0("\"",fileName, "\" matched with the following files...",
                    paste("",fileOptions, sep = "\"\n\"", collapse = ""),
                    "\"\n...and does not know how to proceed, ",
                    "be more specific and try again."))
      } else if (length(fileOptions)==0){
        #If no matches stop and error
        stop(paste0("After searching both \"./Raw_Data\",",
                    "and \"./Downloads\" \"", fileName, "\" yielded no matches.",
                    " Please try again."))
      }}
  }
  # Default behavior: attempt to move file from Downloads, then look
  #   in Raw_Data
  filePath <- file.path(dir, fileName)

  # Read file and reformat column names to prevent bad behaviors
  dat <- readxl::read_excel(filePath, .name_repair = "unique_quiet")
  names(dat) <- gsub(" ", "_", names(dat))

  return(dat)
}#eof
