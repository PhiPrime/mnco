########################     NA COLUMN FUNCTIONS     ########################
### get_raw_na_cols
# Returns a named list of vectors for names of columns containing only NAs in raw data files
# MODIFY TO ALLOW ITERATION THROUGH MULTIPLE DATES?
get_raw_na_cols <- function(date = today()) {
  fileRoots <- c("Students Export", 
                 "Account Export", 
                 "Current Batch Detail Export", 
                 "Enrolled Report")
  
  # Iterate through each raw data file for given date
  na_col_list <- list()
  for (i in 1:4) {
    dat <- readRawData(fileRoots[i], date = date)
    
    # Get and append names of NA columns to list
    na_col <- sapply(dat, function(x) all(is.na(x)))
    na_col_names <- names(na_col)[na_col]
    na_col_list[[i]] <- na_col_names
  }
  names(na_col_list) <- fileRoots
  
  return(na_col_list)
}

### print_raw_na_cols
# Prints names of NA columns in raw data files
# Formatted for copy/paste into a vector in code
print_raw_na_cols <- function(date = today()) {
  na_col_list = get_raw_na_cols(date = date)
  
  for (col_name in names(na_col_list)) {
    cat(col_name, ": \"", sep="")
    cat(na_col_list[[col_name]], sep = "\", \"")
    cat("\"\n")
  }
}

#####################     COMMENT HEADER FUNCTIONS     ######################


###############     THE FUNCTION THAT MAKES THESE HEADERS     ###############

asCommentHeader <- function(title = "", commentChar = "#") {
  #Unify formatting
  title <- toupper(title)
  
  width <- nchar(#As wide as sample text
"MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM")
  tn <- nchar(title)
  
  #Assign number of whitespace around title
  if(title==""){
    #Null constructor settings
    wsn <- 0
  } else {
    wsn <- 5 #This const can be changed
  }
  
  #Create prefix
  prefix <- ""
  suffix <- ""
  comLen <- nchar(commentChar)
  comChar <- substr(commentChar, comLen, comLen)
  
  if(commentChar == "<!--"){
    prefix <- commentChar
    suffix <- "-->"
  }
    
  #Create begining
  intro <- paste0(prefix,
                 paste0(rep(comChar, 
                       (floor((width-tn-2*wsn-nchar(prefix)-nchar(suffix))/2))), 
                       collapse = ""))
  
  #Create ending
  outro <- paste0(paste0(rep(comChar,
                        (ceiling((width-tn-2*wsn-nchar(prefix)-nchar(suffix))/2))),
                         collapse = ""),
                  suffix)
                  
  
  ws <- paste0(rep(" ", wsn), collapse = "")
  
  ret <- paste0(intro, ws, title, ws, outro)
  
  message(ret)
  }#eof

#############################################################################
#                                                                           #
#                   THE FUNCTION THAT MAKES THESE HEADERS                   #
#                                                                           #
#############################################################################

asBigCommentHeader <- function(title, commentChar = "#"){
  #Add extra whitespace
  wsMulti <- (3-sqrt(5))/2 #Multiple of title length
  xtrawsN <- floor(wsMulti*nchar(title))
  threshold <- 76#arbitrarily chosen
  if((2*xtrawsN+nchar(title))>threshold){
    xtrawsN <- floor((threshold-nchar(title))/2)
    
  }
  xtraws <- paste0(rep(" ", xtrawsN), collapse = "")
  title <- paste0(xtraws, title, xtraws)
  
  #For extra empty rows
  wsTitle <- paste0(rep(" ", nchar(title)), collapse="")
  
  #Make messages generate in order for copy&paste
  asCommentHeader("", commentChar)
  asCommentHeader(wsTitle, commentChar)
  asCommentHeader(title, commentChar)
  asCommentHeader(wsTitle, commentChar)
  asCommentHeader("", commentChar)
}

##########################     CLASS FUNCTIONS     ##########################
as_Grade <- function(x){
  #I'll do this later 
  ret <- x
    return(ret)
}

