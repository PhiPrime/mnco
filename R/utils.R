as.rawFilePath <- function(dir, root, date) {
  file.path(dir, as.rawFileName(root, date))
}

as.rawFileName <- function(root, date) {
  paste0(root, "  ", as.radiusDate(date), ".xlsx")
}

as.radiusDate <- function(date) {
  paste(lubridate::month(date),
        lubridate::day(date),
        lubridate::year(date), sep = "_")
}

########################     NA COLUMN FUNCTIONS     ########################
### get_raw_na_cols
# Returns a named list of vectors for names of columns containing only NAs in raw data files
# MODIFY TO ALLOW ITERATION THROUGH MULTIPLE DATES?
get_raw_na_cols <- function(date = the$CURRENT_DATE) {
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
print_raw_na_cols <- function(date = the$CURRENT_DATE) {
  na_col_list = get_raw_na_cols(date)

  for (col_name in names(na_col_list)) {
    cat(col_name, ": \"", sep="")
    cat(na_col_list[[col_name]], sep = "\", \"")
    cat("\"\n")
  }
}
