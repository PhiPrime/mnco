#' Format Radius data file path
#'
#' @param dir Raw data directory path
#' @param root Raw data file root
#' @param date A date
#'
#' @return A character string
#' @export
#'
#' @examples
#' as.rawFilePath(rawDataDir(), "Student Export", "2024-07-31")
as.rawFilePath <- function(dir, root, date) {
  file.path(dir, as.rawFileName(root, date))
}

#' Format Radius style file name
#'
#' @inheritParams as.rawFilePath
#'
#' @return A character string
#' @export
#'
#' @examples
#' as.rawFileName("Students Export", "2024-07-31")
as.rawFileName <- function(root, date) {
  paste0(root, "  ", as.radiusDate(date), ".xlsx")
}

#' Format Radius style date
#'
#' @inheritParams as.rawFilePath
#'
#' @return A character string
#' @export
#'
#' @examples
#' as.radiusDate("2024-07-31")
as.radiusDate <- function(date) {
  paste(lubridate::month(date),
        lubridate::day(date),
        lubridate::year(date), sep = "_")
}

# MODIFY TO ALLOW ITERATION THROUGH MULTIPLE DATES?
#' Get NA column names
#'
#' @param date Date to read data for
#'
#' @return A vector of column names
#' @export
#'
#' @examples
#' get_raw_na_cols()
get_raw_na_cols <- function(date = Sys.Date()) {
  fileRoots <- c("Students Export",
                 "Account Export",
                 "Current Batch Detail Export",
                 "Enrolled Report")

  # Iterate through each raw data file for given date
  na_col_list <- list()
  for (i in 1:4) {
    dat <- readRawData.old(fileRoots[i], date = date)

    # Get and append names of NA columns to list
    na_col <- sapply(dat, function(x) all(is.na(x)))
    na_col_names <- names(na_col)[na_col]
    na_col_list[[i]] <- na_col_names
  }
  names(na_col_list) <- fileRoots

  return(na_col_list)
}

#' Prints names of NA columns in raw data files
#'
#' The column names are formatted for copy/paste into a vector in code
#'
#' @param date Date to read data for
#'
#' @return None (invisible `NULL`)
#' @export
#'
#' @examples
#' print_raw_na_cols
print_raw_na_cols <- function(date = Sys.Date()) {
  na_col_list = get_raw_na_cols(date)

  for (col_name in names(na_col_list)) {
    message(col_name, ": \"", paste0(na_col_list[[col_name]], collapse = "\", \""), "\"")
  }

  invisible(NULL)
}
