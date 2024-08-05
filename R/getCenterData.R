#' Get tidied data from Radius files
#'
#' @param date What date to get data for.
#' @param ignoreMissing Whether to throw an error if file is not found
#' @param dir Directory containing Radius files
#' @param type Which kind of Radius data to get
#'
#' @return A data frame
#' @export
#'
#' @examples
getCenterData <- function(dir, type = "all",
                          date = Sys.Date(), ignoreMissing = F) {
  # Directory must exist
  if (!dir.exists(dir)) stop("`dir` does not exist: \'", dir, "\'")

  # USE match.arg, stopifnot
  if (type == "all") {
    # Get all data and merge
    unmerged <- list()

    for (.data in names(radiusFileRoots)) {


      unmerged <- unmerged %>% append(dat)
    }
  } else if (type %in% names(radiusFileRoots)) {
    # Get and tidy data
    dat <-
      readRawData(dir, type, date) %>%
      tidyRawData(type)

  } else {
    stop("`type` is not a valid argument: \'", type, "\'")
  }

  invisible(tdat)
}

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
  dat <- readxl::read_excel(path, .name_repair = "unique_quiet") %>%
  names(dat) <- names(dat) %>%
    stringr::str_trim() %>%
    stringr::str_replace_all(" ", "_")

  return(dat)

}
