#' Read Radius data excel file
#'
#' @param type Radius data file type
#' @param date Date to read data for
#'
#' @return A data frame
#' @export
#'
#' @examples
#' readRawData("student")
readRawData <- function(type = radiusFileRoots("types"), path = NULL, date = Sys.Date()) {
  # Ensure valid type of Radius data file
  if (type != "path") {
    type <- match.arg(type)

    # Create path to raw data file
    path <- matchRegexRoot(radiusFileRoots(type), date) %>%
      file.path(rawDataDir(), .)
  }

  # Read and clean column names
  data <- readxl::read_excel(path, .name_repair = "unique_quiet")
  names(data) <- names(data) %>%
    stringr::str_trim() %>%
    stringr::str_replace_all(" ", "_") %>%
    stringr::str_replace_all("[()]", "")
  # ADD CHECK FOR DUPLICATE COLUMNS

  invisible(data)
}

#' Extract path of raw data file matched by regex
#'
#' @param rootRegex Radius file root given by radiusFileRoots()
#' @param date Date suffix of file to search for
#'
#' @return Path to matched file
#' @noRd
matchRegexRoot <- function(rootRegex, date) {
  dir <- rawDataDir()
  fileRegex <- rawFileName(rootRegex, date)

  regexMatches <- list.files(dir) %>%
    magrittr::extract(stringr::str_detect(., fileRegex))

  path <- switch(as.character(length(regexMatches)),
    "1" = regexMatches,
    "0" = stop(
      "No files were found matching the regex \"", fileRegex, "\".\n",
      "  Try again after downloading and moving the file to \"", dir, "\"."
    ),
    stop(
      "The following files were matched for the regex \"", fileRegex, "\":\n",
      "\t", paste0(regexMatches, collapse = "\n\t"), "\n",
      "  Try again after keeping only one of these in \"", dir, "\"."
    )
  )

  return(path)
}

#' Format Radius data file path
#'
#' @param dir Raw data directory path
#' @param root Raw data file root
#' @param date A date
#'
#' @return A character string
#' @export
rawFilePath <- function(dir, root, date) {
  file.path(dir, rawFileName(root, date))
}

#' Format Radius style file name
#'
#' @inheritParams rawFilePath
#'
#' @return A character string
#' @export
rawFileName <- function(root, date) {
  paste0(root, "  ", radiusDate(date), ".xlsx")
}

#' Format Radius style date
#'
#' @inheritParams rawFilePath
#'
#' @return A character string
#' @export
radiusDate <- function(date) {
  paste(lubridate::month(date),
        lubridate::day(date),
        lubridate::year(date), sep = "_")
}
