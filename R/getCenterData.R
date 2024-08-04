#' Title
#'
#' @param rawDataDir
#' @param data
#' @param date
#' @param ignoreMissing
#'
#' @return
#' @export
#'
#' @examples
getCenterData <- function(dir, type = "all",
                          date = Sys.Date(), ignoreMissing = F) {
  # Directory must exist
  if (!dir.exists(dir)) stop(dir, " does not exist!")

  if (type == "all") {
    unmerged <- list()

    for (.data in names(radiusFileRoots)) {


      unmerged <- unmerged %>% append(dat)
    }
  } else if (type %in% names(radiusFileRoots)) {
    rawFileName <- as.rawFileName(radiusFileRoots[[type]], date)
    rawFilePath <- file.path(dir, rawFileName)




    dat <- readRawData
  } else {
    stop(type, " is not a valid argument to 'data'.")
  }

  invisible(tdat)
}

readRawData <- function(dir, data, date = Sys.Date)
readRawData <- function(path) {
  if (!file.exists(path)) {
    stop(path, " does not exist!")
  }
}

# as.rawFilePath

as.rawFileName <- function(root, date) {
  paste0(root, "  ", as.radiusDate(date), ".xlsx")
}

as.radiusDate <- function(date) {
  paste(lubridate::month(date),
        lubridate::day(date),
        lubridate::year(date), sep = "_")
}
