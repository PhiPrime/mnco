#' Set Radius data directory
#'
#' @param path A directory path
#'
#' @return The previous raw data directory path, invisibly.
#' @export
#'
#' @examples
#' setRawDataDir("./Raw_Data")
setRawDataDir <- function(path) {
  old <- the$RAW_DATA_DIR
  the$RAW_DATA_DIR <- path
  invisible(old)
}

#' Set cache directory
#'
#' @param path A directory path
#'
#' @return The previous cache directory path, invisibly.
#' @export
#'
#' @examples
#' setCacheDir("./Cache")
setCacheDir <- function(path) {
  old <- the$CACHE_DIR
  the$CACHE_DIR <- path
  invisible(old)
}

#' Set downloads directory
#'
#' @param path A directory path
#'
#' @return The previous downloads directory path, invisibly.
#' @export
#'
#' @examples
#' setDownloadsDir("./Downloads")
setDownloadsDir <- function(path) {
  old <- the$DOWNLOADS_DIR
  the$DOWNLOADS_DIR <- path
  invisible(old)
}
