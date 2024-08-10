#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
setRawDataDir <- function(path) {
  old <- the$RAW_DATA_DIR
  the$RAW_DATA_DIR <- path
  invisible(old)
}

#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
setCacheDir <- function(path) {
  old <- the$CACHE_DIR
  the$CACHE_DIR <- path
  invisible(old)
}

setDownloadsDir <- function(path) {
  old <- the$DOWNLOADS_DIR
  the$DOWNLOADS_DIR <- path
  invisible(old)
}
