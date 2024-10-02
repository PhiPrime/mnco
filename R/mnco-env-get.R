#' Get raw data directory
#'
#' By default, this directory is set to `"mnco-raw-data"`. It can be changed
#' using [setRawDataDir()].
#'
#' @return A character string
#' @export
#'
#' @examples
#' rawDataDir()
rawDataDir <- function(file = NULL) {
  path <- the$RAW_DATA_DIR
  if (!file.exists(path)) {
    stop("`RAW_DATA_DIR` does not exist: ", path, ".\n",
         "Call setRawDataDir() to set a valid directory.")
  }

  if (is.null(file)) {
    return(path)
  } else {
    return(file.path(path, file))
  }
}

#' Get cache directory
#'
#' By default, this directory is set to `"mnco-cache"`. It can be changed
#' using [setCacheDir()].
#'
#' @return A character string
#' @export
#'
#' @examples
#' cacheDir()
cacheDir <- function(file = NULL) {
  path <- the$CACHE_DIR
  if (!file.exists(path)) {
    stop("`CACHE_DIR` does not exist: ", path, ".\n",
         "Call setCacheDir() to set a valid directory.")
  }

  if (is.null(file)) {
    return(path)
  } else {
    return(file.path(path, file))
  }
}

#' Get downloads directory
#'
#' By default, if the user is on Windows this is set to the Downloads directory
#' under their user directory in `C:/Users/` found through `getwd()`. It can be
#' changed using [setDownloadsDir()].
#'
#' @return A character string
#' @export
#'
#' @examples
#' downloadsDir()
downloadsDir <- function(file = NULL) {
  path <- the$DOWNLOADS_DIR
  if (is.na(path)) {
    stop("`DOWNLOADS_DIR` has not been set.\n",
         "Call setDownloadsDir() to set a valid directory.")
  } else if (!file.exists(path)) {
    stop("`DOWNLOADS_DIR` does not exist: ", path, ".\n",
         "Call setDownloadsDir() to set a directory.")
  }

  if (is.null(file)) {
    return(path)
  } else {
    return(file.path(path, file))
  }
}

#' Retrieve Radius file roots
#'
#' @param type `"all"` returns a named vector of file roots named by their
#'  Radius data type. `"types"` returns a
#'
#' @return A named/unnamed character vector or character string.
#'  See parameter `type`.
#' @export
#'
#' @examples
#' radiusFileRoots()
radiusFileRoots <- function(
    type = c("all", "types", names(the$RADIUS_FILE_ROOTS))) {
  type <- match.arg(type)

  switch(type,
         "all" = return(the$RADIUS_FILE_ROOTS),
         "types" = return(names(the$RADIUS_FILE_ROOTS)),
         return(the$RADIUS_FILE_ROOTS[[type]])

  )
}
