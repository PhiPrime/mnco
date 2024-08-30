#' Move Radius data files
#'
#' The source directory is the downloads directory set by
#' [setDownloadsDir()]. On windows, this is defaulted to the user's
#' Downloads folder found through [getwd()]. The target directory is by default
#' `./mnco-raw-data` in the working directory and can be set using
#' [setRawDataDir()].
#'
#' @return Invisible `logical` indicating whether data files were moved
#' @export
#'
#' @examples
#' moveDataDownloads()
moveDataDownloads <- function() {
  # MAKE DEFAULT ARGUMENTS FOR src and dest DIRECTORIES
  files <- list.files(
    downloadsDir(),
    pattern = "\\d{1,2}_\\d{1,2}_\\d{4}.xlsx$"
  )

  src <- file.path(downloadsDir(), files)
  dest <- file.path(rawDataDir(), files)
  filesMoved <- !identical(file.rename(src, dest), logical(0))

  if (filesMoved) {
    message("NOTICE: The following files have been moved from ",
            "\"", downloadsDir(), "\" to \"", rawDataDir(), "\".\n",
            "\t", paste0(files, collapse = "\n\t")
    )
  } else {
    message("NOTICE: No raw data files were found in \"", downloadsDir(), "\".")
  }

  invisible(filesMoved)
}
