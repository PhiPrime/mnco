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

  # Move all files with radius date format
  files <- list.files(
    downloadsDir(),
    pattern = "\\d{1,2}_\\d{1,2}_\\d{4}.xlsx$"
  )

  src <- file.path(downloadsDir(), files)
  dest <- file.path(rawDataDir(), files)
  filesMoved <- !identical(file.rename(src, dest), logical(0))

  # Print list of files that were moved
  if (filesMoved) {
    sprintf(
      "NOTICE: The following files have been moved from '%s' to '%s'.\n",
      downloadsDir(),
      rawDataDir()
    ) %>%
      paste0(tab_message(files)) %>%
      message()
  } else {
    message("NOTICE: No raw data files were found in \"", downloadsDir(), "\".")
  }

  # Update progressHistory cache if today's progress data is available
  if (!(try(getStudentRanking(), silent = T) %>% inherits("try-error"))) {
    # SEPARATE CACHE SAVING TO SEPARATE FUNCTION FOR CLARITY?
    getProgressHistory()
  }

  invisible(filesMoved)
}

# ADD FUNCTION TO AUTOMATICALLY UPDATE CACHE FILES
