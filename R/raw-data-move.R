moveDataDownloads <- function() {
  files <- list.files(downloadsDir(), pattern = "\\d{1,2}_\\d{1,2}_\\d{4}.xlsx$")
  fileSrc <- file.path(downloadsDir(), files)
  fileDest <- file.path(rawDataDir(), files)

  fileMoved <- ifelse(
    identical(file.rename(fileSrc, fileDest), logical(0)),
    FALSE,
    TRUE
  )

  # CHANGE TO NOT PRINT FULL PATH
  if (fileMoved) {
    message("NOTICE: The following files have been moved from ",
            "\"", downloadsDir(), "\" to \"", rawDataDir(), "\".\n",
            "\t", paste0(files, collapse = "\n\t")
    )
  } else {
    message("NOTICE: No raw data files were found in \"", downloadsDir(), "\".")
  }

  return(fileMoved)

}
