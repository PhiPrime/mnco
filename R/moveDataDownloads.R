moveDataDownloads <- function(file_name) {
  download_path <- file.path(regmatches(getwd(), regexpr("^.*?[/].*?[/].*?(?=/)",
                                                         getwd(), perl = T)), "Downloads")
  file_path <- file.path(download_path, file_name)

  if (!file.exists(download_path)) {
    stop("Downloads folder not found at \"", download_path, "\"")
  }

  if (!file.exists(download_path)) {
    stop("Downloads folder not found at \"", download_path, "\"")
  }

  # if(!grepl("Overview$", getwd())) {
  #   stop("While trying to moveDataDownloads, \"",
  #        getwd(),
  #        "\"\n\tis the working directory but does not end with \"Overview\"")
  # }

  file_dest <- file.path(getwd(), "Raw_Data", file_name)
  file_moved <- F

  if (!file.exists(gsub(file_name, "", file_dest))) {
    stop("Raw_Data folder not found at \"", download_path, "\"")
  }

  if (file.exists(file_path)) {
    file.rename(file_path, file_dest)
    # CHANGE TO NOT PRINT FULL PATH
    message("NOTICE: ", file_path,
                "\n\t\t-- moved to -->\n\t", file_dest, "\n", sep="")
    file_moved <- T
  }

  return(file_moved)

}
