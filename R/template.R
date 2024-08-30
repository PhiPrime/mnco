#' Save template to cache file
#'
#' @return None (invisible `NULL`)
#' @export
#'
#' @examples
#' # write later
saveTemplates <- function() {
  cacheFile <- file.path(cacheDir(), "Templates.rds")
  newFile <- readRawData("template") %>%
    #Mark LA timezone, as that's what Radius stores
    dplyr::mutate(
      Last_Modified_Date = lubridate::force_tz(
        .data$Last_Modified_Date, "America/Los_Angeles"
      ),
      Created_Date = lubridate::force_tz(
        .data$Created_Date, "America/Los_Angeles"
      ),
      template = NA_character_
    )

  if(file.exists(cacheFile)){
    #If cache exists, pull it in and look for what's new
    cache <- readRDS(cacheFile)
    #tmp contains ID & cache's Modified Date
    tmp <- dplyr::mutate(cache, Old_Date = .data$Last_Modified_Date,
                  cachedTemplate = .data$template) %>%
      dplyr::select("Created_Date", "Old_Date", "cachedTemplate")

    #Updated is a boolean that is TRUE for positions in newFile that
    # need to be updated
    updated <- which(with(merge(newFile, tmp),
                          Last_Modified_Date!=Old_Date|
                            is.na(cachedTemplate)))

    #newLines are rows that need filled
    newLines <- newFile[updated,]
    newFile[!updated,] <- merge(dplyr::select(newFile[!updated,], -"template"),
                                #If not updated use data in cache
                                cache)
  } else {#If no cache file everything will need updated
    newLines <- newFile
  }

  #Iterate through new rows and prompt user to input
  # template body for each one
  for(i in as.numeric(rownames(newLines))) {
    #Each new one needs filled by user input
    ### NOTE: This does not record line breaks in email body
    message(paste0(
      "Enter the Body for the template named ",
      newFile[i,]$Template_Name, ":"))

    newFile[i,]$template <- paste0(scan(what = ""), sep = " ", collapse =)
  }

  saveRDS(newFile, cacheFile)
}

##getTemplate
## Uses Template cache to return template based on creation date
#' Retrieve template from template cache file
#'
#' @param createdDate POSIXlt object used as key for template
#' @param name Optional argument to get template by name
#'
#' @return A character string
#' @export
#'
#' @examples
#' # write later
getTemplate  <- function(createdDate = "10/19/2021 6:55:40 PM", name = NULL) {
  cacheFile <- file.path(cacheDir(), "Templates.rds")
  dat <- readRDS(cacheFile)

  if (!is.null(name)) return(dat[dat$Template_Name == name,]$template)

  idDate <- strptime(createdDate, "%m/%d/%Y %I:%M:%S %p")
  regexEx <- gsub(" [AP]M", "",
                  gsub(" [0-9]+:", " [0-9]+:", idDate))
  rtn <- dat[grepl(regexEx, dat$Created_Date),]$template
}

#' Get list of templates to be updated
#'
#' @return A data frame
#' @export
#'
#' @examples
#' # write later
templatesNeedUpdated <- function() {
  cacheFile <- file.path(cacheDir(), "Templates.rds")
  newFile <- readRawData("template") %>%
    #Mark LA timezone, as that's what Radius stores
    dplyr::mutate(
      Last_Modified_Date = lubridate::force_tz(
        .data$Last_Modified_Date, "America/Los_Angeles"),
      Created_Date = lubridate::force_tz(
        .data$Created_Date, "America/Los_Angeles"),
      template = NA_character_)

  #Tmp contains ID & Current modified Date
  tmp <- dplyr::mutate(newFile, Current_Date = .data$Last_Modified_Date) %>%
    dplyr::select("Created_Date", "Current_Date")
  #If any dates are different from cache return TRUE
  rtn <- any(with(merge(readRDS(cacheFile), tmp),
                  Last_Modified_Date!=Current_Date))
  return(rtn)
}
