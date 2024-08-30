#' Create map of ???
#'
#' @param useCache `logical` indicating to use the cache file
#'
#' @return A data frame
#' @export
#'
#' @examples
#' # write later
generateMap <- function(useCache = TRUE){
  # Data Retrieval

  accounts <- getCenterData("account")
  enrollments <- getCenterData("enrollment")

  dat <- merge(accounts, enrollments, by = "Account_Id", all.x = TRUE)

  # Consolidation

  tidydat <- dat %>%
    mutate(
      name    = paste(.data$First_Name, .data$Last_Name),
      address = paste(
        .data$Billing_Street_1,
        .data$Billing_City,
        paste(.data$Billing_State, .data$Billing_Zip_Code), sep = ", "),
      info    = paste(.data$name, .data$Mailing_Zip_Code, sep = "<br>"),

      Enrollment_Status         = as.factor(.data$Enrollment_Status),
      Monthly_Amount            = as.numeric(.data$Monthly_Amount),
      Enrollment_Length_of_Stay = as.numeric(
        gsub(" month(s?)", "", .data$Enrollment_Length_of_Stay)
      ),

      total   = .data$Monthly_Amount*.data$Enrollment_Length_of_Stay,
      info    = paste(.data$name, .data$total, sep="<br>")
    ) %>%
    select(
      "name", "total", "info", "address",
      "Enrollment_Status", "Enrollment_Length_of_Stay", "Monthly_Amount"
    )

  ##Check for cached data then
  fileLoc <- file.path(cacheDir(), "geocodeCache.csv")
  cacheNames <- c("name", "total", "info", "address", "Enrollment_Status",
                  "Enrollment_Length_of_Stay", "Monthly_Amount",
                  "lat", "long")

  if(file.exists(fileLoc) && useCache) {
    cached <- utils::read.csv(fileLoc)
    names(cached) <- gsub(" ", "_", names(cached))

    #Subset new entries
    new <- tidydat[!tidydat$name %in% cached$name,]
    if(dim(new)[1]>0){
      new <- tidygeocoder::geocode(new, .data$address, method = 'arcgis')
      ret <- rbind(cached, new)
    } else {
      ret <- cached
    }

  } else {
    if(useCache) {
      message("No file found named: ", fileLoc,". One will now be created.")
    }
    ret <- tidygeocoder::geocode(tidydat, "address", method = 'arcgis')
  }

  utils::write.csv(ret, fileLoc)
  tidydat <- ret

  as.data.frame(tidydat) %>%
    leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addMarkers(
      clusterOptions = leaflet::markerClusterOptions(),
               popup = .data$info
    )
}
