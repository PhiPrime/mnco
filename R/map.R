###########################     MAP FUNCTIONS     ###########################

### generateMap
generateMap <- function(useCache = TRUE){
  # Data Retrieval

  accounts <- getCenterData("account")
  enrollments <- getCenterData("enrollment")

  dat <- merge(accounts, enrollments, by = "Account_Id", all.x = TRUE)

  # Consolidation

  tidydat <- dat %>% dplyr::mutate(name = paste(First_Name, Last_Name),
                            address = paste(Billing_Street_1, Billing_City,
                                            paste(Billing_State, Billing_Zip_Code),
                                            sep = ", "),
                            info = paste(name, Mailing_Zip_Code, sep = "<br>"),
                            Enrollment_Status = as.factor(Enrollment_Status),
                            Monthly_Amount = as.numeric(Monthly_Amount),
                            Enrollment_Length_of_Stay = as.numeric(
                              gsub(" month(s?)", "",
                                   Enrollment_Length_of_Stay)),
                            total = Monthly_Amount*Enrollment_Length_of_Stay,
                            # name = paste0(First_Name, " ", Last_Name,
                            #               ", with ",
                            #               Student_First_Name, " ",
                            #               Student_Last_Name),
                            info = paste(name, total, sep="<br>"))

  tidydat <- dplyr::select(tidydat, name, total, info, address, Enrollment_Status,
                    Enrollment_Length_of_Stay, Monthly_Amount)

  ##Check for cached data then
  fileLoc <- file.path(cacheDir(), "geocodeCache.xlsx")

  cacheNames <- c("name", "total", "info", "address", "Enrollment_Status",
                  "Enrollment_Length_of_Stay", "Monthly_Amount",
                  "lat", "long")

  if(file.exists(fileLoc) & useCache) {

    cached <- read_excel(fileLoc, .name_repair = "unique_quiet")
    names(cached) <- gsub(" ", "_", names(cached))

    #Subset new entries
    new <- tidydat[!tidydat$name %in% cached$name,]
    if(dim(new)[1]>0){
      new <- tidygeocoder::geocode(new, address, method = 'arcgis')
      ret <- rbind(cached, new)
    } else {
      ret <- cached
    }

  } else {
    if(useCache) {
      warning(paste0("No file found named: ",
                     fileLoc,". One will now be created."))
    }
    ret <- tidygeocoder::geocode(tidydat, address, method = 'arcgis')
  }

  write.xlsx(ret, fileLoc)
  tidydat <- ret

  as.data.frame(tidydat) %>% leaflet() %>% addTiles() %>%
    addMarkers(clusterOptions = markerClusterOptions(),
               popup = tidydat$info)
}#eof
