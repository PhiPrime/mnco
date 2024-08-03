## code to prepare `radiusFileRoots` dataset goes here
radiusFileRoots <- c("Students Export",
                     "Account Export",
                     "Current Batch Detail Export",
                     "Enrolled Report")

usethis::use_data(radiusFileRoots, overwrite = TRUE)
