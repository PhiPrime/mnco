## code to prepare `radiusFileRoots` dataset goes here
radiusFileRoots <- list(student = "Students Export",
                        account = "Account Export",
                        progress = "Current Batch Detail Export",
                        enrollment = "Enrolled Report")

usethis::use_data(radiusFileRoots, overwrite = TRUE)
