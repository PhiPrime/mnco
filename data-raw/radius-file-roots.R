## code to prepare `RADIUS_FILE_ROOTS` dataset goes here
RADIUS_FILE_ROOTS <- list(student = "Students Export",
                        account = "Account Export",
                        progress = "Current Batch Detail Export",
                        enrollment = "Enrolled Report")

usethis::use_data(RADIUS_FILE_ROOTS, overwrite = TRUE)
