#######################     SOURCE ALL SCRIPTS     #########################
scripts <- list.files("./Scripts")
scripts <- scripts[grepl(".R$", scripts)&
                     !grepl("All.R$", scripts)]
scripts <- paste0("Scripts/", scripts)
sapply(scripts, source)
rm(scripts)