#################     MAKE INTO UNIFIED KABLE FUNCTION     ##################
kablize <- function(todisplay){
    
  rm <- gsub("(^.{1,8})", "", names(todisplay))
  for(i in 1:length(names(todisplay))){
    names(todisplay)[i] <- gsub(rm[i], "", names(todisplay)[i])
    if(stringr::str_length(rm[i])>0){
      names(todisplay)[i] <- paste0(names(todisplay)[i], "...")
    }
  }
  todisplay <- kableExtra::kbl(todisplay, booktabs = TRUE,
                   "latex", escapse = FALSE) %>%
    kableExtra::kable_styling(latex_options = "striped",
                  position = "left",
                  bootstrap_options = c("hover", "condensed"))
  return(todisplay)
}

#######     FUNCTION TO GENERATE "MOST PRODUCTIVE STUDENT" KABLE     ########
displayMostProductiveStudents <- function(){
  
}
