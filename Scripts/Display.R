kablize <- function(todisplay){
    
  rm <- gsub("(^.{1,8})", "", names(todisplay))
  for(i in 1:length(names(todisplay))){
    names(todisplay)[i] <- gsub(rm[i], "", names(todisplay)[i])
    if(str_length(rm[i])>0){
      names(todisplay)[i] <- paste0(names(todisplay)[i], "...")
    }
  }
  todisplay <- kbl(todisplay, booktabs = TRUE) %>%
    kable_styling(latex_options = "striped",
                  position = "left")
  return(todisplay)
}

displayMostProductiveStudents <- function(){
  
}