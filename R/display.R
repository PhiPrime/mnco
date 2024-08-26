#################     MAKE INTO UNIFIED KABLE FUNCTION     ##################
#' Title
#'
#' @param todisplay
#'
#' @return
#' @export
#'
#' @examples
kablize <- function(todisplay){

  rm <- gsub("(^.{1,8})", "", names(todisplay))
  for(i in 1:length(names(todisplay))){
    names(todisplay)[i] <- gsub(rm[i], "", names(todisplay)[i])
    if(stringr::str_length(rm[i])>0){
      names(todisplay)[i] <- paste0(names(todisplay)[i], "...")
    }
  }
  # Remove underscores in headers
  names(todisplay) <- names(todisplay) %>%
    stringr::str_replace("_", " ")

  todisplay <- todisplay %>%
    kableExtra::kbl(
      format = "latex",
      row.names = F,
      escape = F,
      booktabs = TRUE
    ) %>%
    kableExtra::kable_styling(
      latex_options = "striped",
      position = "left",
      bootstrap_options = c("hover", "condensed")
    )
  return(todisplay)
}

#######     FUNCTION TO GENERATE "MOST PRODUCTIVE STUDENT" KABLE     ########
#' Title
#'
#' @return
#' @export
#'
#' @examples
displayMostProductiveStudents <- function(){

}
