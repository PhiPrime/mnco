#################     MAKE INTO UNIFIED KABLE FUNCTION     ##################
#' Title
#'
#' @param todisplay
#'
#' @return
#' @export
#'
#' @examples
kablize <- function(todisplay, headerWidth = 12) {
  # Format headers for display
  names(todisplay) <- names(todisplay) %>%
    stringr::str_replace("_", " ") %>%
    stringr::str_trunc(headerWidth)

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
