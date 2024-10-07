#################     MAKE INTO UNIFIED KABLE FUNCTION     ##################
#' Format data frame as LaTeX table for .Rmd knit
#'
#' @param todisplay A data frame
#'
#' @return LaTeX talbe
#' @export
#'
#' @examples
#' # write later
kablize <- function(todisplay, headerWidth = 13) {
  # Format headers for display
  names(todisplay) <- names(todisplay) %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_trunc(headerWidth)

  todisplay <- todisplay %>%
    # Format certain columns for display
    mutate(dplyr::across(
      where(lubridate::is.Date),
      ~format(.x, "%m-%d-%Y")
    )) %>%

    # LaTeX formatting
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

# FUNCTION TO GENERATE "MOST PRODUCTIVE STUDENT" KABLE
displayMostProductiveStudents <- function(){

}
