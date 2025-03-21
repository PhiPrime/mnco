#' Create plot of most correlated variables
#'
#' @param dataset A data frame
#' @param removeOutliers `logical` indicating to remove outliers
#' @param oCI ???
#'
#' @return Two ggplot objects
#' @export
#'
#' @examples
#' # write later
plotHighestCor <- function(dataset, removeOutliers = FALSE, oCI = 99){
  #Filter out col with numeric datatypes, any uncertainty is made 0 by default
  numdata <- dplyr::select_if(dataset, is.numeric)
  numdata[is.na(numdata)] <- 0
  numdata[sapply(numdata, is.infinite)] <- 0




  #Find corelations that aren't (nearly) direct and assign value of 0
  cor_mat <- stats::cor(numdata)
  cor_mat[cor_mat >= .97] <- 0

  #Find which variables have the largest correlation
  max_cor_inds <- which(cor_mat == max(abs(cor_mat)), arr.ind = T)
  max_row <- rownames(cor_mat)[max_cor_inds[1,1]]
  max_col <- colnames(cor_mat)[max_cor_inds[1,2]]

  #If desired, use grubbs test to remove high and low outliers
  if(removeOutliers){
    alpha <- 1-oCI/100
    #For both row and col...
    for(vname in c(max_row,max_col)){
      #For each's highest and lowest...
      for(LOWEST in c(F,T)){
        #While p isn't higher than User defined alpha
        p <- 0
        while(p<alpha){
          p <- outliers::grubbs.test(numdata[[vname]], opposite = LOWEST)$p
          if(p<alpha){
            #remove observation from numdata
            #Remove row with max value in numdata$vname;
            #using (-1)^1 and (-1)^0 to toggle highest/lowest
            numdata <- numdata[-which.max(
              (-1)^LOWEST*numdata[[vname]]),]
          }
        }#while
      }#High/Low
    }#for loop finished, outliers should be removed
  }#End of outlier section

  # Standard plot to produce
  stdplot <- function(whatOnX, whatOnY){
    ggplot2::ggplot(numdata, ggplot2::aes(x=.data[[whatOnX]], y=.data[[whatOnY]])) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "loess", formula = y~x) +
      ggplot2::labs(x = whatOnX, y = whatOnY)
  }

  # Make with swapped axes since independent variable is unknown
  p1 <- stdplot(max_row,max_col)
  p2 <- stdplot(max_col,max_row)
  # To Luke: p1+p2 throws error bc can't add ggplot objects -Justin
  c(p1, p2)
  # p1+p2
}

#' Plot grade difference vs. point estimate
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' # write later
expGradedifPest <- function() {
  dat <- getCenterData("progress") %>%
    mutate(Pest = .data$Skills_Mastered/.data$Attendances) %>%
    merge(getMostRecentAssessments()) %>%
    filter(.data$Pest < 2, .data$gradeDif >= -2)

  p2 <-
    ggplot2::ggplot(dat, ggplot2::aes(x=.data$gradeDif, y = .data$Pest)) +
    ggplot2::xlab("Grade Difference") + ggplot2::ylab("P Skills per session") +
    ggplot2::geom_point() + ggplot2::geom_smooth()
}
