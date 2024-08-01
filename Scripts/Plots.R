####################     GRAPH GENERATING FUNCTIONS     #####################

####################     EXPLORATORY PLOT FUNCTIONS     #####################
plotHighestCor <- function(dataset, flip = F){
  numdata <- dplyr::select_if(dataset, is.numeric)
  numdata[is.na(numdata)] <- 0
  if(any(toupper(names(numdata))=="PEST")){#quick fix, should use apply
    numdata$Pest[is.infinite(numdata$Pest)] <- 0
  }
  
  cor_mat <- cor(numdata)
  cor_mat[cor_mat >= .98] <- 0
  
  max_cor_inds <- which(cor_mat == max(abs(cor_mat)), arr.ind = T)
  max_row <- rownames(cor_mat)[max_cor_inds[1,1]]
  max_col <- colnames(cor_mat)[max_cor_inds[1,2]]
  
  # par(mfrow = c(1,2))
  p1 <- ggplot(numdata, aes(x=numdata[[max_row]], y=numdata[[max_col]])) + 
    geom_point() + geom_smooth() + labs(x = max_row, y = max_col)
  
  p2 <- ggplot(numdata, aes(x=numdata[[max_col]], y=numdata[[max_row]])) + 
    geom_point() + geom_smooth() + labs(x = max_col, y = max_row)
  p1+p2
}
