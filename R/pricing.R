### getPricing
#' Title
#'
#' @param gradeRange
#' @param contractLength
#' @param sessionCount
#' @param nstu
#'
#' @return
#' @export
#'
#' @examples
getPricing <- function(gradeRange = "Kindergarten to Pre-Algebra",
                       contractLength = retrieve_variable("Standard_Contract_Length"),
                       sessionCount = retrieve_variable("Standard_Number_Sessions"),
                       nstu = dim(getCenterData("student")[which(getCenterData("student")$Enrollment_Status=="Enrolled"),])[1]){

  ######### Adjustable #########

  priceUB <- retrieve_variable("Price_Upper_Bound") #280-287
  priceLB <- retrieve_variable("Price_Lower_Bound")
  stuLB   <- retrieve_variable("Student_Lower_Bound")
  stuUB   <- retrieve_variable("Student_Upper_Bound")
  lenModifier <- retrieve_variable("Length_Modifier")

  ##############################

  #Calculations
  gradeBin <- !grepl("PRE", toupper(gradeRange))
  priceUB <- priceUB
  med <- (stuUB+stuLB)/2
  xsc <- 1/((stuUB-stuLB)*3/20)

  #Base amount by student count
  base <- (priceUB-priceLB)/
    (1+exp(1)^-(xsc*(nstu-med)))+priceLB

  #Adjustment for contract length
  adjLB <- retrieve_variable("Contract_Adjustment_Lower_Bound")
  adjUB <- retrieve_variable("Contract_Adjustment_Upper_Bound")

  lenUB <- retrieve_variable("Contract_Length_Lower_Bound")
  lenLB <- retrieve_variable("Contract_Length_Upper_Bound")

  logBase <- 10
  lenxsc <- ((logBase^-(adjUB/adjLB-1))-1)/lenUB

  contractLengthMod <- adjLB*(-log10(lenxsc*contractLength+1)+1)

  monthlyAmount <- (((base-100)/10*sessionCount)+100) +
    contractLengthMod + 50*gradeBin

  return(round(monthlyAmount))
}

### getPricingGrid
#' Title
#'
#' @param verbose
#' @param nstu
#'
#' @return
#' @export
#'
#' @examples
getPricingGrid <- function(verbose = FALSE,
                           nstu = dim(getCenterData("student")[which(getCenterData("student")$Enrollment_Status=="Enrolled"),])[1]){
  df <- data.frame()
  for(gradeBin in c("Kindergarten to Pre-Algebra","Algebra and Beyond")){
    for (contractLen in c(1,7,12)){
      for (sessions in c(10,15)){
        df <- rbind(df, data.frame("Grade.Level" = gradeBin,
                                   "Contract.Length" = contractLen,
                                   "Sessions.Per.Month" = sessions,
                                   "Monthly.Rate" = getPricing(gradeBin, contractLen,
                                                               sessions, nstu)))
      }
    }
  }
  if(verbose){
    df <- dplyr::mutate(df, Price.per.Session = round(.data$Monthly.Rate/.data$Sessions.Per.Month,2))
  }
  return(df)
}
