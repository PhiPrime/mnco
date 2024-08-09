#########################     PRICING FUNCTIONS     #########################

### getPricing
getPricing <- function(gradeRange = "Kindergarten to Pre-Algebra",
                       contractLength = 7,
                       sessionCount = 10,
                       nstu = dim(getStudentData()[which(getStudentData()$Enrollment_Status=="Enrolled"),])[1]){

  ######### Adjustable #########

  priceUB <- 368 #280-287
  priceLB <- 275
  stuLB   <- 50
  stuUB   <- 100
  lenModifier <- 20

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
  adjLB <- 60
  adjUB <- -10

  lenUB <- 12
  lenLB <- 1

  logBase <- 10
  lenxsc <- ((logBase^-(adjUB/adjLB-1))-1)/lenUB

  contractLengthMod <- adjLB*(-log10(lenxsc*contractLength+1)+1)

  monthlyAmount <- (((base-100)/10*sessionCount)+100) +
    contractLengthMod + 50*gradeBin

  return(round(monthlyAmount))
}#eof

### getPricingGrid
getPricingGrid <- function(verbose = FALSE,
                           nstu = dim(getStudentData()[which(getStudentData()$Enrollment_Status=="Enrolled"),])[1]){
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
    df <- dplyr::mutate(df, Price.per.Session = round(Monthly.Rate/Sessions.Per.Month,2))
  }
  return(df)
} #eof
