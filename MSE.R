##Function to calculate MSE

MSE <- function(predictor, predictand, MC=NULL, alpha=.01){
  
  #check that series are of equal length
  predictorLength <- length(predictor)
  if (length(predictor) != length(predictand)){
    stop("The two series must be of the same length!\n")
  }
  
  #Initiate variables and run RE/CE
  sumErr <- 0
  sumErrAvg <- 0
  predictandAvg <- mean(predictand)
  
  for (i in 1:length(predictor)){
    err <- (predictor[i] - predictand[i])^2
    sumErr <- sumErr + err
    errAvg <- (predictandAvg - predictand[i])^2
    sumErrAvg <- sumErrAvg + errAvg
  }
  mse <- sumErr/length(predictor)
  mseAvg <- sumErrAvg/length(predictor)
  RE <- 1 - mse/mseAvg
  
  Rsquared <- cor(predictor, predictand)^2
  
  
  if (!is.null(MC)){
    #Repeat run of RE/CE to attain a significance Threshold
    if (require(astrochron) == F){
      install.packages("astrochron")
      library(astrochron)
    }
    
    RE2 <- rep(NA, MC)
    RsquaredSim <- rep(NA, MC)
    
    #Generate surrogate time series of predictor
    surrogate <- surrogates(predictor, nsim = MC, verbose = F, genplot = F)
    surrogate <- surrogate + (mean(predictand) - mean(surrogate))
    for (j in 1:MC){
      
      sumErr2 <- 0
      sumErrAvg2 <- 0
      
      for (i in 1:length(predictand)){
        err2 <- (surrogate[i,j] - predictand[i])^2
        sumErr2 <- sumErr2 + err2
        errAvg2 <- (predictandAvg - predictand[i])^2
        sumErrAvg2 <- sumErrAvg2 + errAvg2
      }
      mse2 <- sumErr2/length(surrogate)
      mseAvg2 <- sumErrAvg2/length(surrogate)
      RE2[j] <- 1 - mse2/mseAvg2
      
      RsquaredSim[j] <- cor(surrogate[,j], predictand)^2
    }
    place <- MC - alpha*MC
    SLev <- sort(RE2)[place]
    
    RsquaredSLev <- sort(RsquaredSim)[place]
    
    returns <- list("MSE" = mse, "RE" = RE, "SigLev" = SLev, "R2" = Rsquared, "R2SigLev" = RsquaredSLev)
  }else{
    returns <- list("MSE" = mse, "RE" = RE, "R2" = Rsquared)
  }
  
  return(returns)
}