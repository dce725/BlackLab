reconStats <- function (recon, field, MC, alpha){

  if (require(stringr) == F){
    install.packages("stringr")
    library(stringr)
  }
  
  if (sum(dim(recon) != dim(field)) > 1){
    stop("The dimensions of the field and reconstruction must match!")
  }
  
  if (sum(apply(recon, 2, function(x) sd(x)) == 0) > 0){
    warning("The reconstruction contains constant series. \nBoth the reconstruction and field will be trimmed.")
    KeepIndex <- names(which(apply(recon, 2, function(x) sd(x)) != 0))
    recon <- recon[,KeepIndex]
    field <- field[,KeepIndex]
  }
  
  if (sum(apply(field, 2, function(x) sd(x)) == 0) > 0){
    warning("The field contains constant series. \nBoth the reconstruction and field will be trimmed.")
    KeepIndex <- names(which(apply(field, 2, function(x) sd(x)) != 0))
    recon <- recon[,KeepIndex]
    field[,KeepIndex]
  }

  source("E:/FieldCorFun/MSE.R")
  
  #Verification Period Stats
  
  CEstats <- rep(NA, dim(field)[2])
  CEsig <- rep(NA, dim(field)[2])
  R2 <- rep(NA, dim(field)[2])
  R2sig <- rep(NA, dim(field)[2])
  for (i in 1:dim(field)[2]){
    MSEstats <- MSE(recon[,i], field[,i], MC=MC, alpha = alpha)
    CEstats[i] <- MSEstats$RE
    CEsig[i] <- MSEstats$SigLev
    R2[i] <- MSEstats$R2
    R2sig[i] <- MSEstats$R2SigLev
    cat("\r")
    cat(paste0(as.integer(i/dim(field)[2]*100), "%"))
  }
  
  grid <- colnames(field)[CEstats>CEsig & CEstats>0]
  lon <- as.numeric(str_extract(grid, "(?<=x)[^a]+"))
  lat <- as.numeric(sub('.*a', '', grid))
  plotStats <- data.frame(lon, lat, CEstats[CEstats>CEsig & CEstats>0])
  colnames(plotStats) <- c("lon", "lat", "RE")
  
  lon2 <- as.numeric(str_extract(KeepIndex, "(?<=x)[^a]+"))
  lat2 <- as.numeric(sub('.*a', '', KeepIndex))
  allStats <- data.frame(lon2, lat2, CEstats, CEsig, R2, R2sig)
  colnames(allStats) <- c("lon", "lat", "RE", "RESigLevel", "Rsquared", "RsquaredSigLevel")
  
  grid2 <- colnames(field)[R2 > R2sig]
  lon3 <- as.numeric(str_extract(grid2, "(?<=x)[^a]+"))
  lat3 <- as.numeric(sub('.*a', '', grid2))
  significantRsquared <- R2[R2 > R2sig]
  R2SigStats <- data.frame(lon3, lat3, significantRsquared)
  
  returns <- list("RsquaredSigStats" = R2SigStats, "RESigStats" = plotStats, "RawStats" = allStats)
  
  return(returns)
}