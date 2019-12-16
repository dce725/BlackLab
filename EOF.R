EOF <- function (field, makePlot = T){
  library(RSpectra)
  library(ggplot2)
  sst7 <- as.matrix(scale(field, scale = F))
  sst7Cov <- cov(sst7)
  
  #Calculate the svd of the covariance matrix
  #svd1 <- svd(sst7Cov)
  svd1 <- svd(sst7)
  #Calculate the eigenvectors, eigenvalues, and scores
  degFree <- min(dim(sst7))
  EVecs <- svd1$v
  EVals <- eigs(cov(sst7), k=4)$values
  #Signals <- as.matrix(sst7)[1:degFree,1:degFree] %*% EVecs[1:degFree,1:degFree]
  Signals <- matrix(data = NA, nrow = dim(EVecs)[2], ncol = 4)
  for (i in 1:4){
    Signals[,i] <- sst7 %*% EVecs[,i]
  }
  #Percentage of variance explained by all modes, subset based on degrees of freedom
  expVar <- as.data.frame(EVals/sum(EVals) * 100)
  
  return(list("Eigenvectors" = EVecs, "Eigenvalues" = EVals, "ExplainedVariance" = expVar, "Signals" = Signals))
}

