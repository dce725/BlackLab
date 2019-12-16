#Run the MC significance test for TreeNob vs HadISST Pacific
#The field is split into a number of bins specified by the user
#A new surrogate time series is created for each iteration based on the length, variance, and AR1 of the supplied series
#Each bin is sampled with replacement 100 times (for alpha of .01 or .05, 1000 times for an alpha of .005 or .001) per iteration - 
#this 'field' is correlated against a surrogate time series
#The correlations are sorted in ascending order and the 99th (for alpha = .01) is selected as the simulated threshold for significance
#After each run, all of the simulated thresholds are averaged to create a point on the convergence plot

#df is the field: columns are gridpoints, rows are timesteps
#See UnpackHad.R for help with formatting
#tSeries is the time series you wish to correlate against the field, a simple vector
#Set maximum number of MC iterations
#Set alpha: example, .05 for 95% significance
#Set convPrec, precision required in number of digits for Pearson Correlation - 
#the level of precision you wish to have in your correlation significance threshold
#Set bins to be the number of groups the field is divided into based on each gridpoint's AR1 - must be an even number >= 4

MCfield <- function(df, tSeries, MC = 100, alpha = 0.05, convPrec = 2, bins = 8, makePlot = T){  
  
  #Set up return list
  fReturn <- list()
  
  if (require(astrochron) == F){
    install.packages("astrochron")
    library(astrochron)
  }
  
  if (require(stats) == F){
    install.packages("stats")
    library(stats)
  }
  
  if (require(stringr) == F){
    install.packages("stringr")
    library(stringr)
  }
  
  numRows <- length(df[,1])
  
  if (bins < 4 || bins/2 != floor(bins/2)){
    warning("Criteria for bin size not met! bins must be even and >= 4\n")
    break
  }

  
  #Calculate AR1 at each field gridpoint
  fieldAR1 <- apply(df, 2, function(x) cor(x[1:(numRows-1)], x[2:numRows]))
  binMin <- min(fieldAR1)
  binMax <- max(fieldAR1)
  AR1sd <- sd(fieldAR1)
  AR1Med <- median(fieldAR1)
  
  binStep <- (binMax - binMin)/bins
  binBounds <- rep(NA, (bins+1))
  for (i in 0:bins){
    binBounds[(i+1)] <- (binMin + (binStep*i))
  }

  #Capture start time
  startTime <- Sys.time()
  
  if (alpha == 0.01 || alpha == 0.05){
    upBound <- floor((1 - alpha)*100)
  }else if (alpha == 0.005 || alpha == 0.001){
    upBound <- floor((1 - alpha)*1000)
  }else{
    stop("alpha must be .05, .01, .005, or .001")
  }
  
  cat("***Note that rounding from 5 is to the even number per IEC 60559 standard***\n")
  
  
  for (z in 1:bins){
    binName <- paste("Bin", z, sep = " ")
    if (z==1){
      GridPoints <- names(fieldAR1[fieldAR1 <= binBounds[(z+1)]])
    }else if(z==bins){
      GridPoints <- names(fieldAR1[fieldAR1 >= binBounds[z]])
    }else{
      GridPoints <- names(fieldAR1[fieldAR1 >= binBounds[z] & fieldAR1 <= binBounds[(z+1)]])
    }
    if (length(GridPoints) == 0) next
    AR1Bounds <- c(binBounds[z], binBounds[(z+1)])
    df1 <- df[,GridPoints]
    sigLev <- rep(NA, MC)
    
    trackConv <- rep(NA, MC)
    sampleData <- NULL
    sampleData <- data.frame(matrix(nrow = numRows))
    
    for (i in 1:MC){
      surrogate <- surrogates(tSeries, verbose = F, genplot = F)
      if (alpha == 0.01 || alpha == 0.05){
        sampleData <- df1[,sample(colnames(df1), 100, replace = T)]
      }else if (alpha == 0.005 || alpha == 0.001){
        sampleData <- df1[,sample(colnames(df1), 1000, replace = T)]
      }else{
        stop("alpha must be .05, .01, .005, or .001")
      }
      
      MCdata <- apply(sampleData, 2, function(x) cor(x, surrogate))
      ordered <- MCdata[order(MCdata)]
      sigLev[i] <- ordered[(upBound)]
      trackConv[i] <- mean(sigLev, na.rm = T)
    }
    
    sigreturn <- trimws(format(round(trackConv[i], (convPrec)), nsmall=(convPrec)))
    cat(sprintf("Total iterations completed for BIN %s: %s\n", z, i))
    cat(sprintf("Significance approximated at r= +/- %s\n\n", sigreturn))
    
    
    if (makePlot){
      par(mfrow=c(1,2))
      plot(na.omit(trackConv), main = paste("Cumulative Simulated Threshold for ", binName, sep = " "), ylab = "Pearson Correlation Coefficient (r)",
           xlab = "Iteration")
      hist(sigLev, main = paste("Histogram of Simulated Significance for ", binName, sep = " "))
    }
    zz <- list("Significance" = sigreturn, "ConvergenceTrack" = trackConv, "Significance Tresholds" = sigLev,
               "AR1Bounds" = AR1Bounds, "GridPoints" = GridPoints)
    fReturn[[binName]] <- zz
    
    
    lon1 <- as.numeric(str_extract(GridPoints, "(?<=x)[^a]+"))
    lat1 <- as.numeric(sub('.*a', '', GridPoints))
    Value <- rep(as.numeric(sigreturn), length(GridPoints))
    dfMap <- as.data.frame(cbind(lon1, lat1, Value))
    colnames(dfMap) <- c("lon", "lat", "Value")
    if (z == 1){
      dfMapOld <- dfMap
    }
    if(z > 1){
      dfMapOld <- rbind(dfMapOld, dfMap)
    }
  }
  fReturn[["MapSigPoints"]] <- dfMapOld
  endTime <- Sys.time()
  timeTot <- format(endTime - startTime)
  cat(sprintf("Total run time: %s\n\n", timeTot))
  par(mfrow=c(2,2))
  return(fReturn)
}





















