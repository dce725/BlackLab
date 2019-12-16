detrendField <- function(field, tSeries,detrend="",nYrs=30){
  #Detrend data
  if (detrend == "linear"){
    field <- apply(field, 2, function(x) pracma::detrend(x))
    tSeries <- pracma::detrend(tSeries)
  }else if (detrend == "spline"){
    field <- apply(field, 2, function(x) dplR::detrend.series(x, make.plot = F, method = "Spline", nyrs = nYrs))
    tSeries <- dplR::detrend.series(tSeries, make.plot = F, method = "Spline", nyrs = nYrs)
  }else{cat("Not Detrended!")}
  return(list("field"=field, "timeSeries"=tSeries))
}