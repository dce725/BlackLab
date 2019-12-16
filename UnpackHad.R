#set file path or leave as NULL to select file from browser
#NCpath <- "E:/HadISST_sst.nc"
#Set start and end months
#Months <- c(1,12)
#Set spatial subsetting
#Lons <- c(110,260)
#Lats <- c(0,70)
#Set pacView to T if using 0 to 360 rather than -180 to 180
#pacView <- T

UnpackHad <- function(NCpath = NULL,
                      Months = c(1,12),
                      Years = c(1950,2003),
                      Lons = c(110,260),
                      Lats = c(0,70),
                      pacView = T){
  
  
  #Load packages
  if (require(ncdf4) == F){
    install.packages("ncdf4")
    library(ncdf4)
  }
  if (require(R.utils) == F){
    install.packages("R.utils")
    library(R.utils)
  }
  if (require(chron) == F){
    install.packages("chron")
    library(chron)
  }
  if (require(reshape) == F){
    install.packages("reshape")
    library(reshape)
  }
  if (require(tidyr) == F){
    install.packages("tidyr")
    library(tidyr)
  }
  
  #User select file
  if (is.null(NCpath)){
    NCpath = file.choose()
  }
  
  #Open netCDF
  nc=ncdf4::nc_open(NCpath)
  
  #Sort data
  Lon <- ncvar_get(nc, "longitude")
  Lat <- ncvar_get(nc, "latitude")
  Time<- ncvar_get(nc, "time")
  sst <- ncvar_get(nc, "sst")
  
  #Refine the temp and time data
  sst[sst<-1.8] <- NA
  Tymd<-month.day.year(Time,c(month = 1, day = 1, year = 1870))
  Year <- Tymd$year
  
  Montha <- Tymd$month
  
  
  #Get the area you want
  #Change lon values from [-180 to 180] to [0 to 360] if required
  if (pacView){
    Lon[Lon<0] <- Lon[Lon<0] + 360
  }
  #subset Lon
  sst <- sst[as.integer(Lon) %in% Lons[1]:Lons[2],,]
  Lon <- Lon[as.integer(Lon) %in% Lons[1]:Lons[2]]
  #subset Lat
  sst <- sst[,as.integer(Lat) %in% Lats[1]:Lats[2],]
  Lat <- Lat[as.integer(Lat) %in% Lats[1]:Lats[2]]
  
  # subset the months you want
  sst <- sst[,,Montha %in% Months[1]:Months[2]]
  Month <- Montha[Montha %in% Months[1]:Months[2]]
  Year <- Year[Montha %in% Months[1]:Months[2]]
  
  #Aggregate to annual resolution
  uniqueYear <- unique(Year)
  sst2 <- sst
  for (i in 1:length(Lon)){
    for (j in 1:length(Lat)){
      for (k in 1:length(uniqueYear)){
        sst2[i,j,k] <- mean(sst[i,j,Year %in% uniqueYear[k]], na.rm = T)
      }
    }
  }
  sst2 <- sst2[,,1:length(uniqueYear)]
  
  #Reshape array to dataframe
  sst3 <- melt(sst2)
  colnames(sst3) <- c("lon", "lat", "time", "sst")
  sst3$lon <- Lon[sst3$lon]
  sst3$lat <- Lat[sst3$lat]
  sst3$time <- uniqueYear[sst3$time]
  #Spread to wide format
  sst3$loc <- paste0("x", paste(sst3$lon, sst3$lat, sep = 'a'))
  sst4 <- sst3[,3:5]
  sst4 <- spread(data = sst4, key = loc, value = sst)
  #remove unused stuff
  sst <- sst4
  rm(sst2)
  rm(sst3)
  rm(sst4)
  
  
  HadISST <- sst[sst$time %in% Years[1]:Years[2], -1]
  rownames(HadISST) <- Years[1]:Years[2]
  rm(sst)
  Completeness <- apply(HadISST, 2, function(x) sum(!is.na(x))/length(x) == 1)
  HadISST <- HadISST[,Completeness]
  uniqueCount <- apply(HadISST, 2, function(x) length(unique(x)))
  HadISST <- HadISST[,uniqueCount>1]
  
  return(HadISST)
}

