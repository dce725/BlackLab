map.heatmap <- function (fieldData, tSeries, sigLev=NULL,
                         color_low="darkblue",color_high="darkred",color_na=gray(0.9),
                         xlim=NULL, ylim=NULL, zlim=NULL,
                         mainTitle="", legendTitle="Correlations", Pacific=T) {
  
  if (require(ggplot2) == F){
    install.packages("ggplot2")
    library(ggplot2)
  }
  if (require(plyr) == F){
    install.packages("plyr")
    library(plyr)
  }

  # Store the base data of the underlying map
  baseData <- map_data("world")
  baseData <- baseData[baseData$long > 0,]
  baseData2 <- map_data("world")
  if(Pacific){
    baseData2$long[baseData2$long < 0] <- baseData2$long[baseData2$long < 0] + 360
    baseData2 <- baseData2[baseData2$long > 180,]
  }
  
  #Extract lat/long data
  lon <- as.numeric(str_extract(colnames(fieldData), "(?<=x)[^a]+"))
  lat <- as.numeric(sub('.*a', '', colnames(fieldData)))
  
  corData <- apply(fieldData, 2, function(x) cor(x,tSeries))
  
  # Combine the data into a dataframe
  dfMap           <- as.data.frame(cbind(lon, lat, round(corData,1)))
  colnames(dfMap) <- c("lon", "lat", "Value")
  dfMap$Value[dfMap$Value >= 0.6] = 0.6
  dfMap$Value[dfMap$Value <= -0.6] = -0.6
  dfMap$Value <- as.factor(dfMap$Value)
  
  # Set limits for x, y, z if not specified as parameters
  if (is.null(xlim)) { xlim <- range( lon,na.rm=TRUE) }
  if (is.null(ylim)) { ylim <- range( lat,na.rm=TRUE) }
  if (is.null(zlim)) { zlim <- range(round(corData,1),na.rm=TRUE) }
  lonLines <- seq(round_any(min(lon), 20), round_any(max(lon), 20), 20)
  latLines <- seq(round_any(min(lat), 20), round_any(max(lat), 20), 20)
  
  #Copy siglev format for fieldcor
  fieldCorDF <- cbind.data.frame(colnames(fieldData), round(corData,1))
  colnames(fieldCorDF) <- c("grid", "val")
  #Check to ensure that field grid is the same as siglev grid
  sigLev$sigLevGrid <- paste0("x", paste(sigLev$lon, sigLev$lat, sep = 'a'))
  aCheck <- sum(sigLev$sigLevGrid %in% fieldCorDF$grid == F)
  bCheck <- sum(fieldCorDF$grid %in% sigLev$sigLevGrid == F)
  if (aCheck + bCheck > 0){
    stop("The points in the field and the sigLev must be exactly the same!\n")
  }
  
  #Create stipple by comparing fieldCor to sigLev
  stipple <- rep(NA, length(fieldCorDF$val))
  for (j in 1:length(fieldCorDF$val)){
    stipple[j] <- abs(fieldCorDF$val[j]) < abs(sigLev$Value[sigLev$sigLevGrid == fieldCorDF$grid[j]])
  }
  
  #Group field Cor data and stipple pattern
  
  dfMap2 <- dfMap[stipple,]
  dfMap3 <- dfMap[!stipple,]
  
  #Create the color palette
  pal <- colorRamp(c(color_low, "white"))
  pal <- pal(seq(0, 1, len = 7))
  pal2 <- colorRamp(c("white", color_high))
  pal2 <- pal2(seq(0, 1, len = 7))
  pal3 <- rbind(pal, pal2[-1,])
  
  cols <- c("-0.6" = rgb(pal3[1,1], pal3[1,2], pal3[1,3], maxColorValue = 255),
            "-0.5" = rgb(pal3[2,1], pal3[2,2], pal3[2,3], maxColorValue = 255),
            "-0.4" = rgb(pal3[3,1], pal3[3,2], pal3[3,3], maxColorValue = 255),
            "-0.3" = rgb(pal3[4,1], pal3[4,2], pal3[4,3], maxColorValue = 255),
            "-0.2" = rgb(pal3[5,1], pal3[5,2], pal3[5,3], maxColorValue = 255),
            "-0.1" = rgb(pal3[6,1], pal3[6,2], pal3[6,3], maxColorValue = 255),
            "0" = rgb(pal3[7,1], pal3[7,2], pal3[7,3], maxColorValue = 255),
            "0.1" = rgb(pal3[8,1], pal3[8,2], pal3[8,3], maxColorValue = 255),
            "0.2" = rgb(pal3[9,1], pal3[9,2], pal3[9,3], maxColorValue = 255),
            "0.3" = rgb(pal3[10,1], pal3[10,2], pal3[10,3], maxColorValue = 255),
            "0.4" = rgb(pal3[11,1], pal3[11,2], pal3[11,3], maxColorValue = 255),
            "0.5" = rgb(pal3[12,1], pal3[12,2], pal3[12,3], maxColorValue = 255),
            "0.6" = rgb(pal3[13,1], pal3[13,2], pal3[13,3], maxColorValue = 255))
  
  cols <- cols[(min(fieldCorDF$val)*10 + 7):(max(fieldCorDF$val)*10 + 7)]
  
  
  # Create the plot
  p <- ggplot(dfMap, aes(x=lon, y=lat, fill=Value)) + theme_bw() + geom_tile()
  p <- p + labs(title=paste(mainTitle,"\n",sep=""), x="", y="")
  p <- p + labs(plot.title = element_text(size = rel(1.5))) 
  p <- p + coord_cartesian(xlim = xlim, ylim = ylim)
  p <- p + scale_y_continuous(expand = c(0,0))
  p <- p + scale_x_continuous(expand = c(0,0))
  p <- p + scale_fill_manual(values = cols)
  if (!is.null(sigLev)){
    p <- p + geom_tile(aes(x=lon, y=lat), data = dfMap2, fill = "grey90", alpha = 0.9)
  }
  p <- p + geom_polygon(data=baseData, aes(x=long, y=lat, group=group), 
                        colour="black", fill="grey70", alpha=1)
  p <- p + geom_polygon(data=baseData2, aes(x=long, y=lat, group=group), 
                        colour="black", fill="grey70", alpha=1)
  p <- p + geom_hline(yintercept  = latLines) + geom_vline(xintercept = lonLines)
  p <- p + labs(fill="Correlations")
  p <- p + theme(axis.text = element_text(size = 20))
  p
  
  dfMap$Value <- corData
  return(list("FieldCorrelations" = dfMap, "SignificantGridpoints" = dfMap3[,1:2], "Plot" = p))
}

