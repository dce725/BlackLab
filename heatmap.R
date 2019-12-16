simple.heatmap <- function (lon=NULL, lat=NULL, grid=NULL, Values,
                         color_na=gray(0.9),
                         xlim=NULL, ylim=NULL, zlim=NULL,
                         mainTitle="", SubTitle="", legendTitle="R^2", Pacific=T, Factors=T) {
  
  if (require(ggplot2) == F){
    install.packages("ggplot2")
    library(ggplot2)
  }
  if (require(plyr) == F){
    install.packages("plyr")
    library(plyr)
  }
  if (require(RColorBrewer) == F){
    install.packages("RColorBrewer")
    library(RColorBrewer)
  }
  if (require(stringr) == F){
    install.packages("stringr")
    library(stringr)
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
  if (!is.null(grid)){
    lon <- as.numeric(str_extract(grid, "(?<=x)[^a]+"))
    lat <- as.numeric(sub('.*a', '', grid))
  }
  
  
  # Combine the data into a dataframe
  dfMap           <- as.data.frame(cbind(lon, lat, as.numeric(Values)))
  colnames(dfMap) <- c("lon", "lat", "Value")
  if (Factors){
    dfMap$Value <- as.factor(as.integer(dfMap$Value*10)/10)
    pal <- colorRamp(c("white", "blue"))
    pal <- pal(seq(0, 1, len = length(unique(dfMap$Value))))
    cols <- rgb(pal, maxColorValue = 255)
  }
  
  
  # Set limits for x, y, z if not specified as parameters
  if (is.null(xlim)) { xlim <- range( lon,na.rm=TRUE) }
  if (is.null(ylim)) { ylim <- range( lat,na.rm=TRUE) }
  if (is.null(zlim)) { zlim <- range(round(Values,1),na.rm=TRUE) }
  lonLines <- seq(round_any(min(lon), 20), round_any(max(lon), 20), 20)
  latLines <- seq(round_any(min(lat), 20), round_any(max(lat), 20), 20)
  
  # Create the plot
  p <- ggplot(dfMap, aes(x=lon, y=lat, fill=Value)) + theme_bw() + geom_tile()
  if (Factors){
    p <- p + scale_fill_manual(values = cols)
  }else{
    p <- p + scale_fill_continuous()
  }
  p <- p + labs(title=mainTitle, subtitle=SubTitle)
  p <- p + labs(plot.title = element_text(size = rel(1.5))) 
  p <- p + coord_cartesian(xlim = xlim, ylim = ylim)
  p <- p + scale_y_continuous(expand = c(0,0))
  p <- p + scale_x_continuous(expand = c(0,0))
  p <- p + geom_polygon(data=baseData, aes(x=long, y=lat, group=group), 
                        colour="black", fill="grey70", alpha=1)
  p <- p + geom_polygon(data=baseData2, aes(x=long, y=lat, group=group), 
                        colour="black", fill="grey70", alpha=1)
  p <- p + geom_hline(yintercept  = latLines) + geom_vline(xintercept = lonLines)
  p <- p + labs(fill=legendTitle)
  p
  
  return(p)
}