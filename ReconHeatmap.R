Recon.heatmap <- function (lon=NULL, lat=NULL, grid=NULL, Values,
                         color_na=gray(0.9), cutoff=3,
                         xlim=NULL, ylim=NULL, zlim=NULL,
                         mainTitle="", SubTitle="", legendTitle="", Pacific=T,
                         curved=F, outline=F, highCol="red", lowCol="blue",
                         zeroPlus = T, gridLines = F) {
  
  if (require(ggplot2) == F){
    install.packages("ggplot2")
    library(ggplot2)
  }
  if (require(ggforce) == F){
    install.packages("ggforce")
    library(ggforce)
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
  
  Values <- as.numeric(Values)
  Values[Values>cutoff] <- cutoff
  Values[Values<(cutoff*-1)] <- cutoff*-1
  # Combine the data into a dataframe
  dfMap           <- as.data.frame(cbind(lon, lat, Values))
  colnames(dfMap) <- c("Longitude", "Latitude", "Value")
  dfMap$Value <- as.factor(as.integer(dfMap$Value*10)/10)
  
  # Set limits for x, y, z if not specified as parameters
  if (is.null(xlim)) { xlim <- range( lon,na.rm=TRUE) }
  if (is.null(ylim)) { ylim <- range( lat,na.rm=TRUE) }
  if (is.null(zlim)) { zlim <- range(round(Values,1),na.rm=TRUE) }
  lonLines <- seq(round_any(min(lon), 20), round_any(max(lon)-20, 20), 20)
  latLines <- seq(round_any(min(lat), 20), round_any(max(lat), 20), 20)
  
  #Create the color palette
  if (sum(Values<0)>1 && zeroPlus == F){
    underZero <- length(seq((cutoff*-1),0,.1))
    overZero <- length(seq(0,cutoff,.1))
    pal <- colorRamp(c(lowCol, "white"))
    pal <- pal(seq(0, 1, len = underZero))
    pal2 <- colorRamp(c("white", highCol))
    pal2 <- pal2(seq(0, 1, len = overZero))
    pal3 <- rbind(pal, pal2[-1,])
    legendLims <- as.character(seq((-1*cutoff),cutoff,.1))
  }else{
    overZero <- length(seq(0,cutoff,.1))
    pal2 <- colorRamp(c("white", highCol))
    pal3 <- pal2(seq(0, 1, len = overZero))
    legendLims <- as.character(seq(0,cutoff,.1))
  }
  
  
  cols <- rgb(pal3, maxColorValue = 255)
  
  # create a set of coordinates to form the outline of the curved plot
  shape<-data.frame(c(ylim[1], ylim[1], ylim[2], ylim[2], ylim[1], ylim[1]), 
                    c(mean(xlim[2], xlim[1]), xlim[1], xlim[1], xlim[2], xlim[2], mean(xlim[2], xlim[1])))
  
  #Cover up plotted map outside bounds
  shape2<-shape
  colnames(shape2) <- c("y", "x")
  full_map <- cbind.data.frame(c(-90,90,90,-90,-90), c(0,0,360,360,0))
  colnames(full_map) <- c("y", "x")
  shape2 <- rbind.data.frame(full_map, shape2)
  
  
  # Create the plot
  p <- ggplot(dfMap, aes(x=Longitude, y=Latitude, fill=Value)) + 
    geom_shape(data=shape, aes(x=shape[,2]+0.5, y = shape[,1]-0.5), colour=NA, fill = color_na) +
    theme_bw() + geom_tile()
  p <- p + scale_fill_manual(values = cols, limits = legendLims)
  p <- p + labs(title=mainTitle, subtitle = SubTitle)
  p <- p + labs(plot.title = element_text(size = rel(1.5))) 
  p <- p + geom_polygon(data=baseData, aes(x=long, y=lat, group=group), 
                        colour="black", fill="grey70", alpha=1)
  p <- p + geom_polygon(data=baseData2, aes(x=long, y=lat, group=group), 
                        colour="black", fill="grey70", alpha=1)
  p <- p + labs(fill=legendTitle)
  if (gridLines){
    p <- p + geom_hline(yintercept  = latLines) + geom_vline(xintercept = lonLines)
  }
  if (outline){
    p <- p + geom_shape(data=shape, aes(x=shape[,2]+0.5, y = shape[,1]-0.5), colour="black", fill = NA, size = 3)
    p <- p + annotate("text", x = 247.5, y = 18.5, label = "20° N", size = 6, angle = 38) + 
      annotate("text", x = 132, y = 61.5, label = "60° N", size = 6, angle = -35) +
      annotate("text", x = 117, y = 53.5, label = "120° E", size = 6, angle = 43) + 
      annotate("text", x = 257, y = 28, label = "100° W", size = 6, angle = -48)
  }
  if (curved){
    geom_polygon(data=shape2, aes(x=x, y=y), fill="white", colour="white", size = 1)
    p <- p + coord_map("conic", lat0 = 40, xlim = xlim, ylim = ylim)
    p <- p + theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(fill = NA),
                   panel.ontop = TRUE,
                   panel.border = element_blank(),
                   panel.margin = margin(t = 40, r = 0, b = 0, l = 0, unit = "pt"),
                   axis.text = element_blank(),
                   axis.ticks = element_blank(),
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank(),
                   legend.position="right",
                   legend.key.width = unit(1,"cm"),
                   axis.title = element_text(size=20),
                   plot.title = element_text(size=20),
                   plot.subtitle = element_text(size=15),
                   legend.title = element_text(size=12),
                   legend.text = element_text(size=12))
    p <- p + scale_y_continuous(expand = c(0,0))
    p <- p + scale_x_continuous(expand = c(0,0))
  }else{
    p <- p + coord_cartesian(xlim = xlim, ylim = ylim)
    p <- p + scale_y_continuous(breaks = latLines, expand = c(0,0))
    p <- p + scale_x_continuous(breaks = lonLines, expand = c(0,0))
    p <- p + theme(
      axis.title = element_text(size=30),
      plot.title = element_text(size=30),
      plot.subtitle = element_text(size=20),
      axis.text = element_text(size=20),
      legend.title = element_text(size=20),
      legend.text = element_text(size=20))
  }
  
  p
  
  return(p)
}
