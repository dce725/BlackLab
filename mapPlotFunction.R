# This function just produces a map plot. It doesn't do the correlations.

# zero lon should be the prime meridion
# zero lat should be the equator
# Example positions:
# Pacific centre: -250 to -100 or 110 to 260
# Atlantic centre: -110 to 30
# Pacific and Atlantic with USA in the centre: -230 to 30

# Note 1: if you put in a large geographic range, e.g. the entire Northern Hemisphere, the y axis labels may not display properly

# Note 2: You will get the following warning message:
# Warning message:
# In is.na(x) : is.na() applied to non-(list or vector) of type 'expression'
# This is due to using an expression to generate the axis labels. It can be ignored.

# Note 3: The axis labels will bunch up when viewing plots in a small viewer.
#         To avoid this I save the plots (10" width 300 dpi) and then view the image  
#         

# The four column data frame(the_data) should already be filtered to just contain data for the region to be plotted.
require(ggplot2)
require(mapproj)
require(ggforce)

point_correl_map<-function(
              the_data = the_data, # a four col data frame [Lon,lat, R, P] The R col is the data to be plotted.
              env_data = "Gridded data name", # The name of the gridded data product
              series = c("proxy names one","proxy names two"), # a list of the proxy names used in the analysis
              max_lat =  80, # set the lon. and lat. range for the plot
              min_lat = 0, 
              max_lon = 180, 
              min_lon = -180, 
              min_yr = 1900, # max_yr and min_yr for the period analysed (for the plot title)
              max_yr = 2000, 
              curved = FALSE, # what projection do you want?
              n_months = 12, #  instrumental monthly mean calcultated from start_month over n_months (for the plot title)
              start_month = 1,
              detrend = FALSE, # was the data detrended? (for the plot title)
              spline_len = 0, # what spline was the data detrended with? (for the plot title)
              low_pass = FALSE, # was the data low pass filtered? (for the plot title)
              low_spline = 0, # what spline was used to low pass filter the data?(for the plot title)
              fill_limits = c(-0.6,0.6), # select a range to plot the data over
              fill_title = "Correl. Coef. (r)", # give the fill bar a title
              file_name = paste0("point_correlation_plot_",Sys.time()), # File name to save plot. Don't add the extension.
              save_plot = FALSE,
              plot_width = 11, # width and height of plot to be saved, in inches.
              plot_height = 6,
              plot_dpi = 300 # resolution to save the plot with
              ){
  

  month_sst <- the_data

  if(max_lat>0 && min_lat>0){hem<-30 # 30 means northern hem projection
  } else if (max_lat<0 && min_lat<0){hem<--30 # -30 means southern hem projection
  } else {hem <-0}
  # run rest of script
  
  # Load the world map data
  WorldData <- map_data('world')
  
  east_world<-subset(WorldData,(WorldData$long>0))
  east_world$long<-east_world$long-360
  east_world$group<- paste0(east_world$group,"_1")
  east_world$region<- paste0(east_world$region,"_1")
  
  west_world<-subset(WorldData,(WorldData$long<0))
  west_world$long<-west_world$long+360
  west_world$group<- paste0(west_world$group,"_2")
  west_world$region<- paste0(west_world$region,"_2")
  
  WorldData<-rbind(WorldData, east_world, west_world)
  
  #Set the colours to use for plotting
  col_scale <- c("#4575b4","#e0f3f8","#d73027")
  
  # Filter the map overlap for the region of interest
  WorldData<-WorldData[WorldData$lon >= min_lon,]
  WorldData<-WorldData[WorldData$lon <= max_lon,]
  WorldData<-WorldData[WorldData$lat >= min_lat,]
  WorldData<-WorldData[WorldData$lat <= max_lat,]

  # create a set of coordinates to form the outline of the curved plot
  shape<-data.frame(c(min_lat, min_lat, max_lat, max_lat, min_lat, min_lat), c(mean(max_lon, min_lon), min_lon, min_lon, max_lon, max_lon, mean(max_lon, min_lon)))
  
    ### Plot options ###
  # set the spacing for the grid lines and axis labels
  X_space<-20
  y_space<-20
  
  x_pad<-10
  y_pad<-7
  
  #set the colour of the grid lines
  grid_col<-"light grey"
  
  #adjust the angle of the Y axis label
  if (curved){
    Y_axis_ang<--0.2385*(max_lon-min_lon)+85.769 # degrees. 
  } else {
    Y_axis_ang<-90
  }
  
  y_axis_space<-35
  
  border_thick<-0.75
  
  # create the labels for the axis
  lat_text<-data.frame(seq(from = min_lat+10, to = max_lat-10, by =y_space),seq(from = min_lat+10, to = max_lat-10, by =y_space), rep(min_lon-y_pad,length(seq(from = min_lat+10, to = max_lat-10, by =y_space))))
  lon_text<-data.frame(seq(from = min_lon+10, to = max_lon-10, by =X_space),seq(from = min_lon+10, to = max_lon-10, by =X_space), rep(min_lat-x_pad,length(seq(from = min_lon+10, to = max_lon-10, by =X_space))))
  
  # correct for the change in spacing between Y axis labels and edge of plot
  # this basically just shifts the position of the y-axis labels so they follow the 
  # tilt of the y axis in the curved projection
  # this will may need adjusting based on the range of latitudes being plotted
  correction_value<-2 
  
  if (curved){
    for (i in 1:length(lat_text[,3])){
      lat_text[i,3]<-lat_text[i,3]-i*correction_value
    }
  }
  
  vert_lines<-data.frame(seq(from = min_lon+10, to = max_lon-10, by = X_space), rep(min_lat,length(seq(from = min_lon+10, to = max_lon-10, by = X_space))), rep(max_lat,length(seq(from = min_lon+10, to = max_lon-10, by = X_space))))
  horiz_lines<-data.frame(seq(from = min_lat+10, to = max_lat-10, by = y_space), rep(min_lon,length(seq(from = min_lat+10, to = max_lat-10, by = y_space))), rep(max_lon,length(seq(from = min_lat+10, to = max_lat-10, by = y_space))))
  
  
  if (detrend){
    detrending<-paste0("Detrended with ", spline_len, " year spline")
  } else {
    detrending<-"No detrending applied"
  }
  
  if (low_pass){
    smoothing<-paste0("Low pass filtered with ", low_spline, " year spline")
  } else {
    smoothing<-"No smoothing applied"
  }
  sel_proxy<- series
  
  # Create text for the model used (this will become env data vs proxy data_1 + proxy data_2...)
  ser_names<- NULL
  if(length(sel_proxy>1)){
    for (i in 1:length(sel_proxy)){
      if (i==1){
        ser_names<-paste0(ser_names, " vs ", sel_proxy[i])
      } else {
        ser_names<-paste0(ser_names, " + ", sel_proxy[i])
      }
    }
  } else {ser_names<- paste0(" vs ",sel_proxy)}
  
  # PLot the data with a world map overlay
  
  plotted_anoms<-suppressWarnings(ggplot()+
                                    geom_tile(data = month_sst, aes(x= month_sst[,1], y = month_sst[,2], fill =month_sst[,3]), na.rm = T) +
                                    scale_fill_gradientn(colours = col_scale, limits = fill_limits)  +
                                    geom_map(data = WorldData, map = WorldData,aes(x = long, y = lat, map_id=region),
                                             fill = "#eaeaea", colour = "#000000", size=0.1)+
                                    scale_x_continuous(breaks = seq(from = min_lon+10, to = max_lon-10, by =20), limits=c((min_lon-y_pad-y_axis_space), max_lon+y_pad+y_axis_space+y_pad),expand = c(0, 0)) +
                                    scale_y_continuous(breaks = seq(from = min_lat+10, to = max_lat-10, by =10), limits=c((min_lat-x_pad-5),max_lat+5), expand = c(0, 0)) +
                                    xlab(expression(paste("Longitude ("^"o","E)"))) +labs(fill = fill_title) +
                                    labs(title = paste0("Correlation between ", env_data, ser_names, " \nCalculated between", analyse_from, " to ", analyse_to, "CE \nSeasonality = mean of months ", start_month, " to ", (start_month+n_months-1),"\n", detrending,"\n",smoothing)) +
                                    theme(panel.grid.major = element_blank(),
                                          panel.grid.minor = element_blank(),
                                          panel.background = element_rect(fill = NA),
                                          panel.ontop = TRUE,
                                          panel.border = element_blank(),
                                          panel.margin = margin(t = 40, r = 0, b = 0, l = 0, unit = "pt"),
                                          axis.text = element_blank(),
                                          axis.ticks = element_blank(),
                                          axis.title.y = element_blank(),
                                          legend.position="right",
                                          legend.key.width = unit(1,"cm"))+
                                    geom_shape(data=shape, aes(x=shape[,2], y = shape[,1]), colour="black", fill = NA, size = border_thick) +
                                    geom_text(data = lat_text, aes(x=lat_text[,3], y =lat_text[,2],label =lat_text[,1])) +
                                    geom_text(data = lon_text, aes(x=lon_text[,2], y =lon_text[,3],label =lon_text[,1])) +
                                    annotate("segment", x = vert_lines[,1], xend = vert_lines[,1], y = vert_lines[,2], yend = vert_lines[,3], colour =grid_col) +
                                    annotate("segment", x = horiz_lines[,2], xend = horiz_lines[,3], y = horiz_lines[,1], yend = horiz_lines[,1], colour =grid_col) +
                                    annotate(geom = "text", x = min_lon-y_pad-20, y = mean(c(min_lat,max_lat)), label = expression(paste("Latitude ("^"o","N)")), color = "black",
                                             angle = Y_axis_ang)
                                  ) 
  
  if (curved){
    plotted_anoms<-plotted_anoms  + coord_map("conic", lat0 = 30)
  }
  
  if (save_plot){
    ggsave(plotted_anoms, filename = paste0(file_name, ".png"), width = plot_width, height = plot_height, units = "in", dpi =  plot_dpi)
  }
  
  return(plotted_anoms)
  
}
