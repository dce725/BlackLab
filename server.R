library(ggplot2)
library(DataCombine)
library(grid)
library(gridExtra)
library(dplR)
library(DescTools)
library(stats)
library(rowr)
library(shiny)
library(shinyjs)
library(shinydashboard)
suppressWarnings(suppressMessages(library(zoo)))
suppressWarnings(suppressMessages(library(zoocat)))

source("custum_functions/normalise_v2.R")
source("custum_functions/correl_heat_map.R")
source("custum_functions/lead_lag_v2.R")
source("custum_functions/num_round.R")
source("custum_functions/graph_theme.R")
server <- function(input, output, session) {
  
############ Tabpanel viewing ########################################  
  
  observe({
    toggle(condition = input$single, selector = "#navbar li a[data-value=single]")
  })
  observe({
    toggle(condition = input$chrono, selector = "#navbar li a[data-value=chrono]")
  })

################### Load data #######################################  
  mydata = reactive({
    inFile = input$file1
    if (is.null(inFile))
      return(NULL)
    tbl = read.csv(inFile$datapath, header=input$header, sep=input$sep,  dec = input$dec)
    return(tbl)
    attach(mydata)
  })
  
  chronology = reactive({
    inFile = input$file2
    if (is.null(inFile))
      return(NULL)
    tbl = read.csv(inFile$datapath, header=input$header, sep=input$sep,  dec = input$dec)
    return(tbl)
    attach(chronology)
  })
  
  output$table<- renderTable({mydata()})
  
  output$table4<- renderTable({chronology()})

########### detrending and combine with chronoology #################  
    
  detrending<- function(){
    de.tnd<-normalise(the.data = mydata(), input$detrending_select, input$splinewindow)
    return(de.tnd)
    }
    
  chron_mean <- function(){    
    new<-normalise(the.data = chronology(), input$detrending_select, input$splinewindow)
    chron_mean<-rowMeans(new[,-1], na.rm = T)
    chron_dat<-data.frame(new[,1],chron_mean)
    return(chron_dat)
  }
  
  chron_n_shells<-function(){
    
    chron_dat<-chron_mean() 
    de.tnd<-detrending()
    shell_IDs<-colnames(de.tnd)
    chron_len<-length(chron_dat[,1])
    series_len<-length(de.tnd[,1])
    chron_strt<-chron_dat[1,1]
    no.series<-ncol(de.tnd)
    
    if(chron_len<series_len){
      NA_ser<-rep(NA,(series_len-chron_len))
      chron_tmp<-c(chron_dat[,2],NA_ser)
      years<-c(chron_strt:chron_strt+series_len)
      chron_dat<-data.frame(years,chron_tmp)
      colnames(chron_dat)<-c("Year","Mean Chronology")
      de.tnd<-cbind(chron_dat,de.tnd[,-1])
    } else if(chron_len>series_len){
      NA.frame<-data.frame(matrix(NA, nrow = (chron_len-series_len), ncol = no.series))
      colnames(NA.frame)<-shell_IDs
      de.tnd<-rbind(de.tnd,NA.frame)
      de.tnd<-data.frame(chron_dat,de.tnd[,-1])
      
    } else  if(chron_len==series_len){
      de.tnd<-cbind(chron_dat,de.tnd[,-1])}
    colnames(de.tnd)<-c("Year","Mean chronology", shell_IDs[-1])
    
    return(de.tnd)
  }
 
  observe({
    x <- names(mydata()[-1])
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- names(mydata()[-1])
    
    # Can also set the label and select items
    updateSelectInput(session, "first_series",
                      label = paste("Select input label", length(x)),
                      choices = x,
                      selected = head(x, 1)
    )
  })
  observe({
    x <- names(mydata()[-1])
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- names(mydata()[-1])
    
    # Can also set the label and select items
    updateSelectInput(session, "second_series",
                      label = paste("Select input label", length(x)),
                      choices = x,
                      selected = head(x, 1)
    )
  })
  
  plotting1<- function(){
    #-----------------------------------------------------------#
    #How many columns of data to analyse?
    orig<-mydata()
    orig<-data.frame(orig[,1],orig[[input$first_series]])
    
    new<-detrending()
    new<-data.frame(new[,1], new[[input$first_series]])
    
    #-----------------------------------------------------------#     
    
    plot1<-ggplot() +
      geom_line(data = new, aes(x = new[,1], y = new[,2]), na.rm=TRUE, alpha =1,  size = input$plot.line) + crossdateR_theme(text.size = input$text.size, line.width = input$line.width,l=20) +
      ylab(expression(atop("Standardised", paste("increment width")))) + xlab("Year")
    plot2<- ggplot() +
      geom_line(data = orig, aes(x = orig[,1], y = orig[,2]), na.rm=TRUE, alpha =0.5, size = input$plot.line) + crossdateR_theme(text.size = input$text.size, line.width = input$line.width,l=20) +
        geom_smooth(data = orig, aes(x = orig[,1], y = orig[,2]), method = "loess", span = 0.1, size = input$plot.line+1, colour= "black", na.rm = TRUE) +
      labs(title = paste0(input$first_series)) +ylab("Increment width") + xlab("Year")
 
    g1 <- ggplotGrob(plot1)
    g2<-  ggplotGrob(plot2)
    g<-rbind(g2, g1, size = "first")
    both <-  grid.draw(g)
    return(both)
  }
  
  output$plot <- renderPlot({
    grid.newpage()
    grid.draw(plotting1())
  })
  
##################### Pairwise plot ###############################
  
  observe({
    x <- names(mydata()[-1])
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- names(mydata()[-1])
    
    # Can also set the label and select items
    updateSelectInput(session, "pairwise_series_1",
                      label = paste("Select input label", length(x)),
                      choices = x,
                      selected = head(x, 1)
    )
  })
  observe({
    x <- names(mydata()[-1])
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- names(mydata()[-1])
    
    # Can also set the label and select items
    updateSelectInput(session, "pairwise_series_2",
                      label = paste("Select input label", length(x)),
                      choices = x,
                      selected = head(x, 1)
    )
  })
  
  pairwise_plot_fun<- function(){
    the.data<-detrending()
    series_1yrs<-c((the.data[,1])+input$PS_1_lag)
    series_2yrs<-c((the.data[,1])+input$PS_2_lag)
    
    series_1<-data.frame(series_1yrs, the.data[[input$pairwise_series_1]])
    series_2<-data.frame(series_2yrs, the.data[[input$pairwise_series_2]])
   
    series_1<-subset(series_1, (complete.cases(series_1)))
    series_2<-subset(series_2, (complete.cases(series_2)))
    
  plot1<-ggplot()+
    geom_line(data = series_1, aes(x=series_1[,1], y=series_1[,2]), na.rm=TRUE, colour="black", size = input$plot.line) + labs(title = paste0(input$pairwise_series_1, " lagged by ", input$PS_1_lag, " years (black line) and ", input$pairwise_series_2, " lagged by ", input$PS_2_lag, " years (red line)" )) +
    geom_line(data = series_2, aes(x=series_2[,1], y=series_2[,2]), na.rm=TRUE, colour="red", size = input$plot.line) + crossdateR_theme(text.size = input$text.size, line.width = input$line.width) +
    ylab("Standardised increment width") + xlab("Year")
  return(plot1)
    }
  
  output$pairwise_plot <- renderPlot({
    pairwise_plot_fun()
  })
  
##################### Pairwise heat maps ##########################  
  
  observe({
    x <- names(mydata()[-1])
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- names(mydata()[-1])
    
    # Can also set the label and select items
    updateSelectInput(session, "sing_first_series",
                      label = paste("Select input label", length(x)),
                      choices = x,
                      selected = head(x, 1)
    )
  })
  observe({
    x <- names(mydata()[-1])
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- names(mydata()[-1])
    
    # Can also set the label and select items
    updateSelectInput(session, "sing_second_series",
                      label = paste("Select input label", length(x)),
                      choices = x,
                      selected = head(x, 1)
    )
  })
  single_heat_map_analysis<-eventReactive(input$go2, {
    run_cor_res<-correl_heat_map(the.data = detrending(), input$sing_first_series, input$sing_second_series, input$neg_lag,input$pos_lag, input$cor_win, input$cor_win)
    return(run_cor_res)
    run_cor_res<-data.frame()
  })
  
  plotting_sing_hm<- function(){
    output_correls<-single_heat_map_analysis()
    plot.data<-output_correls
    
    if(input$adj_lag1){plot.data<-subset(plot.data,((plot.data[,2]>=input$p_neg_lag1)) & (plot.data[,2]<=input$p_pos_lag1)) 
    } else {plot.data<-plot.data}
    
    if(input$adj_x_sing){plot.data<-subset(plot.data,((plot.data[,1]>=input$min_x_sing)) & (plot.data[,1]<=input$max_x_sing)) 
    } else {plot.data<-plot.data}
    
    new<-detrending()
    s1<-input$sing_first_series
    s2<-input$sing_second_series
    series.names<-colnames(new)
    x.ticks<-input$sing.hm.x.tick.spc
    y.ticks<-input$sing.hm.y.tick.spc
    plot.title<- paste0(s1, " vs ", s2)
    y.lab=paste0("lag (years from ", s1, ")")
   
     # Plot a heat map of the correlations    
    plot2<-ggplot(plot.data, aes(x=plot.data[,1], y=plot.data[,2]), na.rm=TRUE) + geom_raster(aes(fill = plot.data[,3])) + crossdateR_theme(text.size = input$text.size, line.width = input$line.width) + 
      scale_fill_gradientn(colours = c("#4575b4","#e0f3f8","#d73027"), limits = c(-1,1)) +labs(fill = "Correl. (R)", x = "Year", y= y.lab, title=plot.title) + 
      scale_x_continuous(breaks = seq(min(plot.data[,1]), max(plot.data[,1]), by = x.ticks)) + 
      scale_y_continuous(breaks = seq(min(plot.data[,2]), max(plot.data[,2]), by = y.ticks)) 
    
    panel_height = unit(1,"npc") - sum(ggplotGrob(plot2)[["heights"]][-3]) - unit(1,"line")
    plot2<- plot2 + guides(fill= guide_colorbar(barheight=panel_height))
    
    return(plot2)
    }
    
  output$plot_sing_hm <- renderPlot({
    grid.newpage()
    grid.draw(plotting_sing_hm())})
  
###################### chronology heat maps ##########################
  
  observe({
    x <- names(mydata()[-1])
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- names(mydata()[-1])
    
    # Can also set the label and select items
    updateSelectInput(session, "plot2_second_series",
                      label = paste("Select input label", length(x)),
                      choices = x,
                      selected = head(x, 1)
    )
  })
  
  heat_map_analysis<-eventReactive(input$go, {
    run_cor_res<-correl_heat_map(the.data = chron_n_shells(), 2, input$plot2_second_series,input$neg_lag,input$pos_lag, input$cor_win, input$cor_win)
    return(run_cor_res)
    run_cor_res<-data.frame()
  })
  
  chron_hm<- function(){
    output_correls<-heat_map_analysis()
    plot.data<-output_correls
    
    if(input$adj_lag2){plot.data<-subset(plot.data,((plot.data[,2]>=input$p_neg_lag2)) & (plot.data[,2]<=input$p_pos_lag2)) 
    } else {plot.data<-plot.data}
    
    if(input$adj_x_chron_hm){plot.data<-subset(plot.data,((plot.data[,1]>=input$min_x_chron_hm)) & (plot.data[,1]<=input$max_x_chron_hm)) 
    } else {plot.data<-plot.data}
    
    new<-chron_n_shells()
    
    s2<-input$plot2_second_series
    series.names<-colnames(new)
    plot.title<- paste0("Mean chronology vs ", s2)
    y.lab=paste0("lag (years from the chronology)")
    # Plot a heat map of the correlations    
    plot2<-ggplot(plot.data, aes(x=plot.data[,1], y=plot.data[,2]), na.rm=TRUE) + geom_raster(aes(fill = plot.data[,3])) + crossdateR_theme(text.size = input$text.size, line.width = input$line.width) + 
      scale_fill_gradientn(colours = c("#4575b4","#e0f3f8","#d73027"), limits = c(-1,1)) +labs(fill = "Correl. (R)", x = "Years", y= y.lab, title=plot.title) 
    
    panel_height = unit(1,"npc") - sum(ggplotGrob(plot2)[["heights"]][-3]) - unit(1,"line")
    plot2<- plot2 + guides(fill= guide_colorbar(barheight=panel_height))
    
    return(plot2)
  }
  
  output$chron_hm <- renderPlot({
    grid.newpage()
    grid.draw(chron_hm())})
  
################## lead lag correlations #################### 
  
  interseries<-function(){
    cross_dat_res<-lead_lag(detrending(), input$neg_lag, input$pos_lag, T)
    
    return(cross_dat_res)
  }
  
  chron_lead_lag<-function(){
    cross_dat_res<-lead_lag(chron_n_shells(),  input$neg_lag, input$pos_lag, F)
    # now calculate the overlap between the aligned dates and the chronology

    rep<-2
    overlap<-c(NA)
    while (rep<=nrow(cross_dat_res)){
      chron.rang<-c(as.numeric(cross_dat_res[1,3]),as.numeric(cross_dat_res[1,4]))
      ser.rang<-c(as.numeric(cross_dat_res[rep,3]),as.numeric(cross_dat_res[rep,4]))
      over.ser<-as.numeric(Overlap(chron.rang,ser.rang))
      overlap<-c(overlap,over.ser)
      rep<-rep+1}
    
    cross_dat_res<-data.frame(cross_dat_res[,1:8],overlap,cross_dat_res[,9:14])
    cross_dat_res[,6]<-as.numeric(cross_dat_res[,6])
   colnames(cross_dat_res)<-c("", "Series ID", "Date of selltedment", "Date of Death","col", "1st lag (years)", "1st R", " 1st P", "Overlap", "2nd lag (years)", "2nd R", "2nd P", "3rd lag (years)", "3rd R", "3rd P")  
   
   cross_dat_res<-cross_dat_res[-(nrow(cross_dat_res)),]
  return(cross_dat_res)
  }
  
############### Chronology comparison and results############
  
  observe({
    x <- names(mydata()[-1])
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- names(mydata()[-1])
    
    # Can also set the label and select items
    updateSelectInput(session, "ser_sel_chron",
                      label = paste("Select input label", length(x)),
                      choices = x,
                      selected = head(x, 1)
    )
  })
  
  chron_mean_plot<-function(){
    chron_plot_dat<-chron_n_shells()
    series_name<-colnames(chron_plot_dat)
    
    ser.sel<-chron_plot_dat[[input$ser_sel_chron]]
    
    plot.title<-paste0("Mean chronology (black line) vs ", input$ser_sel_chron, " (red line) adjusted by ",input$lag_series, "years." )
    mod_years<-chron_plot_dat[,1]+input$lag_series
    mod_ser<-data.frame(mod_years,ser.sel)
    
    min_x<-as.numeric(input$x_start)
    max_x<-as.numeric(input$x_end)
    
    if(input$chron_x_axis){
      mod_ser  <-subset(mod_ser, (mod_ser[,1]>=min_x) & (mod_ser[,1]<= max_x))
      chron_plot_dat <-subset(chron_plot_dat, (chron_plot_dat[,1]>=min_x) & (chron_plot_dat[,1]<= max_x))
    }
    
    plot_1<-ggplot()+
      geom_line(data=chron_plot_dat, aes(x=chron_plot_dat[,1], y=chron_plot_dat[,2]), na.rm=TRUE, size = input$plot.line) + crossdateR_theme(text.size = input$text.size, line.width = input$line.width) +
      geom_line(data=mod_ser, aes(x=mod_ser[,1], y=mod_ser[,2]), na.rm=TRUE, colour="red", size = input$plot.line) +
      ylab("Standardised growth index") +
      xlab("Year") +
      labs(title=plot.title)
    
    return(plot_1)
  }
  
  output$chron_mean <- renderPlot({chron_mean_plot()})
  
  ring_error<-function(){
    lag_data<-chron_lead_lag()
    new<-chron_n_shells()
    years<-new[,1]
    s2<-input$chron_shell
    a<-new[,2]
    b<-new[[input$ser_sel_chron]]
    
    #adjust the dates of the series to be at the best lag position
    lag<-input$lag_series
    if (lag>0){NA.ser<-rep(NA,abs(lag))
    years<-c(years,NA.ser)
    a<-c(a,NA.ser)
    b<-c(NA.ser,b)
    } else if (lag<0) {NA.ser<-rep(NA,abs(lag))
    years<-c(NA.ser,years)
    a<-c(NA.ser,a)
    b<-c(b,NA.ser)
    } 
    
    new<-data.frame(years,a,b, stringsAsFactors = T)
    colnames(new)<-c("years","A","B")
    shell_ID<-colnames(chron_n_shells())
    
    new<-subset(new, (!is.na(new)))
    
    run_cor_res<-correl_heat_map(the.data = new, 2, 3, -5, 5, input$cor_win, input$cor_win)
    return(run_cor_res)
    #-----------------------------------------------------------#
    
  }
  
  plot_ring_error<- function(){
    output_correls<-ring_error()
    new<-detrending()
    
    s2<-input$chron_shell
    series.names<-colnames(new)
    plot.title<- paste0("Mean chronology vs ", input$chron_shell)
    y.lab=paste0("lag (years from mean chronology)")
    # Plot a heat map of the correlations    
    plot2<-ggplot(output_correls, aes(x=output_correls[,1], y=output_correls[,2]), na.rm=TRUE) + geom_raster(aes(fill = output_correls[,3])) + 
      crossdateR_theme(text.size = input$text.size, line.width = input$line.width) + 
      scale_fill_gradientn(colours = c("#4575b4","#e0f3f8","#d73027"), limits = c(-1,1)) +labs(fill = "Correl. (R)", x = "Years", y= y.lab)  +
      scale_y_continuous(breaks = seq(-5, 5, by = 1)) +
      scale_x_continuous(breaks = seq(min(output_correls[,1]), max(output_correls[,1]), by = 10))
    
    panel_height = unit(1,"npc") - sum(ggplotGrob(plot2)[["heights"]][-3]) - unit(1,"line")
    plot2<- plot2 + guides(fill= guide_colorbar(barheight=panel_height))
    
    return(plot2)
  }
  
  output$ring_error <- renderPlot({
    grid.newpage()
    grid.draw(plot_ring_error())})
  
  output$table2<-renderTable(interseries()[,-5])
  
  output$chron_dating<-renderTable({
    cross_dates<-chron_lead_lag()[-1,-5]
    cross_dates<-subset(cross_dates,(as.numeric(cross_dates[,7])<input$sig_val)) # significance P<
    cross_dates<-subset(cross_dates,(as.numeric(cross_dates[,8])>=input$overlap))
    cross_dates
  })
  
####### Add crossdated series to chronology #################  
  
  new_chron<-function(){
    chrono<-chronology()
    new.series<-mydata()
    cross_dates<-chron_lead_lag()
    
    n.series<-ncol(new.series)
    cross_dates<-subset(cross_dates,(cross_dates[,8]<=input$sig_val))
    cross_dates<-subset(cross_dates,(cross_dates[,9]>=input$overlap))
    
    col.num<-cross_dates[,5]-1 #the -1 is because the col numbers in the crossdates frame refer to the chron_n_shells dframe not the de.tnd dframe
    
    min.nw.ser<-min(as.numeric(cross_dates[,3]))
    max.nw.ser<-max(as.numeric(cross_dates[,4]))
    chron.min<-min(chrono[,1])
    chron.max<-max(chrono[,1])
    
    # generate a new year colum that spans the full range of dates of the new chronology data
    new.years<-c(min(c(min.nw.ser,chron.min)):max(c(max.nw.ser,chron.max)))
    new.chrono<-data.frame(new.years)
    
    if(chron.min>min.nw.ser){
      NA.frame<-data.frame(matrix(NA, nrow = abs(chron.min-min.nw.ser), ncol = (ncol(chrono)-1)))
      colnames(NA.frame)<-colnames(chrono[,-1])
      chrono.tmp<-rbind(NA.frame,chrono[,-1])
      new.chrono<-cbind.fill(new.chrono,chrono.tmp, fill=NA) 
    } else {new.chrono<-cbind.fill(new.chrono,chrono[,-1], fill=NA)}
    
    new.data<-new.series[,col.num]
    
    lags<-c()
    mod.new.series<-data.frame()
    
    # genereate list of new lags
    for (i in 1:nrow(cross_dates)){
      ser.dat<-new.data[,i]
      d.of.s<-cross_dates[i,3]
      lag<-as.numeric(d.of.s-min(new.chrono[,1]))
      lags<-c(lags,lag)}
    lag<-NULL
    # adjust the series to the appropriate lag
    
    
    for (i in 1:nrow(cross_dates)){  
      lag<-lags[i]
      ser.dat<-new.data[,i]
      
      if (lag>=1){
        NA.ser<-rep(NA, lag)   
        mod.ser<-c(NA.ser,ser.dat)   
        mod.new.series<-cbind.fill(mod.new.series,mod.ser, fill=NA)
      } else{mod.new.series<-cbind.fill(mod.new.series,ser.dat, fill=NA)}  
      
      mod.ser<-NULL
      lag<-NULL
    }
    
    new.chrono<-cbind.fill(new.chrono,mod.new.series[,-1], fill=NA)  
    
    chron.names<-colnames(chrono)
    ser.nam<-cross_dates[,2]
    colnames(new.chrono)<-c(chron.names,ser.nam)
    return(new.chrono)
    
  }
  
  
  
  
###################### Save outputs #########################  
  
  
  output$add_to_chron<- downloadHandler(
    filename = function() {
      paste("new_chron_raw_data.csv", sep = "")
    },
    content = function(file) {
      write.csv(new_chron(), file, row.names = FALSE)
    })
  
  output$download_detrend <- downloadHandler(
    filename = function() {
      paste("detrended_data.csv", sep = "")
    },
    content = function(file) {
      write.csv(detrending(), file, row.names = FALSE)
    })
  
  output$downloadsingplt = downloadHandler(
    filename = 'single_plot.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = input$width, height = input$height,
                       res = input$resolution, units = "in")
      }
      ggsave(file, plot = plotting1(), device = device)
    })
  
  output$downloadmultplt = downloadHandler(
    filename = 'multi_plot.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = input$width, height = input$height,
                       res = input$resolution, units = "in")
      }
      ggsave(file, plot = multiplot(), device = device)
    })
  
  
  output$downloadsinghtmp = downloadHandler(
    filename = 'single_heatmap.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = input$width, height = input$height,
                       res = input$resolution, units = "in")
      }
      ggsave(file, plot = plotting_sing_hm(), device = device)
    })
  
  output$downloachronhtmp = downloadHandler(
    filename = 'single_heatmap.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = input$width, height = input$height,
                       res = input$resolution, units = "in")
      }
      ggsave(file, plot = chron_hm(), device = device)
    })
  output$download2 = downloadHandler(
    filename = 'outputs/testtoday.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = input$width, height = input$height,
                       res = input$resolution, units = "in")
      }
      ggsave(file, plot = plotting2(), device = device)
    })
  
  output$download3 = downloadHandler(
    filename = 'outputs/testtoday.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = input$width, height = input$height,
                       res = input$resolution, units = "in")
      }
      ggsave(file, plot = plotting3(), device = device)
    })
  
######################## DEBUG #################################
  
  output$test<-renderTable({
    
  })
################################################################  
}