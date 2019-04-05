rm(list = ls())
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

source("custum_functions/normalise_V2.R")
source("custum_functions/correl_heat_map.R")
source("custum_functions/lead_lag_v2.R")
source("custum_functions/num_round.R")
source("custum_functions/graph_theme.R")

ui <- 
  dashboardPage(skin="green",
                
                dashboardHeader(title = "CrossdateR"),
                dashboardSidebar( useShinyjs(),
                                  sidebarMenu(
                                    
                                    menuItem("Load series", tabName = "data_load",
                                             fileInput('file1', 'Load undated series',
                                                       accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv')),
                                             tags$hr(),
                                             checkboxInput('header', 'Header', TRUE),
                                             radioButtons('sep', 'Separator',
                                                          c(Comma=',',
                                                            Semicolon=';',
                                                            Tab='\t'),
                                                          'Comma'),
                                             checkboxInput("single", "Just do pairwise comparisons between individual series", TRUE),
                                             checkboxInput("chrono", h3("Analyse against chronology"), FALSE),
                                             fileInput('file2', 'Load chronology data',
                                                       accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv'))
                                             
                                    ),
                                    
                                    menuItem("Correlation options",
                                             numericInput("No_series", "How many data sets are you comparing?", min = 2, value =3),
                                             numericInput("neg_lag", "Select negative lag (years)", min = 0, value =-20),
                                             numericInput("pos_lag", "Select positivelag (years)", min = 0, value =20),
                                             numericInput("cor_win", "Select correlation window (years)", min = 5, value =21)
                                             
                                    ),
                                    menuItem("Detrending options",
                                             selectInput("detrending_select", h4("Choose a detrending"), 
                                                         choices = list("Do nothing to my data" = 1, 
                                                                        "Convert to z-scores" = 2,
                                                                        "Spline detrending" = 3,
                                                                        "Negative exponential" = 4), selected = 3),
                                             numericInput("splinewindow", "Select a spline window (only applies to spline dentredning option)", value = 21, min = 5)
                                             ),
                                    menuItem("Plot options",
                                             numericInput("text.size", label = "Text size", value = 18),
                                             numericInput("plot.line", label = "Plotted line thickness", value = 0.5),
                                             numericInput("line.width", label = "Axis line weighting", value = 1),
                                             numericInput("resolution", label = "Set resolution of output file", value = 300),
                                             numericInput("width", label = "Set width of output file (in inches)", value = 6),
                                             numericInput("height", label = "Set height of output file (in inches)", value = 4)
                                    )
                                  )
                                  
                ),
                dashboardBody(
                  tabsetPanel(id = "navbar",
                              tabPanel("Individual series", value ="mand",
                                       fluidRow(box(width=12, downloadButton('download_detrend',label="Save individual detrended series"),
                                                    tableOutput("table")))),
                              
                              tabPanel("Chronology data", value ="chrono",
                                       tableOutput("table4")),
                              
                              tabPanel("Detrending Plot", value ="mand",
                                       fluidRow(
                                                box(width=9,
                                                    plotOutput("plot")),
                                                box(width=3, downloadButton('downloadsingplt',label="Download plot as png"),
                                                    selectInput("first_series", label = "Select first series", c(""))
                                                    )
                                       )),
                              
                              tabPanel("Pairwise comparison", value ="mand",
                                       fluidRow(
                                         box(width=9,
                                             plotOutput("pairwise_plot")),
                                         box(width=3,
                                             selectInput("pairwise_series_1", label = "Select first series", c("")),
                                             numericInput("PS_1_lag", "Lag series 1", value =0),
                                             selectInput("pairwise_series_2", label = "Select second series", c("")),
                                             numericInput("PS_2_lag", "Lag series 2", value =0)
                                         )),
                                       fluidRow(
                                         box(width=12,
                                             tableOutput("table2")))),
                              
                              tabPanel("Single Correlation heat map", value ="single",
                                       fluidRow(box(width=12,
                                                    plotOutput("plot_sing_hm")
                                                    )),
                                       fluidRow(box(width=4,
                                                    actionButton("go2", h3("Produce heat map")),
                                                    h3("Select series to analyse"),
                                                    selectInput("sing_first_series", label = "First series", c("")),
                                                    selectInput("sing_second_series", label = "Second series", c("")),
                                                    h3("Save plot"),
                                                    downloadButton('downloadsinghtmp',label="Download plot as png")),
                                                box(width=4, h3("Y axis options"),
                                                    checkboxInput("adj_lag1", "Adjust lag scale", value = F),
                                                    numericInput("p_neg_lag1", "Adjust negative lag (years)", value =-20),
                                                    numericInput("p_pos_lag1", "Adjust positivelag (years)", value =20),
                                                    numericInput("sing.hm.y.tick.spc", label = "Y-axis tick mark spacing", value = 10)),
                                                box(width=4, h3("x axis options"),
                                                    checkboxInput("adj_x_sing", "Adjust X axis scale", value = F),
                                                    numericInput("min_x_sing", "Adjust min X axis value (years)", value =1900),
                                                    numericInput("max_x_sing", "Adjust min X axis value (years)", value =2000),
                                                    numericInput("sing.hm.x.tick.spc", label = "x-axis tick mark spacing", value = 20)
                                                    
                                                    
                                                )
                                       )),
                              
                              tabPanel("chronology Correlation heat map", value ="chrono",
                                       fluidRow(box(width=12,
                                                    plotOutput("chron_hm"))),
                                       fluidRow(box(width=4, 
                                                    actionButton("go", h3("Produce heat map")),
                                                    h3("Select series to analyse against the chronology"),
                                                    selectInput("plot2_second_series", label = "", c("")),
                                                    h3("Save plot"),
                                                    downloadButton('downloachronhtmp',label="Download plot as png")),
                                                box(width=4, h3("Y axis options"),
                                                    checkboxInput("adj_lag2", "Adjust lag scale", value = F),
                                                    numericInput("p_neg_lag2", "Adjust negative lag (years)", value =-20),
                                                    numericInput("p_pos_lag2", "Adjust positivelag (years)", value =20)),
                                                box(width=4, h3("X axis options"),
                                                    checkboxInput("adj_x_chron_hm", "Adjust X axis scale", value = F),
                                                    numericInput("min_x_chron_hm", "Adjust min X axis value (years)", value =1900),
                                                    numericInput("max_x_chron_hm", "Adjust min X axis value (years)", value =2000)
                                                ))),
                              
                              
                              
                              tabPanel("chronology Comparion", value ="chrono",
                                       fluidRow(
                                         box(width=12, plotOutput("chron_mean"),
                                             plotOutput("ring_error"))),
                                       fluidRow(
                                         box(width=4, title ="Select series to analyse",
                                             selectInput("ser_sel_chron", label = "Select series to compare with the chronology", c(""))),
                                         box(width=4, title = "Add series to the chronology file",
                                             downloadButton("add_to_chron", "Add series to chronology"),
                                             numericInput("sig_val", label = "Set significance level required", value = 0.05, min = 0, max=1),
                                             numericInput("overlap", label = "Select minimum overlap required", value = 30, min = 5)
                                         ),
                                         box(width=4, title="Plot options",
                                             numericInput("lag_series", "Lag the series in the plot", value = 0),
                                             checkboxInput("chron_x_axis","Adjust the x-axis?", F),
                                             numericInput("x_start", "Min X value", value = 1900),
                                             numericInput("x_end", "Max X value", value = 2000)
                                         )),
                                       fluidRow( box(width=12,tableOutput("chron_dating"))
                                       )),
                              tabPanel("debug",
                                       tableOutput("test"))
                              
                  )
                ))