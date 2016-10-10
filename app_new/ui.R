library("shiny")
library("shinydashboard")
library("highcharter")
library("dplyr")
library("viridisLite")
library("markdown")
library("quantmod")
library("tidyr")
#library("ggplot2")
library("treemap")
library("forecast")
library("DT")
library(shiny)
library(leaflet)
#library(plotly)
rm(list = ls())

dashboardPage(
  skin = "black",
  dashboardHeader(title = "Crime Analysis", disable = FALSE),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map-marker")),
      menuItem("Examples", tabName = "examples", icon = icon("bar-chart")),
      menuItem("Time Series", tabName = "ts", icon = icon("line-chart")),
      menuItem("Plugins", tabName = "plugins", icon = icon("line-chart")),
      menuItem("Public Facilities",tabName = "public", icon = icon("list-alt"))
    ),
    div(includeMarkdown("crimeinfo.md"), style = "padding:10px")
  ),
  dashboardBody(
    tags$head(tags$script(src = "js/ga.js")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/custom_fixs.css")),
    tabItems(
      tabItem(tabName = "map", 
              sidebarLayout(position = "right", 
                  sidebarPanel(
                         h4("Filter"),
                          
                         # widget for crime type
                         
                         checkboxGroupInput("Crime_Type", label = "Crime_Type",
                                             choices = c("BURGLARY", "FELONY ASSAULT", "GRAND LARCENY",
                                                         "GRAND LARCENY OF MOTOR VEHICLE", "RAPE", "ROBBERY",
                                                         "MURDER & NON-NEGL. MANSLAUGHTE"),
                                             selected = c("BURGLARY", "FELONY ASSAULT", "GRAND LARCENY",
                                                          "GRAND LARCENY OF MOTOR VEHICLE", "RAPE","ROBBERY",
                                                          "MURDER & NON-NEGL. MANSLAUGHTE")),
                              
                         #date range
                         dateRangeInput("Date_Range", "Choose a date range", 
                                        start = "2015-10-01", end = "2015-12-31", 
                                        min = "2000-01-01", max = "2015-12-31"),
                              
                         #start and end hour
                         sliderInput("IntHour", "Start time", 0, 23, 0, step = 1),
                         sliderInput("EndHour", "End time", 0, 23, 23, step = 1),
                        
                         
                     
                         #update button
                         submitButton("Update"),
                         style = "opacity : 0.85"
                             ),
                            
                  mainPanel(
                            leafletOutput("map", width = "100%", height = 700)
                           )
              )
      ), 
              
              
              
      tabItem(tabName = "examples",
              fluidRow(
                column(4, selectInput("theme", label = "Theme",
                                      choices = c(FALSE, "fivethirtyeight", "economist", "dotabuff",
                                                  "darkunica", "gridlight",
                                                  "sandsignika", "null", "handdrwran",
                                                  "chalk"))),
                column(4, selectInput("credits", label = "Credits enabled", choices = c(FALSE, TRUE))),
                column(4, selectInput("exporting", label = "Exporting enabled", choices = c(FALSE, TRUE)))
                
              ),
              box(width = 6, highchartOutput("highchart")),
              #box(width = 6, highchartOutput("highmap")),
              box(width = 6, highchartOutput("highohlc")),
              box(width = 6, highchartOutput("highscatter")),
              box(width = 6, highchartOutput("highstreemap")),
              box(width = 6, highchartOutput("highheatmap")),
              box(width = 10, highchartOutput("highstock")),
              box(width = 2, title = "Filter",
                  checkboxGroupInput("Crimetype", label = "Crime Type: ",
                                     choices = c("GRAND LARCENY", "FELONY ASSAULT", "ROBBERY", 
                                                 "BURGLARY", "GRAND LARCENY OF MOTOR VEHICLE",
                                                 "RAPE", "MURDER"),
                                     selected =c("GRAND LARCENY", "FELONY ASSAULT", "ROBBERY", 
                                                 "BURGLARY", "GRAND LARCENY OF MOTOR VEHICLE",
                                                 "RAPE", "MURDER"))),
              #update button
              submitButton("Update"),
              style = "opacity : 0.85"
              ),
      tabItem(tabName = "ts",
              fluidRow(
                column(4, selectInput("ts", label = "Time series",
                                      choices = c("WWWusage", "AirPassengers",
                                                  "ldeaths", "USAccDeaths")))
              ),
              box(width = 12, highchartOutput("tschart")),
              box(width = 6, highchartOutput("tsforecast")),
              box(width = 6, dataTableOutput("dfforecast")),
              box(width = 6, highchartOutput("tsacf")),
              box(width = 6, highchartOutput("tspacf"))
              ),
    
      tabItem(tabName = "plugins",
              box(width = 12, highchartOutput("pluginsfa"))
              ),
      tabItem(tabName = "public",
              sidebarLayout(position = "right", 
                            sidebarPanel(
                              h4("Filter"),
                              
                              # widget for facility type
                              selectInput("Facility_Category", label = "Facility Category",
                                          choices = c("PUBLIC FACILITY", 
                                                      "ENTERTAINMENT", 
                                                      "RESIDENTIAL AREA",
                                                      "RESTAURANT/CAFE", 
                                                      "BAR"),
                                          selected = "Public Facility (Government Office, Schools, Hospital, Stores and Warehouse, etc.)"),
                              
                              # widget for crime type
                              checkboxGroupInput("p_Crime_Type", label = "Crime_Type",
                                                 choices = c("BURGLARY", "FELONY ASSAULT", "GRAND LARCENY",
                                                             "GRAND LARCENY OF MOTOR VEHICLE", "RAPE", "ROBBERY","MURDER & NON-NEGL. MANSLAUGHTE"),
                                                 selected = c("BURGLARY", "FELONY ASSAULT", "GRAND LARCENY",
                                                              "GRAND LARCENY OF MOTOR VEHICLE", "RAPE","ROBBERY","MURDER & NON-NEGL. MANSLAUGHTE")),
                              
                              #update button
                              submitButton("Update"),
                              style = "opacity : 0.85"
                            ),
                            mainPanel(
                              #plotOutput("facilitymap", width = "100%", height = 700)
                              highchartOutput("facilitymap", width = "100%", height = 700)
                            ))
              )
      )
    )
  )


