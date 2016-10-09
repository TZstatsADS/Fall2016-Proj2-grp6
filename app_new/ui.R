library("shiny")
library("shinydashboard")
library("highcharter")
library("dplyr")
library("viridisLite")
library("markdown")
library("quantmod")
library("tidyr")
library("ggplot2")
library("treemap")
library("forecast")
library("DT")
library(shiny)
library(leaflet)
rm(list = ls())

dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Crime Analysis", disable = FALSE),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Part1", tabName = "part1", icon = icon("map-signs")),
      menuItem("Part2", tabName = "part2", icon = icon("bar-chart")),
      menuItem("Part3", tabName = "part3", icon = icon("line-chart"))
    ),
    div(includeMarkdown("crimeinfo.md"), style = "padding:10px")
  ),
  
  dashboardBody(
    tags$head(tags$script(src = "js/ga.js")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/custom_fixs.css")),
    tabItems(
      tabItem(tabName = "part1",
          fluidRow(
              column(4, selectInput("theme", label = "Theme",
                      choices = c(FALSE, "fivethirtyeight", "economist", "dotabuff",
                                "darkunica", "gridlight", "sandsignika", "null", "handdrwran", "chalk")))
              ),
          
          sidebarLayout(position = "right", 
             sidebarPanel(
                 h4("Filter"),
                              
                 # widget for crime type
                 checkboxGroupInput("Crime_Type", label = "Crime_Type",
                      choices = c("BURGLARY", "FELONY ASSAULT", "GRAND LARCENY",
                                  "GRAND LARCENY OF MOTOR VEHICLE", "RAPE", "ROBBERY"),
                      selected = c("BURGLARY", "FELONY ASSAULT", "GRAND LARCENY",
                                  "GRAND LARCENY OF MOTOR VEHICLE", "RAPE","ROBBERY")),
                              
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
      tabItem(tabName = "part2",
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
              box(width = 12, highchartOutput("highstock"))
      )
      
      
      
      
      
      )
    )
  )
