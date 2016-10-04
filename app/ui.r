#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

# Define UI for the Crime Map application
shinyUI(navbarPage("Crime Map", id="nav",theme = "bootstrap.css",
                   
                   tabPanel("part1",
                            sidebarLayout(position = "right", 
                                          sidebarPanel(
                                            h4("Filter"),
                                            
                                            # widget for crime type
                                            checkboxGroupInput("Crime_Type", label = "Crime_Type",
                                                               choices = c("BURGLARY", "FELONY ASSAULT", "GRAND LARCENY",
                                                                           "GRAND LARCENY OF MOTOR VEHICLE", 
                                                                           "RAPE", "ROBBERY"),
                                                               selected = c("BURGLARY", "FELONY ASSAULT", "GRAND LARCENY",
                                                                            "GRAND LARCENY OF MOTOR VEHICLE",
                                                                            "RAPE","ROBBERY")),
                                            
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
                            )),
                   tabPanel("part2"),
                   tabPanel("part3")
))
