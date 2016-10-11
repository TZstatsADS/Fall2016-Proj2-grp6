library("shiny")
library("shinydashboard")
library("highcharter")
library("dplyr")
library("viridisLite")
library("markdown")
library("quantmod")
library("tidyr")
library("treemap")
library("forecast")
library("DT")
library("shiny")
library("leaflet")
library("plotly")
library("wordcloud2")
rm(list = ls())

dashboardPage(
  skin = "black",
  dashboardHeader(title = "Crime Analysis", disable = FALSE),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map-marker")),
      menuItem("Time Series", tabName = "ts", icon = icon("line-chart")),
      menuItem("Public Facilities Allocation",tabName = "public", icon = icon("list-alt")),
      menuItem("311 Complaint",tabName = "311", icon = icon("bar-chart")),
      menuItem("Prediction", tabName = "predict", icon = icon("table"))
    ),
    div(includeMarkdown("crimeinfo.md"), style = "padding:10px")
  ),
  dashboardBody(
    tags$head(tags$script(src = "js/ga.js")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/custom_fixs.css")),
    tabItems(
      
      ################################################################################################
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
                         
                         h4("Click the Update button to see the map: "),
                         #update button
                         actionButton("button", "Update", 
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                  ),
                            
                  mainPanel(
                            leafletOutput("map", width = "100%", height = 650)
                  )
              )
      ), 
              
      ################################################################################################              
      tabItem(tabName = "ts",
              fluidRow(
                column(4, selectInput("theme", label = "Theme",
                                      choices = c(FALSE, "fivethirtyeight", "economist", "dotabuff",
                                                  "darkunica", "gridlight",
                                                  "sandsignika", "null", "handdrwran",
                                                  "chalk"))),
                column(4, selectInput("exporting", label = "Exporting enabled", choices = c(FALSE, TRUE)))
                
              ),
              box(width = 10, highchartOutput("highstock")),
              box(width = 2, title = "Filter",
                  checkboxGroupInput("Crimetype", label = "Crime Type: ",
                                     choices = c("GRAND LARCENY", "FELONY ASSAULT", "ROBBERY", 
                                                 "BURGLARY", "GRAND LARCENY OF MOTOR VEHICLE",
                                                 "RAPE", "MURDER"),
                                     selected =c("GRAND LARCENY", "FELONY ASSAULT", "ROBBERY", 
                                                 "BURGLARY", "GRAND LARCENY OF MOTOR VEHICLE",
                                                 "RAPE", "MURDER")), 
                  actionButton("button2", "Update", 
                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4")), 
              box(width = 12, highchartOutput("highheatmap")),
              fluidRow(
                column(4, selectInput("ct", label = "Crime Type: ",
                                      choices = c("GRAND LARCENY", "FELONY ASSAULT", "ROBBERY", 
                                                  "BURGLARY", "GRAND LARCENY OF MOTOR VEHICLE",
                                                  "RAPE", "MURDER")))
              ),
              box(width = 12, plotOutput("forecast"))
              ),
      
      ################################################################################################              
      tabItem(tabName = "public",
              absolutePanel(
                bottom = 120, right = 30, width = 300,
                height = "auto",draggable = TRUE, 
                wellPanel(
                  HTML(markdownToHTML(fragment.only=TRUE, text=c(
                    "PUBLIC FACILITY: Hospital, Government, Factory, etc.",
                    "ENTERTAINMENT: Theater, Recreational Facility, Hotel, etc.",
                    "RESIDENTIAL AREA: Apartment, Condo, etc."
                  )))
                  ),
                style = "opacity: 0.92"
                  ),
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
                                                              "GRAND LARCENY OF MOTOR VEHICLE", "RAPE","ROBBERY","MURDER & NON-NEGL. MANSLAUGHTE"))
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Summary", highchartOutput("facilitymap", width = "100%", height = 650)), 
                                tabPanel("Plot")
                              )
                              #plotOutput("facilitymap", width = "100%", height = 700)
                            ))
              ),
      
      ################################################################################################                    
      tabItem(tabName = "311",
             # sidebarLayout(position = "right",
                            fluidRow(
                              column(4, selectInput("Crime.Type", label = "Crime Type", 
                                                    choices = c("BURGLARY", "FELONY ASSAULT", "GRAND LARCENY",
                                                                "GRAND LARCENY OF MOTOR VEHICLE", "RAPE", "ROBBERY",
                                                                "MURDER & NON-NEGL. MANSLAUGHTE","No Crime")))
                           
                            ),
                          #  sidebarPanel(
                           #   h4("Filter"),
                          #    checkboxGroupInput("Crime.Type", label = "Crime Type",
                                     #            choices = c("BURGLARY", "FELONY ASSAULT", "GRAND LARCENY",
                                    #                         "GRAND LARCENY OF MOTOR VEHICLE", "RAPE", "ROBBERY",
                                   #                          "MURDER & NON-NEGL. MANSLAUGHTE","All Crime","No Crime"),
                                  #               selected = c("BURGLARY"))
                             # submitButton("Update"),
                             # style = "opacity : 0.85"
                        #    ),
                        #    mainPanel(
                          #    wordcloud2Output("wordcloud", width = "100%", height = "400px")
                          #  )),
              box(width = 12,wordcloud2Output("wordcloud", width = "100%", height = "400px")),
              box(width = 12, plotlyOutput("ggplotly"))
              ),
      
      ################################################################################################                   
      tabItem(tabName = "predict",
              box(width = 12, highchartOutput("highscatter")),
              box(width = 6, plotlyOutput("crime_30_days"))
      )
      )
    )
  )


