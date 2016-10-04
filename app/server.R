#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(data.table)
library(dplyr)


# Import filtered data
crime_data<-fread('C:/Study/Columbia/W4243_Applied_Data_Science/Github/Project2/Fall2016-Proj2-grp6/data/crime_data.csv')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #read and update the input data
  start_date<-reactive({
    start_date<-input$Date_Range[1]
  })
  
  end_date<-reactive({
    end_date<-input$Date_Range[2]
  })
  
  crime_type<-reactive({
    crime_type<-input$Crime_Type
  })
  
  start_hour<-reactive({
    start_hour<-input$IntHour
  })
  
  end_hour<-reactive({
    end_hour<-input$EndHour
  })
  
  # subsets the crime data depending on user input in the Shiny app
  filtered_crime_data <- reactive({
    #filter by crime type,date range,hour
    filtered_crime_data<-crime_data %>% 
      filter(as.Date(crime_data$date_time,origin = "1970-01-01") >= start_date() & 
               as.Date(crime_data$date_time,origin = "1970-01-01") <= end_date())       %>%
      filter(Offense %in% crime_type()) %>%
      filter(Occurrence_Hour >= start_hour() & 
               Occurrence_Hour <= end_hour())
  })
  
  #set color
  col=c('darkred','yellow','red','deepskyblue','lightgreen','purple')
  
  #legend
  var=c( "BURGLARY", "FELONY ASSAULT", "GRAND LARCENY",
         "GRAND LARCENY OF MOTOR VEHICLE", "RAPE", "ROBBERY")
  
  #color palette
  pal <- colorFactor(col, domain = var)
  
  #out map
  output$map <- renderLeaflet({
    leaflet(data = filtered_crime_data()) %>% 
      addProviderTiles('Stamen.TonerLite') %>% 
      setView(lng = -73.971035, lat = 40.775659, zoom = 12) %>% 
      addCircles(~longitude, ~latitude, radius=40, 
                 stroke=FALSE, fillOpacity=0.4,color=~pal(Offense),
                 popup=~as.character(paste("Crime Type: ",Offense,
                                           "Precinct: ",  Precinct 
                 ))) %>%
      addLegend("topleft", pal = pal, values = ~Offense,
                title = "Crime Type",
                opacity = 1
      )
  })
  
  
})