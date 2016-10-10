usdjpy <- getSymbols("USD/JPY", src = "oanda", auto.assign = FALSE)
eurkpw <- getSymbols("EUR/KPW", src = "oanda", auto.assign = FALSE)

data(citytemp, package = "highcharter")
data(worldgeojson, package = "highcharter")
data(sample_matrix, package = "xts")
#data(GNI2010, package = "treemap")
data(diamonds, package = "ggplot2")

dscounts <- dplyr::count(diamonds, cut) %>% 
  setNames(c("name", "value")) %>% 
  list.parse3()

dsheatmap <- tbl_df(expand.grid(seq(12) - 1, seq(5) - 1)) %>% 
  mutate(value = abs(seq(nrow(.)) + 10 * rnorm(nrow(.))) + 10,
         value = round(value, 2)) %>% 
  list.parse2()

f <- exp

dshmstops <- data.frame(q = c(0, f(1:5)/f(5)), c = substring(viridis(5 + 1), 0, 7)) %>% 
  list.parse2()

####### Weichuan's part
library(shiny)
library(leaflet)
library(data.table)
library(dplyr)
library(plotly)


setwd("C:/Study/Columbia/W4243_Applied_Data_Science/Project2/")
crime_data<-fread('Fall2016-Proj2-grp6/data/crime_data_1.csv')
for(i in 2:20)
{
  input_data<-fread(paste('Fall2016-Proj2-grp6/data/crime_data_',
                          as.character(i),'.csv',sep=''))
  crime_data<-rbind(crime_data,input_data)
}


####### Minghao's part

data <- read.csv('Fall2016-Proj2-grp6/data/preddata.csv')
rownames(data) <- as.Date(data$Date)
data <- data[,3:9]
colnames(data) <- c("GRAND LARCENY", "FELONY ASSAULT", "ROBBERY", 
                    "BURGLARY", "GRAND LARCENY OF MOTOR VEHICLE",
                    "RAPE", "MURDER")
data.xts <- as.xts(data)


###### Jiwen's part

load("Fall2016-Proj2-grp6/data/public_count.RData")
load("Fall2016-Proj2-grp6/data/public_whole.RData")
load("Fall2016-Proj2-grp6/data/crime_count.RData")


function(input, output) {
  
  #### Page 1
  
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
  col=c('darkred','yellow','cyan','deepskyblue','lightgreen','red','purple')
  
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
      addLegend("bottomleft", pal = pal, values = ~Offense,
                title = "Crime Type",
                opacity = 1
      )%>% addMarkers(
        clusterOptions = markerClusterOptions())
  })

  
  
  
  
  
  #### Page 2
  hcbase <- reactive({
    # hcbase <- function() highchart() 
    hc <- highchart() 
    if (input$credits)
      hc <- hc %>% hc_credits(enabled = TRUE, text = "Highcharter", href = "http://jkunst.com/highcharter/")
    
    if (input$exporting)
      hc <- hc %>% hc_exporting(enabled = TRUE)
    if (input$theme != FALSE) {
      theme <- switch(input$theme,
                      null = hc_theme_null(),
                      economist = hc_theme_economist(),
                      dotabuff = hc_theme_db(),
                      darkunica = hc_theme_darkunica(),
                      gridlight = hc_theme_gridlight(),
                      sandsignika = hc_theme_sandsignika(),
                      fivethirtyeight = hc_theme_538(),
                      chalk = hc_theme_chalk(),
                      handdrwran = hc_theme_handdrawn()
      )
      
      hc <- hc %>% hc_add_theme(theme)
    }
    
    hc
    
  })
  
  crimetype<-reactive({
    crimetype<-input$Crimetype
  })
  
  output$highchart <- renderHighchart({
    
    hcbase() %>% 
      hc_title(text = "Monthly Average Temperature") %>% 
      hc_subtitle(text = "Source: WorldClimate.com") %>% 
      hc_yAxis(title = list(text = "Temperature")) %>% 
      hc_xAxis(categories = citytemp$month) %>% 
      hc_add_series(name = "Tokyo", data = citytemp$tokyo) %>% 
      hc_add_series(name = "London", data = citytemp$london) %>% 
      hc_add_series(name = "Berlin", data = citytemp$berlin) 
    
  })
  
  output$highstock <- renderHighchart({
    filtered_preddata <- data.xts[,crimetype()]
    
    plot_object <- hcbase() 
    
    for(i in 1: ncol(filtered_preddata)){
      plot_object <- plot_object %>% 
        hc_add_series_xts(filtered_preddata[,i], name = crimetype()[i]) 
    }
    plot_object
  })
  
  output$highmap <- renderHighchart({
    
    hcbase() %>% 
      hc_add_series_map(worldgeojson, GNI2010, value = "GNI", joinBy = "iso3") %>% 
      hc_colorAxis(stops = dshmstops) 
    
  })
  
  
  ####
  load('Fall2016-Proj2-grp6/data/crime_against_income_data.RData')
  thm<-reactive({
    if (input$theme != FALSE) {
      thm <- switch(input$theme,
                    null = hc_theme_null(),
                    economist = hc_theme_economist(),
                    dotabuff = hc_theme_db(),
                    darkunica = hc_theme_darkunica(),
                    gridlight = hc_theme_gridlight(),
                    sandsignika = hc_theme_sandsignika(),
                    fivethirtyeight = hc_theme_538(),
                    chalk = hc_theme_chalk(),
                    handdrwran = hc_theme_handdrawn()
      )}
  })
  
  output$highscatter <- renderHighchart({
    
    if(input$theme != FALSE)
    {
      hchart(crime_against_income_data, "point", x = Median.Household.Income, y = crime_per_person, size = count_num) %>% 
        hc_xAxis(title=list(text = 'Median Household Income')) %>% 
        hc_yAxis(title=list(text='Crime per person')) %>% 
        hc_title(text = "Crime Against Income by Zipcode") %>% 
        hc_subtitle(text = "Using 2015 crime data") %>% 
        hc_add_theme(thm()) %>% 
        hc_tooltip(useHTML = TRUE, headerFormat = "", 
                   pointFormat = tooltip_table(c("Zipcode", "Population","Crime Count"),
                                               sprintf("{point.%s}",c("zip", "Population",'count_num'))))
    }
    else
    {
      hchart(crime_against_income_data, "point", x = Median.Household.Income, y = crime_per_person, size = count_num) %>% 
        hc_xAxis(title=list(text = 'Median Household Income')) %>% 
        hc_yAxis(title=list(text='Crime per person')) %>% 
        hc_title(text = "Crime Against Income by Zipcode") %>% 
        hc_subtitle(text = "Using 2015 crime data") %>% 
        hc_tooltip(useHTML = TRUE, headerFormat = "", 
                   pointFormat = tooltip_table(c("Zipcode", "Population","Crime Count"),
                                               sprintf("{point.%s}",c("zip", "Population",'count_num'))))      
    }
  })
  ###
  
  output$highstreemap <- renderHighchart({
    
    hcbase() %>% 
      hc_add_series(data = dscounts, type = "treemap", colorByPoint = TRUE) 
    
  })
  
  output$highohlc <- renderHighchart({
    
    hcbase() %>% 
      hc_add_series_ohlc(as.xts(sample_matrix))
    
  })

  output$highheatmap <- renderHighchart({
    
    hcbase() %>% 
      hc_chart(type = "heatmap") %>% 
      hc_xAxis(categories = month.abb) %>% 
      hc_yAxis(categories = 2016 - length(dsheatmap)/12 + seq(length(dsheatmap)/12)) %>% 
      hc_add_series(name = "value", data = dsheatmap) %>% 
      hc_colorAxis(min = 0) 
    
  })
  
  ts <- reactive({
    
    get(input$ts)
    
  })
  
  output$tschart <- renderHighchart({hchart(ts())})
  
  output$tsacf <- renderHighchart({hchart(acf(ts(), plot = FALSE))})
  
  output$tspacf <- renderHighchart({hchart(pacf(ts(), plot = FALSE))})
  
  output$tsforecast <- renderHighchart({
    
    ts <- ts()
    # highcharter:::hchart.forecast
    object <- forecast(ts, level = 95)
    tmf <- datetime_to_timestamp(zoo::as.Date(time(object$mean)))
    nmf <- paste("level", object$level)
    
    dsf <- data_frame(tmf, object$mean) %>% 
      list.parse2()
    
    highchart() %>% 
      hc_add_series_ts(object$x, name = input$ts) %>% 
      hc_add_series(data = dsf, name = "AutoArima Forecast",
                    marker = list(enabled = FALSE),
                    enableMouseTracking = FALSE) %>% 
      hc_add_series(data = dsf, name = "Your Forecast",
                    cursor = "ns-resize", draggableY = TRUE) %>% 
      hc_plotOptions(
        series = list(
          point = list(
            events = list(
              drop = JS("function(){
                        console.log(this.series)
                        window.data = _.map(this.series.data, function(e) { return e.y })
                        Shiny.onInputChange('manualforecast', data);
                        }"))
              )))
    
  })
  
  output$dfforecast <- renderDataTable({
    
    ts <- ts()
    mf <- input$manualforecast #listening the drop event defined in output$tsforecast
    fc <- forecast(ts)$mean
    
    # if you change timeseries input$manualforecast dont change
    # so we update it
    if (is.null(mf) || length(mf) != length(fc))  
      mf <- fc
    
    data_frame(
      datetime = as.Date(time(forecast(ts)$mean)),
      forecast = fc,
      manualforecast = mf,
      diff = round((mf - fc)/fc, 2)
    )
    
  })
  
  output$pluginsfa <- renderHighchart({
    title <- tags$div(icon("quote-left"), "This is a h1 title with a awesome icon", icon("bar-chart"))
    title <- as.character(title)
    
    subtitle <- tags$div("This can be", icon("thumbs-o-up"), "wait for it... awesome")
    subtitle <- as.character(subtitle)
    
    # https://github.com/FortAwesome/Font-Awesome/blob/master/less/variables.less
    
    highchart() %>%
      hc_title(text = title, useHTML = TRUE) %>% 
      hc_subtitle(text = subtitle, useHTML = TRUE) %>% 
      hc_tooltip(
        useHTML = TRUE,
        pointFormat = '<span style="color:{series.color};">{series.options.icon}</span> {series.name}: <b>[{point.x}, {point.y}]</b><br/>'
      ) %>% 
      hc_add_series_scatter(mtcars$mpg[1:16], mtcars$disp[1:16],
                            marker = list(symbol = "text:\\\uf1b9"),
                            icon = as.character(shiny::icon("car")),
                            name = "cars", showInLegend = TRUE) %>% 
      hc_add_series_scatter(mtcars$mpg[17:32], mtcars$disp[17:32],
                            marker = list(symbol = "text:\\\uf1ba"),
                            icon = as.character(shiny::icon("taxi")),
                            name = "cabs", showInLegend = TRUE)  %>% 
      hc_add_theme(hc_theme_google()) %>% 
      hc_chart(zoomType = "xy")
    
  })
  
  # ptype means the public facility type
  ptype<-reactive({
    ptype<-input$Facility_Category
  })
  
  # ctype means the crime type
  ctype<-reactive({
    ctype<-input$p_Crime_Type
  })
  
  # subsets the facility data and crime type depending on user input in the Shiny app
  filtered_facility_data <- reactive({
    #filter by facility category
    filtered_facility_data <- public_count %>% 
      filter(NEW_CATEGORY %in% ptype()) %>%
      rename(pvalue=value)
  })
  filtered_p_crime_data <- reactive({
    #filter by crime type
    filtered_facility_data <- crime_count %>% 
      filter(Offense %in% ctype()) %>%
      rename(cvalue=value)
  })
  
  merge_data <- reactive({
    merge_data <- full_join(filtered_facility_data(),filtered_p_crime_data(),by="region") %>% 
      filter(NEW_CATEGORY != "")
    merge_data$cvalue <- ifelse(is.na(merge_data$Offense),0,merge_data$cvalue)
    merge_data <- as.data.frame(merge_data)
  })
  
  output$facilitymap <- renderPlotly({
    ggplot(merge_data(),aes(pvalue,cvalue))+geom_point()+geom_smooth()+
      labs(x="Number of Public Facilities",y="Number of Crimes")
  })
  
}
