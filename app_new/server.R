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

f <- exp

dshmstops <- data.frame(q = c(0, f(1:5)/f(5)), c = substring(viridis(5 + 1), 0, 7)) %>% 
  list.parse2()

####### Weichuan's part
library(shiny)
library(leaflet)
library(data.table)
library(dplyr)
#library(plotly)


setwd("/Users/yueqizhang/Documents/w5243 ads/project2")
crime_data<-fread('Fall2016-Proj2-grp6/data/crime_data_1.csv')
for(i in 2:20)
{
  input_data<-fread(paste('Fall2016-Proj2-grp6/data/crime_data_',
                          as.character(i),'.csv',sep=''))
  crime_data<-rbind(crime_data,input_data)
}
names(crime_data)[names(crime_data)=='latitude']<-'lat'
names(crime_data)[names(crime_data)=='longitude']<-'lng'

####### Minghao's part

data <- read.csv('Fall2016-Proj2-grp6/data/preddata.csv')
rownames(data) <- seq.Date(as.Date("2006-01-01"), as.Date("2015-12-31"), "days")
data <- data[,3:9]
colnames(data) <- c("GRAND LARCENY", "FELONY ASSAULT", "ROBBERY", 
                    "BURGLARY", "GRAND LARCENY OF MOTOR VEHICLE",
                    "RAPE", "MURDER")
data.xts <- as.xts(data)
data.mon <- apply.monthly(data.xts, mean)
data.mon.sum <- apply(data.mon, 1, sum)

dsheatmap <- tbl_df(expand.grid(seq(12) - 1, seq(10) - 1)) %>% 
  mutate(value = data.mon.sum) %>% 
  list_parse2()

stops <- data.frame(q = 0:4/4,
                    c = rev(substring(heat.colors(4 + 1), 0, 7)),
                    stringsAsFactors = FALSE)
stops <- list_parse2(stops)



###### Jiwen's part

load("Fall2016-Proj2-grp6/data/public_count.RData")
load("Fall2016-Proj2-grp6/data/public_whole.RData")
load("Fall2016-Proj2-grp6/data/crime_count.RData")


###### Yueqi's part
normal<-read.csv("Fall2016-Proj2-grp6/data/type of 311 normal.csv")
crime<-read.csv("Fall2016-Proj2-grp6/data/type of 311 with crime.csv")
crime.murder<-read.csv("Fall2016-Proj2-grp6/data/type of 311 with crime murder.csv")
crime.burglary<-read.csv("Fall2016-Proj2-grp6/data/type of 311 with crime burglary.csv")
crime.felony<-read.csv("Fall2016-Proj2-grp6/data/type of 311 with crime FELONY ASSAULT.csv")
crime.glmv<-read.csv("Fall2016-Proj2-grp6/data/type of 311 with crime GRAND LARCENY OF MOTOR VEHICLE.csv")
crime.gl<-read.csv("Fall2016-Proj2-grp6/data/type of 311 with crime GRAND LARCENY.csv")
crime.rape<-read.csv("Fall2016-Proj2-grp6/data/type of 311 with crime RAPE.csv")
crime.robbery<-read.csv("Fall2016-Proj2-grp6/data/type of 311 with crime ROBBERY.csv")
barplotdata<-read.csv("Fall2016-Proj2-grp6/data/barplotdata.csv",stringsAsFactors = FALSE)


function(input, output) {
  
  #### Page 1
  
  #read and update the input data
  start_date<-eventReactive(input$button, {
    start_date<-input$Date_Range[1]
  })
  
  end_date<-eventReactive(input$button, {
    input$button
    end_date<-input$Date_Range[2]
  })
  
  crime_type<-eventReactive(input$button, {
    input$button
    crime_type<-input$Crime_Type
  })
  
  start_hour<-eventReactive(input$button, {
    start_hour<-input$IntHour
  })
  
  end_hour<-eventReactive(input$button, {
    end_hour<-input$EndHour
  })
  
  # subsets the crime data depending on user input in the Shiny app
  filtered_crime_data <- eventReactive(input$button, {
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
      addCircles(lng=~lng, lat=~lat, radius=40, 
                 stroke=FALSE, fillOpacity=0.4,color=~pal(Offense),
                 popup=~as.character(paste("Crime Type: ",Offense,
                                           "Precinct: ",  Precinct 
                 ))) %>%
      addLegend("bottomleft", pal = pal, values = ~Offense,
                title = "Crime Type",
                opacity = 1 )%>% 
      addMarkers(
        clusterOptions = markerClusterOptions())
  })

  
  
  
  
  
  #### Page 2
  hcbase <- reactive({
    
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
  
  crime <- reactiveValues(type = c("GRAND LARCENY", "FELONY ASSAULT", "ROBBERY", 
                                      "BURGLARY", "GRAND LARCENY OF MOTOR VEHICLE",
                                      "RAPE", "MURDER"))
  observeEvent(input$button2, {
    crime$type <- input$Crimetype
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
    filtered_preddata <- data.xts[,crime$type]
    
    plot_object <- hcbase() %>% hc_title(text = "Crime Time Series By Crime Type")
    
    for(i in 1: ncol(filtered_preddata)){
      plot_object <- plot_object %>% 
        hc_add_series_xts(filtered_preddata[,i], name = crime$type[i]) 
    }
    plot_object
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
      hc_title(text = "Monthly Total Crime Number") %>%
      hc_chart(type = "heatmap") %>% 
      hc_xAxis(categories = month.abb) %>% 
      hc_yAxis(categories = seq(2006, 2015, by = 1)) %>% 
      hc_add_series(name = "Crime", data = dsheatmap) %>% 
      hc_colorAxis(stops = stops, min = 200, max = 400) 
    
  })
  
  
  
  ####### Page 3
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
    filtered_p_crime_data <- crime_count %>% 
      filter(Offense %in% ctype()) %>%
      rename(cvalue=value)
  })
  
  merge_data <- reactive({
    merge_data <- full_join(filtered_facility_data(),filtered_p_crime_data(),by="region") %>% 
      filter(NEW_CATEGORY != "")
    merge_data$cvalue <- ifelse(is.na(merge_data$Offense),0,merge_data$cvalue)
    merge_data$Offense <- ifelse(is.na(merge_data$Offense),merge_data$Offense[which(is.na(merge_data$Offense))-1],merge_data$Offense)
    merge_data <- mutate(merge_data,colour=as.character(Offense))
    merge_data <- as.data.frame(merge_data)
    #lw <- loess(cvalue ~ pvalue, merge_data())
    #fit <- cbind(merge_data$pvalue,lw$fitted)
  })
  
  #output$facilitymap <- renderPl({
    #ggplot(merge_data(),aes(pvalue,cvalue))+geom_point()+geom_smooth()+
      #labs(x="Number of Public Facilities",y="Number of Crimes")
    
  #})
  
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
  
  
  output$facilitymap <- renderHighchart({
    
    if(input$theme != FALSE)
    {
      hchart(merge_data(), "point", x = pvalue, y = cvalue, group = colour) %>% 
        hc_xAxis(title=list(text = 'Number of Public Facilities')) %>% 
        hc_yAxis(title=list(text='Number of Crimes')) %>% 
        hc_title(text = "Crime Against Public Facility distribution by Zipcode") %>% 
        #hc_subtitle(text = "Using 2015 crime data") %>% 
        hc_add_theme(thm()) %>% 
        hc_tooltip(useHTML = TRUE, headerFormat = "", 
                   pointFormat = tooltip_table(c("Zipcode", "Public Facility Count","Crime Count"),
                                               sprintf("{point.%s}",c("region", "pvalue",'cvalue'))))
    }
    else
    {
      hchart(merge_data(), "point", x = pvalue, y = cvalue, group = colour) %>% 
        hc_xAxis(title=list(text = 'Number of Public Facilities')) %>% 
        hc_yAxis(title=list(text='Number of Crimes')) %>% 
        hc_title(text = "Crime Against Public Facility distribution by Zipcode") %>% 
        #hc_subtitle(text = "Using 2015 crime data") %>% 
        #hc_add_theme(thm()) %>% 
        hc_tooltip(useHTML = TRUE, headerFormat = "", 
                   pointFormat = tooltip_table(c("Zipcode", "Public Facility Count","Crime Count"),
                                               sprintf("{point.%s}",c("region", "pvalue",'cvalue'))))
    }
  })
  
  
  # Page 311
  crime.type<-reactive({
    crime.type<-input$Crime.Type
  })
  
  wc<-reactive({
    if(crime.type()=='BURGLARY'){
      wc<-crime.burglary
      wc<-t(wc)
      colnames(wc)<-c('word','freq')
      #wc$word<-as.character(wc$word)
      #wc[,2]<-as.numeric(levels(wc[,2]))[wc[,2]]
      #wc<-data.frame(wc)
    }
    
    if (crime.type()=='FELONY ASSAULT')
    {
      wc<-crime.felony
      wc<-t(wc)
      colnames(wc)<-c('word','freq')
      #wc[,1]<-as.character(wc[,1])
      #wc[,2]<-as.numeric(as.character(wc[,2]))
      #wc<-data.frame(wc)
    }
    
    if (crime.type()=='GRAND LARCENY')
    {
      wc<-crime.gl
      wc<-t(wc)
      colnames(wc)<-c('word','freq')
      #wc[,1]<-as.character(wc[,1])
      #wc[,2]<-as.numeric(as.character(wc[,2]))
      #wc<-data.frame(wc)
    }
    
    
    if (crime.type()=='GRAND LARCENY OF MOTOR VEHICLE')
    {
      wc<-crime.glmv
      wc<-t(wc)
      colnames(wc)<-c('word','freq')
      #wc[,1]<-as.character(wc[,1])
      #wc[,2]<-as.numeric(as.character(wc[,2]))
      #wc<-data.frame(wc)
    }
    
    if (crime.type()=='RAPE')
    {
      wc<-crime.rape
      wc<-t(wc)
      colnames(wc)<-c('word','freq')
      #wc[,1]<-as.character(wc[,1])
      #wc[,2]<-as.numeric(as.character(wc[,2]))
      #wc<-data.frame(wc)
    }
    
    if (crime.type()=='ROBBERY')
    {
      wc<-crime.robbery
      wc<-t(wc)
      colnames(wc)<-c('word','freq')
      #wc[,1]<-as.character(wc[,1])
      #wc[,2]<-as.numeric(as.character(wc[,2]))
      #wc<-data.frame(wc)
    }
    
    if (crime.type()=='MURDER & NON-NEGL. MANSLAUGHTE')
    {
      wc<-crime.murder
      wc<-t(wc)
      colnames(wc)<-c('word','freq')
      #wc[,1]<-as.character(wc[,1])
      #wc[,2]<-as.numeric(as.character(wc[,2]))
      #wc<-data.frame(wc)
    }
    
   # if (crime.type()=='All Crime')
   # {
   #   wc<-crime
    #  wc<-t(wc)
     # colnames(wc)<-c('word','freq')
      #wc[,1]<-as.character(wc[,1])
      #wc[,2]<-as.numeric(as.character(wc[,2]))
      #wc<-data.frame(wc)
   # }
    
    if (crime.type()=='No Crime')
    {
      wc<-normal
      wc<-t(wc)
      colnames(wc)<-c('word','freq')
      #wc[,1]<-as.character(wc[,1])
      #wc[,2]<-as.numeric(as.character(wc[,2]))
      #wc<-data.frame(wc)
    }
    wc
  })
  
  output$wordcloud <- renderWordcloud2({
    data_wordcloud<-data.frame(wc())
    data_wordcloud$word<-as.character(data_wordcloud$word)
    data_wordcloud$freq<-as.numeric(levels(data_wordcloud$freq))[data_wordcloud$freq]
    wordcloud2(data_wordcloud,size = 1,shape = 'circle')
  })
  
  output$ggplotly<-renderPlotly({
    g<-ggplot(barplotdata,aes(x=Type,fill=Crime))+geom_bar(position="dodge")+xlab(" ")+ylab("Complaint Number")+theme(axis.text.x=element_text(vjust = 1, hjust = 0.5,angle = 45))
    ggplotly(g)
  })
}
