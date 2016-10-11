library(shiny)
library(leaflet)
library(data.table)


setwd("~/Github/")


###### Crime map datasets

crime_data<-fread('Fall2016-Proj2-grp6/data/crime_data_1.csv')
for(i in 2:20)
{
  input_data<-fread(paste('Fall2016-Proj2-grp6/data/crime_data_',
                          as.character(i),'.csv',sep=''))
  crime_data<-rbind(crime_data,input_data)
}
names(crime_data)[names(crime_data)=='latitude']<-'lat'
names(crime_data)[names(crime_data)=='longitude']<-'lng'


###### Time series analysis datasets

dat <- read.csv('Fall2016-Proj2-grp6/data/preddata.csv')
rownames(dat) <- seq.Date(as.Date("2006-01-01"), as.Date("2015-12-31"), "days")
data <- dat[,3:9]
colnames(data) <- c("GRAND LARCENY", "FELONY ASSAULT", "ROBBERY", 
                    "BURGLARY", "GRAND LARCENY OF MOTOR VEHICLE",
                    "RAPE", "MURDER")
data.ts <- cbind(data, Date = dat$Date)
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

load("Fall2016-Proj2-grp6/data/fit.RData")



###### Public Facility Allocation datasets

load("Fall2016-Proj2-grp6/data/public_count.RData")
load("Fall2016-Proj2-grp6/data/public_whole.RData")
load("Fall2016-Proj2-grp6/data/crime_count.RData")


###### 311 complaints datasets

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


###### Prediction datasets

load('Fall2016-Proj2-grp6/data/crime_against_income_data.RData')


function(input, output) {
  
  #### Map ######################################################################
  
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

  
  
  ################################################################################  
  
  
  #### Theme #####################################################################
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
  
  ################################################################################  
  
  crime <- reactiveValues(type = c("GRAND LARCENY", "FELONY ASSAULT", "ROBBERY", 
                                      "BURGLARY", "GRAND LARCENY OF MOTOR VEHICLE",
                                      "RAPE", "MURDER"))
  observeEvent(input$button2, {
    crime$type <- input$Crimetype
  })
  
  ts.ct <- reactive({
    ts.ct <- input$ct
  })
  
  output$highstock <- renderHighchart({
    filtered_preddata <- data.xts[,crime$type]
    
    plot_object <- hcbase() %>% hc_title(text = "Crime Time Series By Crime Type")
    
    if (length(crime$type)==2){
      plot_object <- plot_object %>% 
        hc_yAxis_multiples(
          list(title = list(text = crime$type[1])), 
          list(title = list(text = crime$type[2]))
        ) %>%
        hc_add_series_xts(filtered_preddata[,1], name = crime$type[1]) %>%
        hc_add_series_xts(filtered_preddata[,2], name = crime$type[2], yAxis = 1)
      
    } else {
      for(i in 1: ncol(filtered_preddata)){
        plot_object <- plot_object %>% 
          hc_add_series_xts(filtered_preddata[,i], name = crime$type[i]) 
      }
    }
    
    plot_object
  })
  

  output$highheatmap <- renderHighchart({
    dsheatmap <- lapply(dsheatmap, sapply,round)
    hcbase() %>% 
      hc_title(text = "Monthly Total Crime Number") %>%
      hc_chart(type = "heatmap") %>% 
      hc_xAxis(categories = month.abb) %>% 
      hc_yAxis(categories = seq(2006, 2015, by = 1)) %>% 
      hc_add_series(name = "Crime", data = dsheatmap) %>% 
      hc_colorAxis(stops = stops, min = 200, max = 400) 
    
  })
  
  output$forecast <- renderPlot({
    if (ts.ct() %in% "GRAND LARCENY") {
      plot(forecast(GL.fit), main = ts.ct())
    } else if (ts.ct() %in% "FELONY ASSAULT"){
      plot(forecast(FA.fit), main = ts.ct())
    } else if (ts.ct() %in% "ROBBERY"){
      plot(forecast(RO.fit), main = ts.ct())
    } else if (ts.ct() %in% "BURGLARY"){
      plot(forecast(BU.fit), main = ts.ct())
    } else if (ts.ct() %in% "GRAND LARCENY OF MOTOR VEHICLE"){
      plot(forecast(MV.fit), main = ts.ct())
    } else if (ts.ct() %in% "RAPE"){
      plot(forecast(RA.fit), main = ts.ct())
    } else if (ts.ct() %in% "MURDER"){
      plot(forecast(MU.fit), main = ts.ct())
    } 
    
  })
  
  #### Public Facility Allocation ######################################################################
  
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
    i <- which(is.na(merge_data$Offense))
    for (j in i){
        merge_data$Offense[i] <- merge_data$Offense[i-1]
    }
    merge_data <- mutate(merge_data,colour=as.character(Offense))
    merge_data <- as.data.frame(merge_data)
    #lw <- loess(cvalue ~ pvalue, merge_data())
    #fit <- cbind(merge_data$pvalue,lw$fitted)
  })
  
  output$facilitymap <- renderHighchart({
      hchart(merge_data(), "point", x = pvalue, y = cvalue, group = colour) %>% 
        hc_xAxis(title=list(text = 'Number of Public Facilities')) %>% 
        hc_yAxis(title=list(text='Number of Crimes')) %>% 
        hc_title(text = "Crime Against Public Facility distribution by Zipcode") %>% 
        #hc_subtitle(text = "Using 2015 crime data") %>% 
        hc_tooltip(useHTML = TRUE, headerFormat = "", 
                   pointFormat = tooltip_table(c("Zipcode", "Public Facility Count","Crime Count"),
                                               sprintf("{point.%s}",c("region", "pvalue",'cvalue'))))
  })
  
  ##################################################################################
  
  #### 311 Complaints #############################################################

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
  
  ##################################################################################
  
  ##### Prediction #######################################################################
  
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
  
  #########################################################################
  load('Fall2016-Proj2-grp6/data/murder_result.RData')
  load('Fall2016-Proj2-grp6/data/other_result.RData')
  murder_slices<-murder_result$crime_count
  other_slices<-other_result$crime_count
  lbls <- c("BURGLARY", "FELONY ASSAULT", "GRAND LARCENY", "GRAND LARCENY OF MOTOR VEHICLE",
            "MURDER & NON-NEGL. MANSLAUGHTE",'ROBBERY',"RAPE")
  murder_pct <- round(murder_slices/sum(murder_slices),2)
  other_pct <- round(other_slices/sum(other_slices),2)
  crime_data_30days <- data.frame(lbls, murder_pct, other_pct)
  
  output$crime_30_days <- renderPlotly({
    plot_ly(crime_data_30days, x =lbls, y =murder_pct, type = 'bar', name = 'Murder Pct') %>%
      add_trace(x=lbls,y = other_pct, type='bar',name = 'Other Pct') %>%
      layout(title='30 days accumulated Crime Compare',
             yaxis = list(title = 'Percent'), barmode = 'group')
  })
  
  ########################################################################
  
}
