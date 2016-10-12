#clear global environment
rm(list=ls(all=TRUE))

library(data.table)
library(dplyr)

#set work dictority
setwd("C:/Study/Columbia/W4243_Applied_Data_Science/Github/Project2/Fall2016-Proj2-grp6/data")

# Import filtered data and zip statics
load('crime_data_with_zipcode.RData')
zip_sta<-read.csv('Stats_Demographics_by_zipcode.csv')
zip_sta$zipcode<-as.character(zip_sta[,1])

#rename some column
names(crime.new)[names(crime.new)=="Occurrence Year"] <- "Occurrence_Year"


#to speed up, use the 2015 data only
crime_data<-crime.new %>% filter(Occurrence_Year>=2015)

#group by crime type, zip code
crime_data_group<-crime_data %>% group_by(Offense,zip)%>% summarise(count_num=n())
crime_data_group_total<-crime_data %>% group_by(zip)%>% summarise(count_num=n())

#merge the two table by left join
merge_data<-left_join(crime_data_group,zip_sta,by=c('zip'='zipcode'))
merge_data_total<-left_join(crime_data_group_total,zip_sta,by=c('zip'='zipcode'))

#total crime data vs income
crime_against_income_data<-merge_data_total %>% filter(Median.Household.Income<150000 & Median.Household.Income>10000)
crime_against_income_data$crime_per_person<-as.numeric(crime_against_income_data$count_num)/(crime_against_income_data$Population)



library(highcharter)
thm <- hc_theme(
  chart = list(
    backgroundColor = "black"
  ),
  yAxis = list(
    gridLineWidth = 0
  )
)

colors <- c("#FB1108","#FD150B","#FA7806","#FBE426","#FCFB8F",
            "#F3F5E7", "#C7E4EA","#ABD6E6","#9AD2E1")

crime_against_income_data$color <- colorize(log(crime_against_income_data$Median.Household.Income), colors)

x <- c("Zipcode", "Population")
y <- sprintf("{point.%s}",
             c("zip", "Population"))
tltip <- tooltip_table(x, y)


hchart(crime_against_income_data[crime_against_income_data$zip!='10018',],
       "point", x = Median.Household.Income, y = crime_per_person, size = count_num) %>% 
  hc_xAxis(title='Median Household Income ') %>% 
  hc_yAxis(title='Crime per person ') %>% 
  hc_title(text = "Crime Against Income by Zipcode") %>% 
  hc_subtitle(text = "Using 2015 crime data") %>% 
  hc_add_theme(thm) %>% 
  hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = tltip)


save(crime_against_income_data,file='crime_against_income_data.RData')
data_10018<-crime_data %>% filter(zip=='10018')
crime_10018<-data_10018 %>% group_by(Offense) %>% summarise(count_num=n())








#burlary
burlary_data<-merge_data[(merge_data$Offense=='BURGLARY')&
                          (merge_data$Median.Household.Income<150000)&(merge_data$Median.Household.Income>10000),]
plot(burlary_data$Median.Household.Income,as.numeric(burlary_data$count_num))
plot(burlary_data$Median.Household.Income,as.numeric(burlary_data$count_num)/burlary_data$Population)
#felony_assault
felony_assault_data<-merge_data[(merge_data$Offense=='FELONY ASSAULT')&
                                (merge_data$Median.Household.Income<150000)&(merge_data$Median.Household.Income>10000),]
plot(felony_assault_data$Median.Household.Income,as.numeric(felony_assault_data$count_num))
#grand_larceny
grand_larceny_data<-merge_data[(merge_data$Offense=='GRAND LARCENY')&
                                (merge_data$Median.Household.Income<150000)&(merge_data$Median.Household.Income>10000),]
plot(grand_larceny_data$Median.Household.Income,as.numeric(grand_larceny_data$count_num))
#grand_larceny_of_motor_vehicle
grand_larceny_of_motor_vehicle_data<-merge_data[(merge_data$Offense=='GRAND LARCENY OF MOTOR VEHICLE')&
                                                (merge_data$Median.Household.Income<150000)&(merge_data$Median.Household.Income>10000),]
plot(grand_larceny_of_motor_vehicle_data$Median.Household.Income,as.numeric(grand_larceny_of_motor_vehicle_data$count_num))
#MURDER & NON-NEGL. MANSLAUGHTE
murder_data<-merge_data[(merge_data$Offense=='MURDER & NON-NEGL. MANSLAUGHTE')&
                                                  (merge_data$Median.Household.Income<150000)&(merge_data$Median.Household.Income>10000),]
plot(murder_data$Median.Household.Income,as.numeric(murder_data$count_num))
#RAPE
rape_data<-merge_data[(merge_data$Offense=='RAPE')&
                        (merge_data$Median.Household.Income<150000)&(merge_data$Median.Household.Income>10000),]
plot(rape_data$Median.Household.Income,as.numeric(rape_data$count_num))
#ROBBERY
robbery_data<-merge_data[(merge_data$Offense=='ROBBERY')&
                        (merge_data$Median.Household.Income<150000)&(merge_data$Median.Household.Income>10000),]
plot(robbery_data$Median.Household.Income,as.numeric(robbery_data$count_num))



