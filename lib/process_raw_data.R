#clear global environment
rm(list=ls(all=TRUE))

#set work dictority
setwd("C:/Study/Columbia/W4243_Applied_Data_Science/Github/Project2/Fall2016-Proj2-grp6/data")

#read data
library(data.table)
raw_data=fread('NYPD_7_Major_Felony_Incidents.csv')

#remove all record where NA exists
processed_data=na.omit(raw_data)

#add two column latitude and longitude
processed_data$latitude =apply(processed_data,1,
                               function(x) as.numeric(substring(x[20],2,regexpr(',',x[20])[1]-1)))
processed_data$longitude=apply(processed_data,1,
                               function(x) as.numeric(substring(x[20],regexpr(',',x[20])[1]+1,nchar(x[20],type='char')-1)))

#add a column date_time so that the date can be compare
#notice that the date is store as num,we have to use as.Date(data_part[1]$date_time,origin = "1970-01-01") to rebuild the date
processed_data$date_time =apply(processed_data,1,
                               function(x) as.Date(substring(x[3],1,10),"%m/%d/%Y"))

#rename some column
names(processed_data)[names(processed_data)=="Occurrence Hour"] <- "Occurrence_Hour"


#output data,since GitHub has 25MB limit so split the data into 20 parts
data_length=nrow(processed_data)
for(i in 1:20)
{
  write.csv(processed_data[(data_length/20*(i-1)+1):(data_length/20*i),],file=paste('crime_data_',as.character(i),'.csv',sep=''))
}


