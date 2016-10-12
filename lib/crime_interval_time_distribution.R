#This script is used to test if there is some relationship between crimes

#clear global environment
rm(list=ls(all=TRUE))
library(data.table)
library(dplyr)
# Import filtered data
crime_data<-fread('C:/Study/Columbia/W4243_Applied_Data_Science/Github/Project2/Fall2016-Proj2-grp6/data/crime_data_1.csv')
for(i in 2:20)
{
  input_data<-fread(paste('C:/Study/Columbia/W4243_Applied_Data_Science/Github/Project2/Fall2016-Proj2-grp6/data/crime_data_',
                          as.character(i),'.csv',sep=''))
  crime_data<-rbind(crime_data,input_data)
}

#rename some column
names(crime_data)[names(crime_data)=="Occurrence Year"] <- "Occurrence_Year"


#to speed up, use the 2015 data only
crime_data_part<-crime_data %>% filter(Occurrence_Year>=2014)

#create an absoulte_hour variable used for calculating hour difference
crime_data_part$absolute_hour<-apply(crime_data_part,1,
                                     function(x) as.numeric(x[9])+24*as.numeric(x[24]))


############################################################################## taks all felony as the same
#group by absolute hour and precinct
crime_data_part_group<-crime_data_part %>% group_by(absolute_hour,Precinct) %>% summarise(count_num=n())

#Get the unique precinct
Precinct<-unique(crime_data_part_group$Precinct)
# For loop to get the p value for every district
hour_vector_total<-c()
result<-matrix(data=NA,nrow=length(Precinct),ncol=4)
library(MASS)
library(vcd)
for(precinct_num in 1:length(Precinct))
{
  #get the data in a specific precinct
  data_by_precinct<-crime_data_part_group[crime_data_part_group$Precinct==Precinct[precinct_num],]
  
  #order by time , ascending
  data_by_precinct<-data_by_precinct[order(data_by_precinct$absolute_hour),]
  
  #get the length
  row_length<-nrow(data_by_precinct)
  
  #get the frequency number
  data_by_precinct_frequency_number<-data_by_precinct$count_num[2:row_length]
  
  #calculate the time interval by hour
  data_by_precinct_interval<-data_by_precinct$absolute_hour[2:row_length]-data_by_precinct$absolute_hour[1:row_length-1]
  
  #combine the time interval and frequency number to get the full data
  hour_vector<-rep(data_by_precinct_interval[1],data_by_precinct_frequency_number[1])
  for(i in 2:length(data_by_precinct_frequency_number))
  {
    hour_vector_temp<-rep(data_by_precinct_interval[i],data_by_precinct_frequency_number[i])
    hour_vector<-c(hour_vector,hour_vector_temp)
  }
  
  #test if it;s exponential distribution
  # estimate the parameters
  parameters <- fitdistr(hour_vector, "exponential") 
  # goodness of fit test
  output<-ks.test(hour_vector, "pexp", parameters$estimate) # p-value > 0.05 -> distribution not refused
  result[precinct_num,1]<-Precinct[precinct_num]
  result[precinct_num,2]<-output$p.value
  result[precinct_num,3]<-output$statistic
  result[precinct_num,4]<-length(hour_vector)
  hour_vector_total<-c(hour_vector_total,hour_vector)
}

#test if it;s exponential distribution
# estimate the parameters
parameters <- fitdistr(hour_vector_total, "exponential") 
# goodness of fit test
output<-ks.test(hour_vector_total, "pexp", parameters$estimate) # p-value > 0.05 -> distribution not refused
hist(hour_vector_total,breaks=100)
# plot a graph
hist(hour_vector_total, freq = FALSE, breaks = 1000, col='green',
     xlim = c(0, quantile(hour_vector_total, 0.995)),xlab='Crime Interval in hour',
     main='Distribution of crime interval')
curve(dexp(x, rate = parameters$estimate), col = "red", add = TRUE)
legend(30,0.1,'exponential with rate 0.14',col='red',pch='l')

save(hour_vector_total,file='hour_vector_total.RData')
library(highcharter)
hchart(density(hour_vector_total))

library(plotly)
plot_ly(x = ~rnorm(50), type = "histogram")

library(plotly)
plot_ly(x = hour_vector_total,type = "histogram") %>%
  layout(
    xaxis = list(range = c(0, 50)))

library(plotly)

df <- data.frame(x = hour_vector_total)

ggplot(df, aes(x=x)) +
  geom_histogram(aes(y = ..density..), binwidth=density(df$x)$bw) +
  geom_density(fill="red", alpha = 0.2) %>% xlim(list(range = c(0, 50)))

ggplotly()

library(graphics)
plot(ecdf(hour_vector_total),xlim=c(0, quantile(hour_vector_total, 0.99)),col='red',vertical=TRUE)
lines(ecdf(rexp(10000,0.14)))

############################################################################## 


############################################################################## 
#taks all felony as different
#group by absolute hour , precinct and offense
crime_data_part_group<-crime_data_part %>% group_by(absolute_hour,Precinct,Offense) %>% summarise(count_num=n())

#Get the unique precinct
Precinct<-unique(crime_data_part_group$Precinct)

# For loop to get the p value for every district
hour_vector_total<-c()
result<-matrix(data=NA,nrow=length(Precinct),ncol=4)
library(MASS)
library(vcd)
for(precinct_num in 1:length(Precinct))
{
  #get the data in a specific precinct and a specific crime type
  data_by_precinct<-crime_data_part_group[(crime_data_part_group$Precinct==Precinct[precinct_num]
                                           &crime_data_part_group$Offense=='GRAND LARCENY'),]
  
  #order by time , ascending
  data_by_precinct<-data_by_precinct[order(data_by_precinct$absolute_hour),]
  
  #get the length
  row_length<-nrow(data_by_precinct)
  
  #get the frequency number
  data_by_precinct_frequency_number<-data_by_precinct$count_num[2:row_length]
  
  #calculate the time interval by hour
  data_by_precinct_interval<-data_by_precinct$absolute_hour[2:row_length]-data_by_precinct$absolute_hour[1:row_length-1]
  
  #combine the time interval and frequency number to get the full data
  hour_vector<-rep(data_by_precinct_interval[1],data_by_precinct_frequency_number[1])
  for(i in 2:length(data_by_precinct_frequency_number))
  {
    hour_vector_temp<-rep(data_by_precinct_interval[i],data_by_precinct_frequency_number[i])
    hour_vector<-c(hour_vector,hour_vector_temp)
  }
  
  #test if it;s exponential distribution
  # estimate the parameters
  parameters <- fitdistr(hour_vector, "exponential") 
  # goodness of fit test
  output<-ks.test(hour_vector, "pexp", parameters$estimate) # p-value > 0.05 -> distribution not refused
  result[precinct_num,1]<-Precinct[precinct_num]
  result[precinct_num,2]<-output$p.value
  result[precinct_num,3]<-output$statistic
  result[precinct_num,4]<-length(hour_vector)
  hour_vector_total<-c(hour_vector_total,hour_vector)
}



##############################################################################################
# Import filtered data and zip statics
library(dplyr)
setwd('C:/Study/Columbia/W4243_Applied_Data_Science/Github/Project2/Fall2016-Proj2-grp6/data')
load('crime_data_with_zipcode.RData')

##only check murder data and the crime before them
murder_data<-crime.new[crime.new$Offense=='MURDER & NON-NEGL. MANSLAUGHTE',]
murder_data_part<-murder_data[sample(1:nrow(murder_data),100),]
burglary_count<-0
felony_count<-0 
grand_larceny_count<-0           
grand_larceny_motor_count<-0
murder_count<-0 
rape_count<-0      
robbery_count<-0              
for(i in 1:nrow(murder_data))
{
  temp_precinct<-murder_data[i,]$Precinct
  temp_date_time<-murder_data[i,]$date_time
  filtered_data<-crime.new %>% filter((Precinct ==temp_precinct)    &
                                      (date_time < temp_date_time)   &
                                      (date_time > (temp_date_time-30))
                                      )
  temp_result<-filtered_data %>% group_by(Offense) %>% summarise(count_num=n())
  if(nrow(temp_result)==0)
  {next}
  for(j in 1:nrow(temp_result))
  {
    if(temp_result$Offense[j]=="BURGLARY")
    {burglary_count<-burglary_count+temp_result$count_num[j]}
    else if(temp_result$Offense[j]=="FELONY ASSAULT")
    {felony_count<-felony_count+temp_result$count_num[j]}
    else if(temp_result$Offense[j]=="GRAND LARCENY" )
    {grand_larceny_count<-grand_larceny_count+temp_result$count_num[j]}
    else if(temp_result$Offense[j]=="GRAND LARCENY OF MOTOR VEHICLE")
    {grand_larceny_motor_count<-grand_larceny_motor_count+temp_result$count_num[j]}
    else if(temp_result$Offense[j]=="MURDER & NON-NEGL. MANSLAUGHTE" )
    {murder_count<-murder_count+temp_result$count_num[j]}
    else if(temp_result$Offense[j]=="RAPE")
    {rape_count<-rape_count+temp_result$count_num[j]}
    else
    {robbery_count<-robbery_count+temp_result$count_num[j]}
  }
  print(paste(as.character(i/nrow(murder_data)*100),'%',' completed',sep=''))
}

#slices <- c(burglary_count, felony_count,grand_larceny_count, 
#           grand_larceny_motor_count, murder_count,robbery_count,rape_count)
slices<-murder_result$crime_count
lbls <- c("BURGLARY", "FELONY ASSAULT", "GRAND LARCENY", "GRAND LARCENY OF MOTOR VEHICLE",
          "MURDER & NON-NEGL. MANSLAUGHTE",'ROBBERY',"RAPE")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Crime")


murder_result<-data.frame( c("BURGLARY", "FELONY ASSAULT", "GRAND LARCENY", "GRAND LARCENY OF MOTOR VEHICLE",
                             "MURDER & NON-NEGL. MANSLAUGHTE",'ROBBERY',"RAPE"),
                           slices)
colnames(murder_result)<-c('crime_type','crime_count')
save(murder_result,file='murder_result.RData')

load('murder_result.RData')

##################################################################
#check other data
other_data<-crime.new[crime.new$Offense!='MURDER & NON-NEGL. MANSLAUGHTE',]
other_data_part<-other_data[sample(1:nrow(other_data),5000),]
burglary_count_other<-0
felony_count_other<-0 
grand_larceny_count_other<-0           
grand_larceny_motor_count_other<-0
murder_count_other<-0 
rape_count_other<-0      
robbery_count_other<-0              
for(i in 1:nrow(other_data_part))
{
  temp_precinct<-other_data_part[i,]$Precinct
  temp_date_time<-other_data_part[i,]$date_time
  filtered_data<-crime.new %>% filter((Precinct ==temp_precinct)    &
                                        (date_time < temp_date_time)   &
                                        (date_time > (temp_date_time-30))
  )
  temp_result<-filtered_data %>% group_by(Offense) %>% summarise(count_num=n())
  if(nrow(temp_result)==0)
  {next}
  for(j in 1:nrow(temp_result))
  {
    if(temp_result$Offense[j]=="BURGLARY")
    {burglary_count_other<-burglary_count_other+temp_result$count_num[j]}
    else if(temp_result$Offense[j]=="FELONY ASSAULT")
    {felony_count_other<-felony_count_other+temp_result$count_num[j]}
    else if(temp_result$Offense[j]=="GRAND LARCENY" )
    {grand_larceny_count_other<-grand_larceny_count_other+temp_result$count_num[j]}
    else if(temp_result$Offense[j]=="GRAND LARCENY OF MOTOR VEHICLE")
    {grand_larceny_motor_count_other<-grand_larceny_motor_count_other+temp_result$count_num[j]}
    else if(temp_result$Offense[j]=="MURDER & NON-NEGL. MANSLAUGHTE" )
    {murder_count_other<-murder_count_other+temp_result$count_num[j]}
    else if(temp_result$Offense[j]=="RAPE")
    {rape_count_other<-rape_count_other+temp_result$count_num[j]}
    else
    {robbery_count_other<-robbery_count_other+temp_result$count_num[j]}
  }
  print(paste(as.character(i/nrow(other_data_part)*100),'%',' completed',sep=''))
}

slices_other <- c(burglary_count_other, felony_count_other,grand_larceny_count_other, 
            grand_larceny_motor_count_other, murder_count_other,robbery_count_other,rape_count_other)
lbls_other <- c("BURGLARY", "FELONY ASSAULT", "GRAND LARCENY", "GRAND LARCENY OF MOTOR VEHICLE",
          "MURDER & NON-NEGL. MANSLAUGHTE",'ROBBERY',"RAPE")
pct_other <- round(slices_other/sum(slices_other)*100)
lbls_other <- paste(lbls_other, pct_other) # add percents to labels 
lbls_other <- paste(lbls_other,"%",sep="") # ad % to labels 
pie(slices_other,labels = lbls_other, col=rainbow(length(lbls_other)),
    main="Pie Chart of Crime Other")

other_result<-data.frame( c("BURGLARY", "FELONY ASSAULT", "GRAND LARCENY", "GRAND LARCENY OF MOTOR VEHICLE",
                            "MURDER & NON-NEGL. MANSLAUGHTE",'ROBBERY',"RAPE"),
                          slices_other)
colnames(other_result)<-c('crime_type','crime_count')
save(other_result,file='other_result.RData')


##################################################################################################
#using 30 days accumulated felony and robbery crime count to predict murder
#clear global environment
rm(list=ls(all=TRUE))

library(data.table)
library(dplyr)

#set work dictority
setwd("C:/Study/Columbia/W4243_Applied_Data_Science/Github/Project2/Fall2016-Proj2-grp6/data")

# Import filtered data and zip statics
load('crime_data_with_zipcode.RData')
#only select felony assult, robbery, murder and only select data after 2005.
filtered_data<-crime.new                             %>%
              filter(date_time >= 13000)

#using a result to store the count result
result<-c()

#every day count the felony and robbery data in a specific precinct for the past 30days
#and count the murder data in the precinct in next day
for(i in 13030:(max(filtered_data$date_time)-1))
{
  #filter the data since we care about the past 30 days data
  total_data<-filtered_data                                 %>%
              filter((date_time<=i) & (date_time> (i-30))) 
  
  #group the data by Offense and Precinct
  data_count_group<-total_data                         %>% 
                    group_by(Offense,Precinct)         %>% 
                    summarise(count_num=n())
  
  #the unique precinct
  precinct<-unique(data_count_group$Precinct)
  
  
  for(j in precinct)
  {
    felony_count<-data_count_group[(data_count_group$Offense=='FELONY ASSAULT') &
                                   (data_count_group$Precinct== j),]$count_num
    if(length(felony_count)==0)
    {felony_count<-0}
    
    robbery_count<-data_count_group[(data_count_group$Offense=='ROBBERY') &
                                    (data_count_group$Precinct== j),]$count_num
    if(length(robbery_count)==0)
    {robbery_count<-0}
    
    burglary_count<-data_count_group[(data_count_group$Offense=='BURGLARY') &
                                     (data_count_group$Precinct== j),]$count_num
    if(length(burglary_count)==0)
    {burglary_count<-0}
    
    larceny_count<-data_count_group[(data_count_group$Offense=='GRAND LARCENY') &
                                      (data_count_group$Precinct== j),]$count_num
    if(length(larceny_count)==0)
    {larceny_count<-0}
    
    motor_larceny_count<-data_count_group[(data_count_group$Offense=='GRAND LARCENY OF MOTOR VEHICLE') &
                                          (data_count_group$Precinct== j),]$count_num
    if(length(motor_larceny_count)==0)
    {motor_larceny_count<-0}
    
    murder_count<-data_count_group[(data_count_group$Offense=='MURDER & NON-NEGL. MANSLAUGHTE') &
                                      (data_count_group$Precinct== j),]$count_num
    if(length(murder_count)==0)
    {murder_count<-0}
    
    rape_count<-data_count_group[(data_count_group$Offense=='RAPE') &
                                     (data_count_group$Precinct== j),]$count_num
    if(length(rape_count)==0)
    {rape_count<-0}
    
    #count the murder in the specific precinct in next day
    murder_next_day<-nrow(filtered_data %>%
                       filter(date_time ==(i+1) ) %>%
                       filter(Offense=='MURDER & NON-NEGL. MANSLAUGHTE') %>%
                       filter(as.numeric(Precinct)== j))
    
    result<-rbind(result,c(j,felony_count,robbery_count,burglary_count,
                           larceny_count,motor_larceny_count,murder_count,
                           rape_count,murder_next_day))
  }
  print(paste(as.character((i-13029)/(max(filtered_data$date_time)-13030)*100),'%',' completed',sep=''))
}

#rename
colnames(result)<-c('Precinct','Felony','Robbery','Burglary','larceny','motor_larceny',
                    'Murder','Rape','Murder_count')
save(result,file='30days_crime_data_against_murder_data.RData')

##########################################################################################
#plot the data and check if svm can fit the data
#set work dictority
setwd("C:/Study/Columbia/W4243_Applied_Data_Science/Github/Project2/Fall2016-Proj2-grp6/data")

# Import filtered data and zip statics
load('30days_crime_data_against_murder_data.RData')
help(qplot)
library(ggplot2)
set.seed(1)
train_rank<-sample(1:nrow(result),0.2*nrow(result))
train_data<-result[train_rank,]
train_data[,9]<-apply(train_data,1,function(x) ifelse(x[9]>=1,x[9]<-1,x[9]<-0))

train_data<-cbind(train_data,apply(train_data,1,function(x) x[2]/(sum(x)-x[1]-x[9])))
train_data<-cbind(train_data,apply(train_data,1,function(x) x[3]/(sum(x)-x[1]-x[9])))


test_data<-result[-train_rank,]
test_data[,9]<-apply(test_data,1,function(x) ifelse(x[9]>=1,x[9]<-1,x[9]<-0))

#only use a part of data
set.seed(1)
train_data_part<-train_data[sample(1:nrow(train_data),0.2*nrow(train_data)),]
set.seed(1)
test_data_part<-test_data[sample(1:nrow(test_data),0.2*nrow(test_data)),]

set.seed(1)
plot_rank<-sample(1:nrow(train_data),0.03*nrow(train_data))
qplot(train_data[plot_rank,10],train_data[plot_rank,11],colour=train_data[plot_rank,9],
      xlim=c(0,0.4),ylim=c(0,0.5),
      xlab='Felony ratio',ylab='Robbery ratio',main='Felong and Robbery ratio against Murder')

library(e1071)
#original SVM regression model
SVM<-function(xtrain,ytrain,xtest,ytest)
{
  x_train = as.matrix(xtrain)
  y_train = as.factor(ytrain)
  x_test = as.matrix(xtest)
  y_test = as.factor(ytest)
  x_mean = apply(as.matrix(x_train),2,mean)
  x_sd = apply(as.matrix(x_train),2,sd)
  x_train_norm = matrix(data=0,nrow=nrow(x_train),ncol=ncol(x_train))
  for (i in 1:nrow(x_train_norm))
  {
    x_train_norm[i,] = (x_train[i,]-x_mean)/x_sd
  }
  x_test_norm = matrix(data=0,nrow=nrow(x_test),ncol=ncol(x_test))
  for (i in 1:nrow(x_test))
  {
    x_test_norm[i,] = (x_test[i,]-x_mean)/x_sd
  }
  #y_train_factor=as.factor(y_train>0)
  #train_data=data.frame(x_train_norm,y_train_factor)
  train_data=data.frame(x_train_norm,y_train)
  tune.out=tune(svm ,y_train~.,data=train_data ,kernel ='linear',
                ranges =list(cost=c(0.001 , 0.01, 0.1, 1)))
  bestmod =tune.out$best.model
  ypred=predict (bestmod ,x_test_norm )
  #y_test_factor=as.factor(y_test>0)
  #result=list("pred"=ypred,"true"=y_test_factor)
  result=list("pred"=ypred,"true"=y_test)
  return(result)
}

svm_part_result<-SVM(train_data_part[,2:8],train_data_part[,9],
                     test_data_part[,2:8],test_data_part[,9])
table(predict =svm_part_result$pred , truth= svm_part_result$true)


#################################################################################################
#clear global environment
#save the ratio
rm(list=ls(all=TRUE))

#set work dictority
setwd("C:/Study/Columbia/W4243_Applied_Data_Science/Github/Project2/Fall2016-Proj2-grp6/data")

# Import filtered data and zip statics
load('30days_crime_data_against_murder_data.RData')

#
set.seed(1)
train_rank<-sample(1:nrow(result),0.2*nrow(result))
train_data<-result[train_rank,]

#
train_data[,9]<-apply(train_data,1,function(x) ifelse(x[9]>=1,x[9]<-1,x[9]<-0))

#
for(i in 2:8){
  train_data<-cbind(train_data,apply(train_data,1,function(x) x[i]/sum(x[2:8])))
}

#
crime_ratio_result<-data.frame(train_data[,9:16])
colnames(crime_ratio_result)<-c('murder_count',"felony","robbery",'burglary',
                                 "larceny", "larceny_motor",'murder','rape')
set.seed(1)
crime_ratio_result_part<-crime_ratio_result[sample(1:nrow(crime_ratio_result),0.1*nrow(crime_ratio_result)),]
save(crime_ratio_result_part,file='crime_ratio_result_part.RData')

