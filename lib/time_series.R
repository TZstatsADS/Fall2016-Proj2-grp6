##### Extract daily crime data by crime type

A <- read.csv("NYPD_7_Major_Felony_Incidents.csv")
A <- na.omit(A)
A <- A[A$Occurrence.Year>=2006,]
A$Date <- as.Date(A$Occurrence.Date ,format='%m/%d/%Y %I:%M:%S %p')
As <- A[,c("Offense", "Date")]

d <- seq(as.Date("2006-01-01"), as.Date("2015-12-31"), by='days')
d.frame <- data.frame(list(Date=d))


library(plyr)
library(forecast)
library(nnet)
library(Hmisc)


l <- levels(As$Offense)
A.list <- split(As, f = As$Offense)

FA <- ddply(A.list$`FELONY ASSAULT`, .(Date), nrow)
M <- ddply(A.list$`MURDER & NON-NEGL. MANSLAUGHTE`, .(Date), nrow)
BUR <- ddply(A.list$BURGLARY, .(Date), nrow)[-c(1:2),]
GL <- ddply(A.list$`GRAND LARCENY`, .(Date), nrow)
GL.M <- ddply(A.list$`GRAND LARCENY OF MOTOR VEHICLE`, .(Date), nrow)
R <- ddply(A.list$RAPE, .(Date), nrow)
ROB <- ddply(A.list$ROBBERY, .(Date), nrow)

Merged <- Merge(BUR, FA, GL, GL.M, M, R, ROB, id = ~Date)
Merged <- Merge(GL, FA, ROB, BUR, GL.M, R, M, id = ~Date)
colnames(Merged) <- c('Date', "GRAND LARCENY", "FELONY ASSAULT", "ROBBERY", 
                      "BURGLARY", "GRAND LARCENY OF MOTOR VEHICLE", "RAPE",
                      "MURDER")
write.csv(Merged, file='preddata.csv', na='0')


#### make plot to see trend
M.date.full <- merge(d.frame, M.date, all=T)
M.date.full$V1[which(is.na(M.date.full$V1))] <- 0
M.date.full$ad <- M.date.full$V1*10
plot(V1~Date, data = FA.date, ylim = c(0,100), 
     xlim=c(as.Date('2006-01-01'), as.Date('2006-03-01')), type='l')
par(new=T)
plot(ad~Date, data = M.date.full, ylim = c(0,100), 
     xlim=c(as.Date('2006-01-01'), as.Date('2006-03-01')), type='l', 
     col=2, axes=F)
par(new=F)

FA.ts <- msts(FA.date$V1, seasonal.periods = c(7, 365.25))
FA.fit <- tbats(FA.ts)
FA.fc <- forecast(FA.fit)
plot(FA.fc)


M.ts <- msts(M.date.full$V1, seasonal.periods = c(7, 365.25))
M.fit <- tbats(M.ts)
M.fc <- forecast(M.fit)
plot(M.fc)


#####save fit model for app use

GL.fit <- tbats(msts(data[,1], seasonal.periods = c(7, 365.25)))
FA.fit <- tbats(msts(data[,2], seasonal.periods = c(7, 365.25)))
RO.fit <- tbats(msts(data[,3], seasonal.periods = c(7, 365.25)))
BU.fit <- tbats(msts(data[,4], seasonal.periods = c(7, 365.25)))
MV.fit <- tbats(msts(data[,5], seasonal.periods = c(7, 365.25)))
RA.fit <- tbats(msts(data[,6], seasonal.periods = c(7, 365.25)))
MU.fit <- tbats(msts(data[,7], seasonal.periods = c(7, 365.25)))

save(GL.fit, FA.fit, RO.fit, BU.fit, MV.fit, RA.fit, MU.fit, file = "~/Github/fit.Rdata")

