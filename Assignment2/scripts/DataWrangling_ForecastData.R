
library(tree)
library(MASS)
library(ISLR)
library(grid)
library(neuralnet)



setwd("/Users/ling/Documents/INFO 7390 Data Science/Assignment/Assignment 2")

ForecastRawData1<- read.csv2("forecastNewData1.csv",header=T)
ForecastRawData2<-read.csv("forecastNewData2.csv",header=T)




date1 = ForecastRawData1[,1]
hour1 = ForecastRawData1[,2]
temperature1 = ForecastRawData1[,3]

date2 = ForecastRawData2[,1]
hour2 = ForecastRawData2[,2]
temperature2 = ForecastRawData2[,3]

#Month
ForecastRawData1 = cbind(ForecastRawData1[,1],6,ForecastRawData1[,2:3])
month2 = gsub("/[0-9]*/[0-9]*","",date2)
month2 = as.numeric(month2)
ForecastRawData2 = cbind(ForecastRawData2[,1],month2,ForecastRawData2[,2:3])

#Day
day1 = gsub("201606","",date1)
day1 = as.numeric(day1)
ForecastRawData1 = cbind(ForecastRawData1[,1:2],day1,ForecastRawData1[,3:4])

day2 = gsub("/[0-9][0-9][0-9][0-9]","",date2)
day2 = gsub("[0-9]*/","",day2)
day2 = as.numeric(day2)
ForecastRawData2 = cbind(ForecastRawData2[,1:2],day2,ForecastRawData2[,3:4])

#year
ForecastRawData1 = cbind(ForecastRawData1[,1:3],2016,ForecastRawData1[,4:5])
ForecastRawData2 = cbind(ForecastRawData2[,1:3],2014,ForecastRawData2[,4:5])


#peakhour
peakhour1 = c()
for(i in 1:length(date1)){
  if(ForecastRawData1[i,5]%%24 < 7||ForecastRawData1[i,5]%%24 > 19 ){
    peakhour1 = append(peakhour1,0)
  }
  else{
    peakhour1 = append(peakhour1,1)
  }
}
ForecastRawData1 = cbind(ForecastRawData1[,1:5],peakhour1,ForecastRawData1[,6])

peakhour2 = c()
for(i in 1:length(date2)){
  if(ForecastRawData2[i,5]%%24 < 7||ForecastRawData2[i,5]%%24 > 19 ){
    peakhour2 = append(peakhour2,0)
  }
  else{
    peakhour2 = append(peakhour2,1)
  }
}
ForecastRawData2 = cbind(ForecastRawData2[,1:5],peakhour2,ForecastRawData2[,6])



#DayofWeek
DayofWeek1 = c()
for(i in 1:length(date1)){
  DayofWeek1 = append(DayofWeek1, ((day1[i]-5)%%7)  )
}
ForecastRawData1 = cbind(ForecastRawData1[,1:5],DayofWeek1,ForecastRawData1[,6:7])

DayofWeek2 = c()
for(i in 1:length(date2)){
  DayofWeek2 = append(DayofWeek2, ((day2[i]-1)%%7)  )
}
ForecastRawData2 = cbind(ForecastRawData2[,1:5],DayofWeek2,ForecastRawData2[,6:7])


#weekdays
weekdays1= c()
for(i in 1:length(date1)){
  if(DayofWeek1[i] == 0||DayofWeek1[i] == 6){
    weekdays1 = append(weekdays1,0)
  }
  else{
    weekdays1 = append(weekdays1,1)
  }
}
ForecastRawData1 = cbind(ForecastRawData1[,1:6],weekdays1,ForecastRawData1[,7:8])

weekdays2 = c()
for(i in 1:length(date2)){
  if(DayofWeek2[i] == 0||DayofWeek2[i] == 6){
    weekdays2 = append(weekdays2,0)
  }
  else{
    weekdays2 = append(weekdays2,1)
  }
}
ForecastRawData2 = cbind(ForecastRawData2[,1:6],weekdays2,ForecastRawData2[,7:8])


names = c("Date","month","day","year","hour","DayofWeek","weekdays","peakhour","temperature")
names(ForecastRawData1) = names
names(ForecastRawData2) = names

write.table(ForecastRawData1, "ready-forecast-data1.csv", sep="," ,row.name=F)
write.table(ForecastRawData2, "ready-forecast-data2.csv", sep="," ,row.name=F)

