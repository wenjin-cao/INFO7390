

setwd("/Users/ling/Documents/INFO 7390 Data Science/Assignment/Assignment 1/part2")
data<-read.csv("NewData.csv",header=T)



#delete 
#power factor
#kVARh
for (i in 1095:1) {
  if(i%%3 != 1){
    data = data[-i,];
  }
}


#backup data
write.table( data, "DataWithEnergy2.csv",sep=",",row.name=F)



#delete
#Channel
#Units
data = data[,-3:-4]




#add
#DayofWeek
DayofWeek = c(3)
for(i in 4:336){
  if(i%%7 == 0){
    DayofWeek = append(DayofWeek,0)
  }
  else{
    DayofWeek = append(DayofWeek,i%%7)
  }
}
data = cbind(data[,1:2],DayofWeek,data[,3:ncol(data)])



#add
#year
year = c(2014)
for(i in 1:333)
  year = append(year,2014)
data = cbind(data[,1:2],year,data[,3:ncol(data)])



#add
#day
day = c()
for(i in 1:31)  
  day = append(day,i)
for(i in 1:28)
  day = append(day,i)
for(i in 1:31)
  day = append(day,i)
for(i in 1:30)
  day = append(day,i)
for(i in 1:31)
  day = append(day,i)
for(i in 1:30)
  day = append(day,i)
for(i in 1:31)
  day = append(day,i)
for(i in 1:31)
  day = append(day,i)
for(i in 1:30)
  day = append(day,i)
for(i in 1:31)
  day = append(day,i)
for(i in 1:30)
  day = append(day,i)

data = cbind(data[,1:2],day,data[,3:ncol(data)])




#add
#month

month = c()

for(i in 1:31)
  month = append(month,1)
for(i in 1:28)
  month = append(month,2)
for(i in 1:31)
  month = append(month,3)
for(i in 1:30)
  month = append(month,4)
for(i in 1:31)
  month = append(month,5)
for(i in 1:30)
  month = append(month,6)
for(i in 1:31)
  month = append(month,7)
for(i in 1:31)
  month = append(month,8)
for(i in 1:30)
  month = append(month,9)
for(i in 1:31)
  month = append(month,10)
for(i in 1:30)
  month = append(month,11)

data = cbind(data[,1:2],month,data[,3:ncol(data)])





#add
#weekdays
weekdays= c()

for(i in 3:336){
  if(i%%7 == 0||i%%7 == 6){
    weekdays = append(weekdays,0)
  }
  else{
    weekdays = append(weekdays,1)
  }
}
data = cbind(data[,1:6],weekdays,data[,7:ncol(data)])



#make a new data
#repeat everyday 24 times
newdata = data.frame()

for(i in 1:334){
  title = data[i,1:7]
  for(j in 1:24){
    newdata = rbind(newdata,title)
  }
}

hour = c()
for(i in 0:8015){ #8016 = 334*24
  hour = append(hour,i%%24)
}
newdata = cbind(newdata[,1:5],hour,newdata[,6:7])

peakhour = c()
for(i in 0:8015){
  if(i%%24 < 7||i%%24 > 19 ){
    peakhour = append(peakhour,0)
  }
  else{
    peakhour = append(peakhour,1)
  }
}
newdata = cbind(newdata,peakhour)





#add calculated energy to new data
data = newdata 
rawdata<-read.csv("DataWithEnergy2.csv",header=T)

rawdata = rawdata[,-1:-4]
kWh = c(0)
data = cbind(data[,1:2],kWh,data[,3:9])

for(i in 0:8015){
  c = 1+((i-(i%%24))/24)
  for(j in 1:24){
    data[(c-1)*24+j,3] = sum(rawdata[c,(((j-1)*12)+1):(((j-1)*12)+12)])
  }
}

write.table(data, "data.csv", sep="," ,row.name=F)





install.packages("devtools")
install_github("Ram-N/weatherData")
install.packages("plyr")

library("devtools")
library("weatherData")
library("plyr")


weather = getWeatherForDate("KBOS","2014-01-01", "2014-11-30",opt_detailed = T,opt_all_columns = T)
write.table(weather, "rawdata-weather2.csv", sep="," ,row.name=F)
#weather<-read.csv("rawdata-weather2.csv",header=T)

library("plyr")
weather = arrange(weather,DateUTC)

#delete useless information
weather = weather[,-4:-16]
weather = weather[,-2]

#select XX:45 as our data
savedata = grep("[0-9]*:54 [P|A]M", weather$Time)

weathernew = data.frame()
for(i in savedata){
  weathernew = rbind(weathernew,weather[i,])
}
weather = weathernew

#change the time to an integer
timelist = weather[,1]
timelist = gsub(":54 [P|A]M","",timelist)
listnum = as.numeric(timelist)
weather = cbind(weather,listnum)
weather = weather[,-1]



#delete the repeat data
repetetemp = weather[1,2]
for(p in 2:(length(timelist)-1)){
  if( repetetemp == (weather[p,2])  ){
    weather = weather[-p,]
  }
  repetetemp = weather[p,2]
}


data<-read.csv("data.csv",header=T)
temperature = c(0)
data = cbind(data,temperature)


count = 1
tempmean = 0
templist = c()
missinglist = c()
temptotal = 0

for(i in 1:334){
  for(j in 0:23){
    if(weather[count,2]%%12 == (j)%%12){
      #data is correct match
      temptotal = temptotal+weather[count,1]
      data[(i-1)*24+j+1,11] = weather[count,1]
      count = count+1
    }
    else{
      #data is missing
      templist = append(templist,(i-1)*24+j+1)
      #data[(i-1)*24+j+1,11] = "NOTVALID"
      missinglist = append(missinglist,(i-1)*24+j+1)
    }
    
    tempmean = temptotal/(24-length(templist))
    tempmean=round(tempmean,1)
    for (k in templist){
      data[k,11] = tempmean
    }
    
  }
  tempmean = 0
  templist = c()
  temptotal = 0
  
}

write.table(missinglist, "MissingWeatherData2.csv", sep="," ,row.name=F)



data<-read.csv("datakwh.csv",header=T)

#Check if data in kWh is complete
list = data[,3]
MissingKwhData = c()
for(i in length(list):1){
  if(is.na(list[i])||list[i]<=10){
    data = data[-i,]
    MissingKwhData = append(MissingKwhData,i)
  }
}
write.table(MissingKwhData, "MissingKwhData2.csv", sep="," ,row.name=F)




write.table(data, "ready-data2.csv", sep="," ,row.name=F)







