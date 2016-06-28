
library(forecast)
library("MASS")
library("ISLR")
library("zoo")
setwd("/Users/ling/Documents/INFO 7390 Data Science/Assignment/Assignment 2")


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
write.table( data, "DataWithEnergy.csv",sep=",",row.name=F)


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
rawdata<-read.csv("DataWithEnergy.csv",header=T)

rawdata = rawdata[,-1:-4]
kWh = c(0)
data = cbind(data[,1:2],kWh,data[,3:9])

for(i in 0:8015){
  c = 1+((i-(i%%24))/24)
  for(j in 1:24){
    data[(c-1)*24+j,3] = sum(rawdata[c,(((j-1)*12)+1):(((j-1)*12)+12)])
  }
}

write.table(data, "DataWithoutWeather.csv", sep="," ,row.name=F)



library("devtools")
library("weatherData")
library("plyr")



weather = getWeatherForDate("KBOS","2014-01-01", "2014-11-30",opt_detailed = T,opt_all_columns = T)
write.table(weather, "rawdata-weather.csv", sep="," ,row.name=F)
#weather<-read.csv("rawdata-weather.csv",header=T)

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

#Weather data is ready, now we are combine weather and new data





data<-read.csv("DataWithoutWeather.csv",header=T)
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

write.table(missinglist, "MissingWeatherData.csv", sep="," ,row.name=F)

write.table(data, "FinalData-Zero.csv", sep="," ,row.name=F)



#Check if data in kWh is complete
list = data[,3]
MissingKwhData = c()
for(i in length(list):1){
  if(is.na(list[i])||list[i]==0){
    data = data[-i,]
    MissingKwhData = append(MissingKwhData,i)
  }
}
write.table(MissingKwhData, "MissingKwhData.csv", sep="," ,row.name=F)
write.table(data, "FinalData-A-NoZero.csv", sep="," ,row.name=F)


 MissingKwhData<-read.csv("MissingKwhData.csv",header=T)

#original Data with Zero
data.Zero<-read.csv("FinalData-Zero.csv",header=T)
lm.Zero= lm(kWh ~ month + day + hour + DayofWeek + weekdays + peakhour + temperature,data = data.Zero)
summary(lm.Zero)
pred.Zero = predict(lm.Zero,data.Zero)
acc.Zero <- t(accuracy(pred.Zero, data.Zero$kWh))


#Part A
data.NoZero<-read.csv("FinalData-A-NoZero.csv",header=T)
lm.NoZero= lm(kWh ~ month + day + hour + DayofWeek + weekdays + peakhour + temperature,data = data.NoZero)
summary(lm.NoZero)
pred.NoZero = predict(lm.NoZero,data.NoZero)
acc.NoZero <- t(accuracy(pred.NoZero, data.NoZero$kWh))


#Part B
data.FillZero = data.Zero
pred.FillZeroTemp = predict(lm.NoZero,data.Zero)
for(i in MissingKwhData){
  data.FillZero[i,3] = pred.FillZeroTemp[i]
}
write.table(data.FillZero, "FinalData-B-FillZero.csv", sep="," ,row.name=F)
lm.FillZero= lm(kWh ~ month + day + hour + DayofWeek + weekdays + peakhour + temperature,data = data.FillZero)
summary(lm.FillZero)
pred.FillZero = predict(lm.FillZero,data.FillZero)
acc.FillZero <- t(accuracy(pred.FillZero, data.FillZero$kWh))


#Part C
data.NA = data.Zero
for(i in MissingKwhData){
  data.NA[i,3] = NA
}
#data.NA = na.locf(data.NA,na.rm = TRUE)
data.NA = na.locf(data.NA)
write.table(data.NA, "FinalData-C-NA.csv", sep="," ,row.name=F)
lm.NA= lm(kWh ~ month + day + hour + DayofWeek + weekdays + peakhour + temperature,data = data.NA)
summary(lm.NA)
pred.NA = predict(lm.NA,data.NA)
acc.NA <- t(accuracy(pred.NA, data.Zero$kWh))



#Part D

data.D = data.Zero
data.D.withoutWrong = data.D
list.D = data.D[,3]
WoringKwhData = c()
for(i in length(list.D):1){
  if(is.na(list.D[i])||list.D[i]<=10){
    WoringKwhData = append(WoringKwhData,i)
    data.D.withoutWrong = data.D.withoutWrong[-i,]
  }
}

write.table(WoringKwhData,"WrongkWhData.csv", sep="," ,row.name=F)

lm.D.temp = lm(kWh ~ month + day + hour + DayofWeek + weekdays + peakhour + temperature,data = data.D.withoutWrong)
pred.Dtemp = predict(lm.D.temp,data.Zero)
for(i in WoringKwhData){
  data.D[i,3] = pred.Dtemp[i]
}

write.table(data.D, "FinalData-D.csv", sep="," ,row.name=F)
lm.D= lm(kWh ~ month + day + hour + DayofWeek + weekdays + peakhour + temperature,data = data.D)
summary(lm.D)
pred.D = predict(lm.D,data.D)
acc.D <- t(accuracy(pred.D, data.D$kWh))

list.D = data.D[,3]
for(i in length(list.D):1){
  if(list.D[i]<=0){
    data.D = data.D[-i,]
  }
}

lm.D2= lm(kWh ~ month + day + hour + DayofWeek + weekdays + peakhour + temperature,data = data.D)
summary(lm.D2)
pred.D2 = predict(lm.D2,data.D)
acc.D2 <- t(accuracy(pred.D2, data.D$kWh))

write.table(data.D, "FinalData-D+.csv", sep="," ,row.name=F)


#Combine the Accuracy
acc = cbind(acc.Zero,acc.NoZero,acc.FillZero,acc.NA,acc.D,acc.D2)
colnames(acc) = c("Zero","NoZero","FillZero","NA","D","D+")
acc = round(acc,3)
acc = acc[-1, ]
acc = acc[-3,]
rowname = c("RMSE","MAE","MAPE")
acc= cbind(rowname,acc)
write.table(acc, "Accuracy.csv", sep="," ,row.name=F)

write.table(data.D, "ready-data.csv", sep="," ,row.name=F)



