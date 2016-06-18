

data<-read.csv("/Users/ling/Documents/INFO 7390 Data Science/Assignment/Assignment 1/forecastData.csv",header=T)

date = data[,1]
hour = data[,2]
temperature = data[,3]

month = gsub("/[0-9]*/[0-9]*","",date)
month = as.numeric(monthL)
data = cbind(data[,1],month,data[,2:3])

day = gsub("/[0-9][0-9][0-9][0-9]","",date)
day = gsub("[0-9]*/","",day)
day = as.numeric(day)
data = cbind(data[,1:2],day,data[,3:4])

year = gsub("[0-9]*/[0-9]*/","",date)
year = as.numeric(year)
data = cbind(data[,1:3],year,data[,4:5])


peakhour = c()
for(i in 1:length(date)){
  if(data[i,5]%%24 < 7||data[i,5]%%24 > 19 ){
    peakhour = append(peakhour,0)
  }
  else{
    peakhour = append(peakhour,1)
  }
}
data = cbind(data[,1:5],peakhour,data[,6])


DayofWeek = c()
for(i in 1:length(date)){
  DayofWeek = append(DayofWeek, ((day[i]-1)%%7)  )
}
data = cbind(data[,1:5],DayofWeek,data[,6:7])


weekdays= c()
for(i in 1:length(date)){
  if(DayofWeek[i] == 0||DayofWeek[i] == 6){
    weekdays = append(weekdays,0)
  }
  else{
    weekdays = append(weekdays,1)
  }
}
data = cbind(data[,1:6],weekdays,data[,7:8])

names(data)[names(data)=="data[, 6]"]="temperature";
names(data)[names(data)=="data[, 1]"]="Date";
names(data)[names(data)=="Hr"]="hour";


write.table(data, "/Users/ling/Documents/INFO 7390 Data Science/Assignment/Assignment 1/ready-forecast-data.csv", sep="," ,row.name=F)