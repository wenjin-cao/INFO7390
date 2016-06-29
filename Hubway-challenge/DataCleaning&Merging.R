

setwd("/Users/ling/Documents/INFO 7390 Data Science/Assignment/Assignment class")

#read the file
stations<- read.csv("hubway_stations.csv",header=T)
trips<-read.csv("hubway_trips.csv",header=T)


# copy the station to a start_station and end_station 
# it is easier for us to merge the station and trips
startstations = stations
colnames(startstations) = c("start_id","start_terminal",  "start_station" ,"start_municipal","start_lat","start_lng","start_status")
endstations = stations
colnames(endstations)= c("end_id","end_terminal","end_station","end_municipal","end_lat","end_lng" ,"end_status")

# merge data
trips <- merge(trips, endstations,  by.x = "end_statn",   by.y = "end_id")
trips <- merge(trips, startstations, by.x = "strt_statn",  by.y = "start_id")

#remove ' in front of every zipcode
trips$zip_code = gsub("'","",trips$zip_code)

#seperate date to day and time
start_day = gsub(" [0-9][0-9]:[0-9][0-9]:[0-9][0-9]","",trips$start_date)
start_time = gsub("[0-9]*/[0-9]*/[0-9]* ","",trips$start_date)
end_day = gsub(" [0-9][0-9]:[0-9][0-9]:[0-9][0-9]","",trips$end_date)
end_time = gsub("[0-9]*/[0-9]*/[0-9]* ","",trips$end_date)

trips = cbind(trips[,1:7],start_day,start_time,trips[,8:25])
trips = cbind(trips[,1:10],end_day,end_time,trips[,11:25])

#export the file
write.table(trips, "trips.csv", sep="," ,row.name=F)
