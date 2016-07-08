

################################################
#change the last column ad nonad to binary
change.ad.to.01 <- function(data){
  V1559 = c()
  for ( i in 1:length(data[,1559] )){
    if( data[i,1559] == "ad.")
      V1559 = append(V1559,1)
    else
      V1559 = append(V1559,0)
  }
  data[1559] = V1559
  return (data)
}


#################################################
#Find Mode in a Binary Column
find.mode.binary <- function( testc){
  element1 = element2 = 0
  for(i in testc){
    if(i == 0)   element1 = element1+1
    if(i == 1)   element2 = element2+1
  }
  maxi = max(element1,element2)
  if(maxi == element1)  ma = 0
  if(maxi == element2)  ma = 1
  return(ma)
}


#################################################
# Find column which is all with zero
find.Zero.column <- function(data){
  WrongColumn = c()
  for(i in length(data[1,]):4  ){
    delete = TRUE
    for(j in length(data[,1]):1){
      if(data[j,i] == 1){
        delete = FALSE
        break
      }
    }
    if(delete){
      WrongColumn = append(WrongColumn,i)
    }
  }
  return(WrongColumn)
}
#################################################
#delete raw data which is NA or "?"
delete.missing.data.by.column.NA <- function(data,colD){
  for (i in length(data[,colD]):1){
    if( is.na(DeleteData[i,colD])  || DeleteData[i,colD] == "?" ){
      data = data[-i,]
    }
  }
  return(data)
}

#################################################
#fine missing data 
#fill with the average
find.missing.data<- function(data,colN){
  MissingList = c()
  Sum = 0
  for (i in length(data[,colN]):1){
    if(   is.na(data[i,colN])  ){
      MissingList = append(MissingList,i)
    }
    else
      Sum = Sum + data[i,colN]
  }
  Average = round(Sum/(length(data[,colN]) - length(MissingList)),2)
  for(i in MissingList)
    data[i,colN] = Average
  
  Missing = read.csv("p2-data-MissingData.csv")
  MissingList[920] = NA
  Missing = cbind(Missing,MissingList)
  write.table(Missing, "p2-data-MissingData.csv", sep="," ,row.name=F)
  return(data)

}

#################################################
#fine missing data  in 4th column
#fill with the mode
find.missing.data.fill.with.mode<- function(data){
  missing_4=c()
  mode_4 = find.mode.binary(data[,4])
  for (i in length(data[,4]):1){
    if( data[i,4] == "?"){
      missing_4 = append(missing_4,i)
    }
  }
  for(i in missing_4)
    data[i,4] = mode_4
  
  Missing = read.csv("p2-data-MissingData.csv")
  missing_4[920] = NA
  Missing = cbind(Missing,missing_4)
  write.table(Missing, "p2-data-MissingData.csv", sep="," ,row.name=F)
  return(data)
  
}





################################################
setwd("/Users/ling/Documents/INFO 7390 Data Science/Assignment/Mid-Term/problem2")

rawdata<- read.csv("ad.txt",header=F)
rawdata[,1] = as.numeric(gsub(" ","", rawdata[,1]))
rawdata[,2] = as.numeric(gsub(" ","", rawdata[,2]))
rawdata[,3] = as.numeric(gsub(" ","", rawdata[,3]))
rawdata = change.ad.to.01(rawdata)



#replace missing data with NA
write.table(rawdata, "p2-data-WithNA.csv", sep="," ,row.name=F)




#replace missing data with average and mode
AverageData = rawdata
Missing = data.frame(1:920)
write.table(Missing, "p2-data-MissingData.csv",row.name=F)
remove(Missing)
#column 1-3
AverageData=find.missing.data(AverageData,1)
AverageData=find.missing.data(AverageData,2)
AverageData=find.missing.data(AverageData,3)
# column 4
AverageData=find.missing.data.fill.with.mode(AverageData)
write.table(AverageData, "p2-data-Average.csv", sep="," ,row.name=F)





#delete the raw which contain the missing data
DeleteData = rawdata
DeleteData = delete.missing.data.by.column.NA(DeleteData,1)
DeleteData = delete.missing.data.by.column.NA(DeleteData,2)
DeleteData = delete.missing.data.by.column.NA(DeleteData,3)
DeleteData = delete.missing.data.by.column.NA(DeleteData,4)
#delete zero Column to build model to fill the missing data
wrong = find.Zero.column(DeleteData)
DeleteDataDeleteColumn = DeleteData
for( i in wrong){
  DeleteDataDeleteColumn = DeleteDataDeleteColumn[,-i]
}
remove(wrong)
write.table(DeleteDataDeleteColumn, "p2-data-DeleteDataDeleteColumn.csv", sep="," ,row.name=F)
##########################################
#build model by DeleteDataDeleteColumn
#to fill the missing data 
library(caTools)

PreModelV1 = DeleteDataDeleteColumn[,-2:-3]
attach(PreModelV1)
lmV1 = lm(V1~.,PreModelV1)
PreForecaseV1 = AverageData[,-2:-3]
ForecastV1 = round(predict(lmV1,PreForecaseV1),2)
remove(PreModelV1,PreForecaseV1)

PreModelV2 = DeleteDataDeleteColumn[,-1]
PreModelV2 = PreModelV2[,-2]
attach(PreModelV2)
lmV2 = lm(V2~.,PreModelV2)
PreForecaseV2 = AverageData[,-1]
PreForecaseV2 = PreForecaseV2[,-2]
ForecastV2 = round(predict(lmV2,PreForecaseV2),2)
remove(PreModelV2,PreForecaseV2)

PreModelV3 = DeleteDataDeleteColumn[,-1:-2]
attach(PreModelV3)
lmV3 = lm(V3~.,PreModelV3)
PreForecaseV3 = AverageData[,-1:-2]
ForecastV3 = round(predict(lmV3,PreForecaseV3),4)
remove(PreModelV3,PreForecaseV3)

Missing = read.csv("p2-data-MissingData.csv",sep = ",")
Missing1 = as.integer(na.exclude(Missing[,2])) 
Missing2 = as.integer(na.exclude(Missing[,3])) 
Missing3 = as.integer(na.exclude(Missing[,4])) 
remove(Missing)

FillData = AverageData
for(i in Missing1){
  FillData[i,1] = ForecastV1[i]
}
for(i in Missing2){
  FillData[i,2] = ForecastV2[i]
}
for(i in Missing3){
  FillData[i,3] = ForecastV3[i]
}
remove(ForecastV1,ForecastV2,ForecastV3)
remove(Missing1,Missing2,Missing3)
write.table(FillData, "p2-data-FillData.csv", sep="," ,row.name=F)








