
######################################
#generate date
#from the 1st hour of 2009/7/1
#to the 12th hour of 2012/6/28.

generate.date<-function(){
  
  newDate = seq.Date(from = as.Date("2009/7/1"),as.Date("2012/06/28"),by="day")
  newDate = gsub("-","",newDate)
  newDate = as.numeric(newDate)
  date = c()
  for(i in 1: length(newDate)){
    for (j in 0:23){
      date = append(date,newDate[i]*100+j)
    }
  }
  date = date[1:26244]
  return(date)
}
  

##############################################
#find average for every hour by 4 observation
find.average.in.4.data<-function(data,from){
  for (i in 1: length(date)){
    n=4
    if(is.na(data[i,(from)])){
      n=n-1
      data[i,(from)] = 0
    }
    if(is.na(data[i,(from+1)])){
      n=n-1
      data[i,(from+1)] = 0
    }
    if(is.na(data[i,(from+2)])){
      n=n-1
      data[i,(from+2)] = 0
    }
    if(is.na(data[i,(from+3)])){
      n=n-1
      data[i,(from+3)] = 0
    }
    if(n!=0)
      data[i,(from+4)] = round(sum(data[i,from:(from+3)])/n,2)
  }
  return(data)
}

############################################
# find missing data in the train.csv
# use date to match
find.missing.data.in.train<- function(){
  count = 1
  Row = Date = c()
  for ( i in  1: length(date)  ){
    if(count<=length(train[,1])){
      if(  date[i] != train[count,1]  ){
        Row = append(Row,i)
        Date = append(Date,date[i])
        }
      else
        count = count+1
    }
  }
  MissingWP = cbind(Date,Row)
  row.names(MissingWP) = c(1:length(Row))
  # wf.missingWP.csv
  write.table(MissingWP, "wf.MissingWP.csv",row.name=F)
}
  
##################################################

setwd("/Users/ling/Documents/INFO 7390 Data Science/Assignment/Mid-Term/problem3")

train<- read.csv("rawdata/train.csv",header=T)
wf1<-read.csv("rawdata/windforecasts_wf1.csv",head=T)
wf2<-read.csv("rawdata/windforecasts_wf2.csv",head=T)
wf3<-read.csv("rawdata/windforecasts_wf3.csv",head=T)
wf4<-read.csv("rawdata/windforecasts_wf4.csv",head=T)
wf5<-read.csv("rawdata/windforecasts_wf5.csv",head=T)
wf6<-read.csv("rawdata/windforecasts_wf6.csv",head=T)
wf7<-read.csv("rawdata/windforecasts_wf7.csv",head=T)
 
date = generate.date()  # head(date)   tail(date)
find.missing.data.in.train()


###############################################################
#start from here
#just change the number

# wf1 wf2 wf3 wf4 wf5 wf6 wf7
wf = wf7
# 1 2 3 4 5 6 7
WFnumber = 7

u1=u2=u3=u4=u_average=v1=v2=v3=v4=v_average=ws1=ws2=ws3=ws4=ws_average=wd1=wd2=wd3=wd4=wd_average=c(NA)
wf.new=cbind(date,u1,u2,u3,u4,u_average,v1,v2,v3,v4,v_average,ws1,ws2,ws3,ws4,ws_average,wd1,wd2,wd3,wd4,wd_average)
remove(v1,v2,v3,v4,v_average,u1,u2,u3,u4,u_average,ws1,ws2,ws3,ws4,ws_average,wd1,wd2,wd3,wd4,wd_average)

#u: 2:5 6
for(i in 1:(length(wf[,1])/48)){
  tempn = i%%4
  wf.new[ (1+(i-1)*12):(48+(i-1)*12), 2+tempn] = wf[(1+(i-1)*48):(48+(i-1)*48),3]
}

# v: 7-10 11
for(i in 1:(length(wf[,1])/48)){
  tempn = i%%4
  wf.new[ (1+(i-1)*12):(48+(i-1)*12), 7+tempn] = wf[(1+(i-1)*48):(48+(i-1)*48),4]
}

# ws: 12-15 16
for(i in 1:(length(wf[,1])/48)){
  tempn = i%%4
  wf.new[ (1+(i-1)*12):(48+(i-1)*12), 12+tempn] = wf[(1+(i-1)*48):(48+(i-1)*48),5]
}

# wd: 17-20 21
for(i in 1:(length(wf[,1])/48)){
  tempn = i%%4
  wf.new[ (1+(i-1)*12):(48+(i-1)*12), 17+tempn] = wf[(1+(i-1)*48):(48+(i-1)*48),6]
}
remove(tempn)

wf.new = find.average.in.4.data(wf.new,2)
wf.new = find.average.in.4.data(wf.new,7)
wf.new = find.average.in.4.data(wf.new,12)
wf.new = find.average.in.4.data(wf.new,17)

# wf1.mergeData.csv
filename = gsub(" ","",paste("wf",WFnumber,".mergeData.csv"))
write.table(wf.new, filename, sep="," ,row.name=F)
remove(filename)


#take the average column out 
#build a new data frame to train model
u = wf.new[,6]
v = wf.new[,11]
ws = wf.new[,16]
wd = wf.new[,21]
wp = c(NA)
wf.final = cbind(date,u,v,ws,wd,wp)
remove(u,v,ws,wd,wp)

count = 1
for( i in 1: length(date) ) {
  if(count<=length(train[,1])){
    if( date[i] == train[count,1] ){
      wf.final[i,6] = train[count,WFnumber+1]
      count = count+1
    }
  }
}
remove(count)

#wf1.data.csv
filename = gsub(" ","",paste("wf",WFnumber,".data.csv"))
write.table(wf.final, filename, sep="," ,row.name=F)
remove(filename)


wf.train = wf.final[1:13176,] 
wf.evaluation = wf.final[13177:26244,]

for ( i in  length(wf.train[,6]) :1 ){
  if(is.na(wf.train[i,6])  ){
    wf.train = wf.train[-i,]
  }
}

filename.train = gsub(" ","",paste("wf",WFnumber,"-ready-train.csv"))
filename.evaluation = gsub(" ","",paste("wf",WFnumber,"-ready-evaluation.csv"))
write.table(wf.train, filename.train, sep="," ,row.name=F)
write.table(wf.evaluation, filename.evaluation, sep="," ,row.name=F)
remove(filename.evaluation,filename.train)




