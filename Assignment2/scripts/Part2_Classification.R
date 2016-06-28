library(ISLR)
mydata<-read.csv("/Users/Wenjin/Desktop/>INFO7390 DataScience/Assignments/Assignment 2/ling-part-1/ready-data.csv",header=T)
attach(mydata)
mkWh = mean(kWh)
kWh_class = ifelse(kWh>mkWh,"Above_Normal","Optimal")
#kWh_ = ifelse(kWh>mkWh,1,0)
mydata = data.frame(mydata,kWh_class)
#head(mydata)

## Classification tree
library(tree)
mytree = tree(kWh_class~month + day + hour + DayofWeek + weekdays + peakhour + temperature,mydata)
plot(mytree)
text(mytree,pretty = 0)
print(mytree)

## Neural network
library(neuralnet)
library(nnet)
#net.mydata <- neuralnet(kWh_~month + day + hour + DayofWeek + weekdays + peakhour + temperature,mydata,hidden = 0)
#plot(net.mydata)
nn = nnet(kWh_class~month + day + hour + DayofWeek + weekdays + peakhour + temperature,mydata,size=1)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(nn)
print(nn)

## Logical regression
lg = glm(kWh_class~month + day + hour + DayofWeek + weekdays + peakhour + temperature,family=binomial(link='logit'),mydata)
par(mfrow = c(2,2))
plot(lg,which=c(1:4))

## Calculate the classification performance metrics
# Classification tree
set.seed(2)
train = sample(1:nrow(mydata),200)
mydata.test = mydata[-train,]
kWh_class.test = kWh_class[-train]
tree.train = tree(kWh_class~month + day + hour + DayofWeek + weekdays + peakhour + temperature,mydata,subset = train)
tree.pred = predict(tree.train,mydata.test,type="class")
me = table(tree.pred,kWh_class.test) 
err = round((me[2,1]+me[1,2])/(me[1,1]+me[1,2]+me[2,1]+me[2,2])*100,digits = 2)
# Neural network
set.seed(22)
train = sample(1:nrow(mydata),200)
mydata.test = mydata[-train,]
kWh_class.test = kWh_class[-train]
net.pred = predict(nn,mydata,type = "class")
net.t = table(net.pred,kWh_class)
net.e = round((net.t[2,1]+net.t[1,2])/(net.t[1,1]+net.t[1,2]+net.t[2,1]+net.t[2,2])*100,digits = 2)

# Output the performance metrics
row1 = c("Account No.","26435791004","")
row2 = c("Neural Network","","")
row3 = c("Overall Error",paste(net.e,"%"),"")
row4 = c("","Above Normal","Optimal")  
row5 = c("Above Normal",net.t[1,1],net.t[1,2])
row6 = c("Optimal",net.t[2,1],net.t[2,2])

row7 = c("Classification Tree","","")
row8 = c("Overall Error",paste(err,"%"),"")
row9 = c("","Above Normal","Optimal")
row10 = c("Above Normal",me[1,1],me[1,2])
row11 = c("Optimal",me[2,1],me[2,2])
pm = rbind(row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,row11)
write.table(pm, "/Users/Wenjin/Desktop/>INFO7390 DataScience/Assignments/Assignment 2/outputs/ClassificationPerformanceMetrics.csv", sep = ",", row.names = F, col.names = F)

## Forecast new data
forecast1<-read.csv("/Users/Wenjin/Desktop/>INFO7390 DataScience/Assignments/Assignment 2/ling-Forecast-Part1-a/ready-forecast-data1.csv",header=T)
forecast2<-read.csv("/Users/Wenjin/Desktop/>INFO7390 DataScience/Assignments/Assignment 2/ling-Forecast-Part1-a/ready-forecast-data2.csv",header=T)
# Classification tree
tree.forecast1.res <- cbind(forecast1, kWh_class = predict(mytree,forecast1,type = "class"))
tree.forecast1.res = tree.forecast1.res[,-2:-4]
tree.forecast1.res = tree.forecast1.res[,-3:-5]
write.csv(tree.forecast1.res, "/Users/Wenjin/Desktop/>INFO7390 DataScience/Assignments/Assignment 2/outputs/forecastOutput_26435791004_ClassificationTree.csv", row.names = F)
tree.forecast2.res <- cbind(forecast2, kWh = predict(mytree,forecast2,type = "class"))
tree.forecast2.res = tree.forecast2.res[,-2:-4]
tree.forecast2.res = tree.forecast2.res[,-3:-5]
write.csv(tree.forecast2.res, "/Users/Wenjin/Desktop/>INFO7390 DataScience/Assignments/Assignment 2/outputs/forecastOutput2_26435791004_ClassificationTree.csv", row.names = F)

# Neural network
forecastData1 <- forecast1[,-4]
forecastData1 <- forecastData1[,-1]
forecastData2 <- forecast2[,-4]
forecastData2 <- forecastData2[,-1]
net.forecast1.res <- cbind(forecast1, kWh = predict(nn,forecastData1,type = "class")) 
#net.forecast1.res <- cbind(forecast1, kWh = compute(net.mydata,forecastData1))
net.forecast1.res = net.forecast1.res[,-2:-4]
net.forecast1.res = net.forecast1.res[,-3:-5]
write.csv(net.forecast1.res, "/Users/Wenjin/Desktop/>INFO7390 DataScience/Assignments/Assignment 2/outputs/forecastOutput_26435791004_neuralNetworkClassification.csv", row.names = F)
net.forecast2.res <- cbind(forecast2, kWh = predict(nn,forecastData2,type = "class"))
net.forecast2.res = net.forecast2.res[,-2:-4]
net.forecast2.res = net.forecast2.res[,-3:-5]
write.csv(net.forecast2.res, "/Users/Wenjin/Desktop/>INFO7390 DataScience/Assignments/Assignment 2/outputs/forecastOutput2_26435791004_neuralNetworkClassification.csv", row.names = F)

