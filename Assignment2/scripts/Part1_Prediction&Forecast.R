library(MASS)
library(ISLR)
mydata<-read.csv("/Users/Wenjin/Desktop/>INFO7390 DataScience/Assignments/Assignment 2/ling-part-1/ready-data.csv",header=T)

## Split mydata into train and test data
smp_size <- floor(0.75 * nrow(mydata)) #75% of the sample size
set.seed(123) #Set the seed to make your partition reproductible
train_ind <- sample(seq_len(nrow(mydata)), size = smp_size)
train <- mydata[train_ind, ]
test <- mydata[-train_ind, ]

## Regression tree
library(tree)
set.seed(1) #Random number generation(the need is the possible desire for reproducible results)
training = sample(1:nrow(mydata),nrow(mydata)/2)
testing = mydata[-training,]
tree.mydata = tree(kWh~month + day + hour + DayofWeek + weekdays + peakhour + temperature,mydata,subset = training)
#cv.tree(tree.mydata)
#summary(tree.mydata)
tree.pred <- predict(tree.mydata, testing)
#accuracy(testing$kWh,tree.pred)
plot(tree.mydata)
text(tree.mydata,pretty = 0)
#Variables actually used in tree construction:
#  [1] "temperature" "peakhour"    "weekdays"    "month"       "hour" 
tree.mydata = prune.tree(tree.mydata,best = 5)
tree.pred <- predict(tree.mydata, testing)
accuracy(testing$kWh,tree.pred)
#cv.tree(tree.mydata)
#summary(tree.mydata)
#plot(tree.pred)
plot(tree.mydata)
text(tree.mydata,pretty = 0)

## Neural network
mydata=mydata[,-6]
mydata=mydata[,-1]
mydata=mydata[,-1] 
library(neuralnet)
net.mydata <- neuralnet(kWh~month + day + hour + DayofWeek + weekdays + peakhour + temperature,mydata,hidden = 0)
plot(net.mydata)
print(net.mydata)

## Calculate accuracy measures for forecast model
library(forecast)
# Regression tree
tree.pred <- predict(tree.mydata, testing)#pred = predict(tree.mydata, testing) #pred = predict(tree.mydata, test)
acc = t(accuracy(testing$kWh,tree.pred))
# Neural network
library (neuralnet)
mydata<-read.csv("/Users/Wenjin/Desktop/>INFO7390 DataScience/Assignments/Assignment 2/ling-part-1/ready-data.csv",header=T)
test <- mydata[,-1:-3]
datanet <-neuralnet(kWh~ month+day+hour+DayofWeek+weekdays+peakhour+temperature, mydata, hidden=0) #plot(datanet)
results <- compute(datanet, test[,-3])
acc2 = t(accuracy(mydata$kWh, results$net.result))
## Output the performance metrics file
row1 = c("Account No.","26435791004")
row2 = c("Neural Network","")
row3 = c("MAE",round(acc2[3,1],digits = 2))
row4 = c("MAPE",paste(round(acc2[5,1],digits = 2),"%"))
row5 = c("RMSE",round(acc2[2,1],digits = 2))
row6 = c("Regression Tree","")
row7 = c("MAE",round(acc[3,1],digits = 2))
row8 = c("MAPE",paste(round(acc[5,1],digits = 2),"%"))
row9 = c("RMSE",round(acc[2,1],digits = 2))
pm = rbind(row1,row2,row3,row4,row5,row6,row7,row8,row9)
write.table(pm, "/Users/Wenjin/Desktop/>INFO7390 DataScience/Assignments/Assignment 2/outputs/PredictionPerformanceMetrics.csv", sep = ",", row.names = F, col.names = F)

## Forecast new data using cleaned data : forecastNewData.xlsx and forecastNewData2.csv
forecast1<-read.csv("/Users/Wenjin/Desktop/>INFO7390 DataScience/Assignments/Assignment 2/ling-Forecast-Part1-a/ready-forecast-data1.csv",header=T)
forecast2<-read.csv("/Users/Wenjin/Desktop/>INFO7390 DataScience/Assignments/Assignment 2/ling-Forecast-Part1-a/ready-forecast-data2.csv",header=T)
# Regression tree
tree.forecast1.res <- cbind(forecast1, kWh = predict(tree.mydata,forecast1))
tree.forecast1.res = tree.forecast1.res[,-2:-4]
tree.forecast1.res = tree.forecast1.res[,-3:-5]
write.csv(tree.forecast1.res, "/Users/Wenjin/Desktop/>INFO7390 DataScience/Assignments/Assignment 2/outputs/forecastOutput_26435791004_regressionTree.csv", row.names = F)
tree.forecast2.res <- cbind(forecast2, kWh = predict(tree.mydata,forecast2))
tree.forecast2.res = tree.forecast2.res[,-2:-4]
tree.forecast2.res = tree.forecast2.res[,-3:-5]
write.csv(tree.forecast2.res, "/Users/Wenjin/Desktop/>INFO7390 DataScience/Assignments/Assignment 2/outputs/forecastOutput2_26435791004_regressionTree.csv", row.names = F)
# Neural network
forecastData1 <- forecast1[,-4]
forecastData1 <- forecastData1[,-1]
forecastData2 <- forecast2[,-4]
forecastData2 <- forecastData2[,-1]
net.forecast1.res <- cbind(forecast1, kWh = compute(net.mydata,forecastData1))
net.forecast1.res = net.forecast1.res[,-2:-4]
net.forecast1.res = net.forecast1.res[,-3:-5]
net.forecast1.res = net.forecast1.res[,-4:-11]
write.csv(net.forecast1.res, "/Users/Wenjin/Desktop/>INFO7390 DataScience/Assignments/Assignment 2/outputs/forecastOutput_26435791004_neuralNetwork.csv", row.names = F)
net.forecast2.res <- cbind(forecast2, kWh = compute(net.mydata,forecastData2))
net.forecast2.res = net.forecast2.res[,-2:-4]
net.forecast2.res = net.forecast2.res[,-3:-5]
net.forecast2.res = net.forecast2.res[,-4:-11]
write.csv(net.forecast2.res, "/Users/Wenjin/Desktop/>INFO7390 DataScience/Assignments/Assignment 2/outputs/forecastOutput2_26435791004_neuralNetwork.csv", row.names = F)
