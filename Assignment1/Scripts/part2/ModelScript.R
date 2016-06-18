
library(MASS)
library(ISLR)
ready <- read.csv("D:/R files/ready-data2.csv")
ready$logkWh <- log(ready$kWh)
#75% of the sample size
smp_size <- floor(0.75 * nrow(ready))

#Set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(ready)), size = smp_size)

#Split the data into training and testing
train <- ready[train_ind, ]
test <- ready[-train_ind, ]

#Fit a linear regression model 
lm.fit = lm(logkWh ~ log(month) + day + hour + DayofWeek + weekdays + peakhour + log(temperature),data = ready)

#Summary of the fit
summary(lm.fit)

#Measures of predictive accuracy
library(forecast)
pred = predict(lm.fit, test)
acc <- t(accuracy(pred, train$logkWh))
rowlist = rownames(acc)
acc = cbind(rowlist,acc)

#Output coefficients
list  = coefficients(model)
namelist = names(list)
list = cbind(namelist,list)
list[1,1] = "constant"
row1= c("Account No.","26435791004")
list = rbind(row1, list)
list[2,2] <- round(as.numeric(list[2,2]) , digits = 2)
list[3,2] <- round(as.numeric(list[3,2]), digits = 2)
list[4,2] <- round(as.numeric(list[4,2]), digits = 2)
list[5,2] <- round(as.numeric(list[5,2]), digits = 2)
list[6,2] <- round(as.numeric(list[6,2]), digits = 2)
list[7,2] <- round(as.numeric(list[7,2]), digits = 2)
list[8,2] <- round(as.numeric(list[8,2]), digits = 2)
list[9,2] <- round(as.numeric(list[9,2]), digits = 2)
write.table(list, "D:/R files/RegressionOutputs2.csv", sep = ",", row.names = F, col.names = F)

account = row.names(acc)
number = acc[,2]
pm = cbind(account,number)
row2 = pm[3,]
row3 = pm[5,]
row4 = c("RMS",pm[2,2])
pm = rbind(row1,row2,row3,row4)

pm[2,2] <- round(as.numeric(pm[2,2]) , digits = 2)
pm[3,2] <- paste(round(as.numeric(pm[3,2]), digits = 2),"%")
pm[4,2] <- round(as.numeric(pm[4,2]), digits = 2)

write.table(pm, "D:/R files/MetricsOutput2.csv", sep = ",", row.names = F, col.names = F)



#Read csv files
forecast <- read.csv("D:/R files/ready-forcast-new-data.csv")
names(forecast)[names(forecast) == "V2"] = "hour"

forecast2 <- read.csv("D:/R files/ready-forecast-data2.csv")
names(forecast2)[names(forecast2) == "Hour"] = "hour"
#Build Model

ready2 <- read.csv("D:/R files/ready-data2.csv")

ready2$logkWh <- log(ready2$kWh)
model2 = lm(logkWh ~ log(month) + day + hour + DayofWeek 
            + weekdays + peakhour + log(temperature),
            data = ready2)

#Use model to get predict data
forecast.res <- cbind(forecast, kWh = exp(1)^predict(model2,forecast))
forecast2.res <- cbind(forecast2, kWh = exp(1)^predict(model2,forecast2))

#Formalize
forecast.res = forecast.res[,-2:-4]
forecast.res = forecast.res[,-3:-5]
forecast2.res = forecast2.res[,-2:-4]
forecast2.res = forecast2.res[,-3:-5]


#Write the results into new files
write.csv(forecast.res, "D:/R files/forecastOutput1.csv", row.names = F)
write.csv(forecast2.res, "D:/R files/forecastOutput3.csv", row.names = F)

