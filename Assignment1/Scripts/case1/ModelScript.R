library(MASS)
library(ISLR)
ready <- read.csv("D:/R files/ready-data1.csv")
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
write.table(list, "D:/R files/RegressionOutputs1.csv", sep = ",", row.names = F, col.names = F)

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

write.table(pm, "D:/R files/MetricsOutput1.csv", sep = ",", row.names = F, col.names = F)

forecast1 <- read.csv("D:/R files/ready-forcast-data.csv")
ready <- read.csv("D:/R files/ready-data.csv")
ready$logkWh <- log(ready$kWh)

model = lm(logkWh ~ log(month) + day + hour + DayofWeek 
           + weekdays + peakhour + log(temperature),
           data = ready)
forecast1.res <- cbind(forecast1, kWh = exp(1)^predict(model,forecast1))
forecast1.res = forecast1.res[,-2:-4]
forecast1.res = forecast1.res[,-3:-5]
write.csv(forecast1.res, "D:/R files/forecastOutput2.csv", row.names = F)
