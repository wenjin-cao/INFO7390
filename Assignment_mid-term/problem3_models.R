
library(MASS)
library(ISLR)
#farm1
readydata <- read.csv("D:/R files/train/wf1-ready-train.csv")
newdata <- read.csv("D:/R files/train/wf1-ready-evaluation.csv")
newdata <- cbind(newdata,logwd = log(newdata[,5]))

for(i in nrow(readydata):1){
  if(is.na(readydata[i,6])){
    readydata <- readydata[-i,]
  }
}

#r sqr = 0.51
colnames(readydata)
lmodel <- lm(wp~ u+v+ws+wd, readydata)
summary(lmodel)

readydata <- cbind(readydata,logwd = log(readydata[,5]))

lmodel <- lm(wp~ u+v+ws+logwd, readydata)
summary(lmodel)

library(grid)
library(neuralnet)
mydata <- readydata
mydata <- mydata[,-1]
mydata <- mydata[,-4]
smp_size <- floor(0.75 * nrow(mydata)) #75% of the sample size
set.seed(123) #Set the seed to make your partition reproductible
train_ind <- sample(seq_len(nrow(mydata)), size = smp_size)
train <- mydata[train_ind, ]
test <- mydata[-train_ind, ]

nmodel <- neuralnet(wp~ u+v+ws+logwd, train, hidden = 0)

library(tree)
mydata <- readydata
training = sample(1:nrow(mydata),nrow(mydata)/2)
testing = mydata[-training,]

# prune will reduce the accuracy
tmodel <- tree(wp~ u+v+ws+logwd, mydata, subset = training)
cv.tree <- cv.tree(tmodel)
plot(tmodel)
prune.tmodel <- prune.tree(tmodel, best = 5)
plot(prune.tmodel)

#the tree model is better than others, we decide to use tree model to get predict values.
wp1 <- predict(tmodel, newdata = newdata)
date1 <- newdata[,1]
#calculate the accuracy metrics and generate csv files.
write.csv(t(accuracy(testing$wp,predict(tmodel, newdata = testing))),"D:/R files/train/treeM.csv")

library(forecast)
write.csv(t(accuracy(lmodel)),"D:/R files/train/lmM.csv")
n.results <- compute(nmodel, test[,-5])
write.csv(t(accuracy(test$wp, n.results$net.result)),"D:/R files/train/nnM.csv")


#farm2
readydata <- read.csv("D:/R files/train/wf2-ready-train.csv")
newdata <- read.csv("D:/R files/train/wf2-ready-evaluation.csv")
newdata <- cbind(newdata,logwd = log(newdata[,5]))
date2 <- newdata[,1]
for(i in nrow(readydata):1){
  if(is.na(readydata[i,6])){
    readydata <- readydata[-i,]
  }
}

lmodel <- lm(wp~., data = readydata)
summary(lmodel)
#r sqr = 0.51

colnames(readydata)
lmodel <- lm(wp~ u+v+ws+wd, readydata)
#r sqr = 0.50

summary(lmodel)
readydata <- cbind(readydata,logwd = log(readydata[,5]))

lmodel <- lm(wp~ u+v+ws+logwd, readydata)
summary(lmodel)

library(grid)
library(neuralnet)
mydata <- readydata
mydata <- mydata[,-1]
mydata <- mydata[,-4]

nmodel <- neuralnet(wp~ u+v+ws+logwd, mydata, hidden = 0)

library(tree)
mydata <- readydata

# prune will reduce the accuracy
tmodel <- tree(wp~ u+v+ws+logwd, mydata)
cv.tree <- cv.tree(tmodel)
plot(tmodel)
prune.tmodel <- prune.tree(tmodel, best = 5)
plot(prune.tmodel)

#the tree model is better than others, we decide to use tree model to get predict values.
wp2 <- predict(tmodel, newdata = newdata)

#farm3
readydata <- read.csv("D:/R files/train/wf4-ready-train.csv")
newdata <- read.csv("D:/R files/train/wf4-ready-evaluation.csv")
newdata <- cbind(newdata,logwd = log(newdata[,5]))
date3 <- newdata[,1]
for(i in nrow(readydata):1){
  if(is.na(readydata[i,6])){
    readydata <- readydata[-i,]
  }
}

lmodel <- lm(wp~., data = readydata)
summary(lmodel)
#r sqr = 0.51

colnames(readydata)
lmodel <- lm(wp~ u+v+ws+wd, readydata)
#r sqr = 0.50

summary(lmodel)
readydata <- cbind(readydata,logwd = log(readydata[,5]))

lmodel <- lm(wp~ u+v+ws+logwd, readydata)
summary(lmodel)

library(grid)
library(neuralnet)
mydata <- readydata
mydata <- mydata[,-1]
mydata <- mydata[,-4]


nmodel <- neuralnet(wp~ u+v+ws+logwd, mydata, hidden = 0)

library(tree)
mydata <- readydata

# prune will reduce the accuracy
tmodel <- tree(wp~ u+v+ws+logwd, mydata)
cv.tree <- cv.tree(tmodel)
plot(tmodel)
prune.tmodel <- prune.tree(tmodel, best = 5)
plot(prune.tmodel)

#the tree model is better than others, we decide to use tree model to get predict values.
wp3 <- predict(tmodel, newdata = newdata)

#farm4
readydata <- read.csv("D:/R files/train/wf4-ready-train.csv")
newdata <- read.csv("D:/R files/train/wf4-ready-evaluation.csv")
newdata <- cbind(newdata,logwd = log(newdata[,5]))
date4 <- newdata[,1]
for(i in nrow(readydata):1){
  if(is.na(readydata[i,6])){
    readydata <- readydata[-i,]
  }
}

lmodel <- lm(wp~., data = readydata)
summary(lmodel)
#r sqr = 0.51

colnames(readydata)
lmodel <- lm(wp~ u+v+ws+wd, readydata)
#r sqr = 0.50

summary(lmodel)
readydata <- cbind(readydata,logwd = log(readydata[,5]))

lmodel <- lm(wp~ u+v+ws+logwd, readydata)
summary(lmodel)

library(grid)
library(neuralnet)
mydata <- readydata
mydata <- mydata[,-1]
mydata <- mydata[,-4]

nmodel <- neuralnet(wp~ u+v+ws+logwd, mydata, hidden = 0)

library(tree)
mydata <- readydata
# prune will reduce the accuracy
tmodel <- tree(wp~ u+v+ws+logwd, mydata)
cv.tree <- cv.tree(tmodel)
plot(tmodel)
prune.tmodel <- prune.tree(tmodel, best = 5)
plot(prune.tmodel)

#the tree model is better than others, we decide to use tree model to get predict values.
wp4 <- predict(tmodel, newdata = newdata)

#farm5
readydata <- read.csv("D:/R files/train/wf5-ready-train.csv")
newdata <- read.csv("D:/R files/train/wf5-ready-evaluation.csv")
newdata <- cbind(newdata,logwd = log(newdata[,5]))
date5 <- newdata[,1]
for(i in nrow(readydata):1){
  if(is.na(readydata[i,6])){
    readydata <- readydata[-i,]
  }
}

lmodel <- lm(wp~., data = readydata)
summary(lmodel)
#r sqr = 0.51

colnames(readydata)
lmodel <- lm(wp~ u+v+ws+wd, readydata)
#r sqr = 0.50

summary(lmodel)
readydata <- cbind(readydata,logwd = log(readydata[,5]))

lmodel <- lm(wp~ u+v+ws+logwd, readydata)
summary(lmodel)

library(grid)
library(neuralnet)
mydata <- readydata
mydata <- mydata[,-1]
mydata <- mydata[,-4]
smp_size <- floor(0.75 * nrow(mydata)) #75% of the sample size
set.seed(123) #Set the seed to make your partition reproductible
train_ind <- sample(seq_len(nrow(mydata)), size = smp_size)
train <- mydata[train_ind, ]
test <- mydata[-train_ind, ]

nmodel <- neuralnet(wp~ u+v+ws+logwd, train, hidden = 0)

library(tree)
mydata <- readydata
training = sample(1:nrow(mydata),nrow(mydata)/2)
testing = mydata[-training,]

# prune will reduce the accuracy
tmodel <- tree(wp~ u+v+ws+logwd, mydata, subset = training)
cv.tree <- cv.tree(tmodel)
plot(tmodel)
prune.tmodel <- prune.tree(tmodel, best = 5)
plot(prune.tmodel)

#the tree model is better than others, we decide to use tree model to get predict values.
wp5 <- predict(tmodel, newdata = newdata)

#farm6
readydata <- read.csv("D:/R files/train/wf6-ready-train.csv")
newdata <- read.csv("D:/R files/train/wf6-ready-evaluation.csv")
newdata <- cbind(newdata,logwd = log(newdata[,5]))
date6 <- newdata[,1]
for(i in nrow(readydata):1){
  if(is.na(readydata[i,6])){
    readydata <- readydata[-i,]
  }
}

lmodel <- lm(wp~., data = readydata)
summary(lmodel)
#r sqr = 0.51

colnames(readydata)
lmodel <- lm(wp~ u+v+ws+wd, readydata)
#r sqr = 0.50

summary(lmodel)
readydata <- cbind(readydata,logwd = log(readydata[,5]))

lmodel <- lm(wp~ u+v+ws+logwd, readydata)
summary(lmodel)

library(grid)
library(neuralnet)
mydata <- readydata
mydata <- mydata[,-1]
mydata <- mydata[,-4]

nmodel <- neuralnet(wp~ u+v+ws+logwd, mydata, hidden = 0)

library(tree)
mydata <- readydata
# prune will reduce the accuracy
tmodel <- tree(wp~ u+v+ws+logwd, mydata)
cv.tree <- cv.tree(tmodel)
plot(tmodel)
prune.tmodel <- prune.tree(tmodel, best = 5)
plot(prune.tmodel)

#the tree model is better than others, we decide to use tree model to get predict values.
wp6 <- predict(tmodel, newdata = newdata)

#farm7
readydata <- read.csv("D:/R files/train/wf7-ready-train.csv")
newdata <- read.csv("D:/R files/train/wf7-ready-evaluation.csv")
newdata <- cbind(newdata,logwd = log(newdata[,5]))
date7 <- newdata[,1]
for(i in nrow(readydata):1){
  if(is.na(readydata[i,6])){
    readydata <- readydata[-i,]
  }
}

lmodel <- lm(wp~., data = readydata)
summary(lmodel)
#r sqr = 0.51

colnames(readydata)
lmodel <- lm(wp~ u+v+ws+wd, readydata)
#r sqr = 0.50

summary(lmodel)
readydata <- cbind(readydata,logwd = log(readydata[,5]))

lmodel <- lm(wp~ u+v+ws+logwd, readydata)
summary(lmodel)

library(grid)
library(neuralnet)
mydata <- readydata
mydata <- mydata[,-1]
mydata <- mydata[,-4]

nmodel <- neuralnet(wp~ u+v+ws+logwd, mydata, hidden = 0)

library(tree)
mydata <- readydata

# prune will reduce the accuracy
tmodel <- tree(wp~ u+v+ws+logwd, mydata)
cv.tree <- cv.tree(tmodel)
plot(tmodel)
prune.tmodel <- prune.tree(tmodel, best = 5)
plot(prune.tmodel)

#the tree model is better than others, we decide to use tree model to get predict values.
wp7 <- predict(tmodel, newdata = newdata)

bind = c(NA)
date = date1
test2 = cbind(date1,bind)
id <- c(1:length(date))
result <- cbind(id, date, wp1,wp2,wp3,wp4,wp5,wp6,wp7)
write.csv(result, "D:/R files/train/result.csv", row.names = F)
