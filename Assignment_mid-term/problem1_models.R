mydata <- read.csv("/Users/Wenjin/Desktop/>INFO7390_DataScience/Assignments/Mid Term/Mid-Term-ling/problem1/p1-ready-data.csv")
attach(mydata)

Y_class = ifelse(Y>0,"Credible","Non-credible")
Y_class = as.factor(Y_class) #converted from numeric variable to factor variable


## Logistic regression 
# create training and validation data from given data
# use the training data to train the model and the testing data to validate model
library(caTools)
set.seed(88)
split <- sample.split(mydata$Y, SplitRatio = 0.75)
# get training and test data
lgTraining <- subset(mydata, split == TRUE)
lgTesting <- subset(mydata, split == FALSE)
ff = Y~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20 + X21 + X22 + X23
lg = glm(Y~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20 + X21 + X22 + X23,family=binomial(link='logit'),lgTraining)
summary(lg)
library(glmnet)
mat = model.matrix(Y~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20 + X21 + X22 + X23,model.frame(ff,mydata))
lg2 = glmnet(model.matrix(Y~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20 + X21 + X22 + X23,model.frame(ff,mydata)),Y)
summary(lg2)
#lg.pred = predict(lg,type = 'response')
lg.pred = predict(lg,lgTesting)
# confusion matrix
lg.table = table(lgTesting$Y,lg.pred>0.5)
lg.table
lg.err = round((lg.table[2,1]+lg.table[1,2])/(lg.table[1,1]+lg.table[1,2]+lg.table[2,1]+lg.table[2,2])*100,digits = 2)
lg.err
# ROC Curve
library(ROCR)
lgPred <- prediction(lg.pred, lgTesting$Y)
lgROCRperf <- performance(lgPred, 'tpr','fpr')
plot(lgROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
# lift chart
lgPerf <- performance(lgPred,"lift","rpp")
plot(lgPerf, main="lift curve", colorize=T)


## Neural net
library(neuralnet)
library(nnet)
set.seed(9998) #Set the seed to make your partition reproductible
smp_size <- floor(0.75 * nrow(mydata)) #75% of the sample size
train_ind <- sample(seq_len(nrow(mydata)), size = smp_size)
train <- mydata[train_ind, ]
test <- mydata[-train_ind, ]
trainY_class = ifelse(train$Y>0,"Credible","Non-credible")
trainY_class = as.factor(trainY_class)
nn = nnet(trainY_class~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20 + X21 + X22 + X23,train,size=4)
summary(nn)
net.pred = predict(nn,test,type = "class")
testY_class = ifelse(test$Y>0,"Credible","Non-credible")
testY_class = as.factor(testY_class)
net.table = table(net.pred,testY_class)
net.table
#Note: Most of the time, the predicted entries are all 'Non-credible' and no 'Credible'
#net.err = round((net.table[2,1]+net.table[1,2])/(net.table[1,1]+net.table[1,2]+net.table[2,1]+net.table[2,2])*100,digits = 2)
#net.err
net.err_ = round((net.table[1,1])/(net.table[1,1]+net.table[1,2])*100,digits = 2)
net.err_
# ROC Curve
library(ROCR)
#testY_class = ifelse(test$Y>0,1,0)
netPred <- prediction(predict(nn,test,type = "raw"), testY_class)
netROCRperf <- performance(netPred, 'tpr','fpr')
plot(netROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
# lift chart
netPerf <- performance(netPred,"lift","rpp")
plot(netPerf, main="lift curve", colorize=T)


## Classification tree
library(tree)
set.seed(2)
tree.train = sample(1:nrow(mydata),22500)
tree.test = mydata[-tree.train,]
Y_class.test = Y_class[-tree.train]
tree = tree(Y_class~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20 + X21 + X22 + X23,mydata,subset = tree.train)
summary(tree)
tree.pred = predict(tree,tree.test,type="class")
tree.table = table(tree.pred,Y_class.test) 
tree.table
#tree.pred = predict(tree,mydata,type="class")
#tree.table = table(tree.pred,Y_class) 
tree.err = round((tree.table[2,1]+tree.table[1,2])/(tree.table[1,1]+tree.table[1,2]+tree.table[2,1]+tree.table[2,2])*100,digits = 2)
tree.err
# ROC Curve
library(ROCR)
Y_class = ifelse(Y>0,1,0)
mytree = tree(Y_class~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20 + X21 + X22 + X23,mydata)
treePred <- prediction(predict(mytree), mydata$Y)
treeROCRperf <- performance(treePred, 'tpr','fpr')
plot(treeROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
# lift chart
treePerf <- performance(treePred,"lift","rpp")
plot(treePerf, main="lift curve", colorize=T)


## Output
row1 = c("Performance Matrics","","")
row2 = c("Logisctic Regression","","")
row3 = c("Overall Error",paste(lg.err,"%"),"")
row4 = c("","Credible","Non-credible")  
row5 = c("Credible",lg.table[1,1],lg.table[1,2])
row6 = c("Non-credible",lg.table[2,1],lg.table[2,2])
row7 = c("","","")
row8 = c("Neural network","","")
#row9 = c("Overall Error",paste(net.err,"%"),"")
row9_ = c("Overall Error",paste(net.err_,"%"),"")
row10 = c("","Credible","Non-credible")  
#row11 = c("Credible",net.table[1,1],net.table[1,2])
#row12 = c("Non-credible",net.table[2,1],net.table[2,2])
row11_ = c("Credible","0","0")
row12_ = c("Non-credible",net.table[1,1],net.table[1,2])
row13 = c("","","")
row14 = c("Classification tree","","")
row15 = c("Overall Error",paste(tree.err,"%"),"")
row16 = c("","Credible","Non-credible")  
row17 = c("Credible",tree.table[1,1],tree.table[1,2])
row18 = c("Non-credible",tree.table[2,1],tree.table[2,2])
#results = rbind(row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,row11,row12,row13,row14,row15,row16,row17,row18)
results_ = rbind(row1,row2,row3,row4,row5,row6,row7,row8,row9_,row10,row11_,row12_,row13,row14,row15,row16,row17,row18)
#write.table(results, "/Users/Wenjin/Desktop/>INFO7390_DataScience/Assignments/Mid Term/outputs_problem1.csv", sep = ",", row.names = F, col.names = F)
write.table(results_, "/Users/Wenjin/Desktop/>INFO7390_DataScience/Assignments/Mid Term/outputs_problem1.csv", sep = ",", row.names = F, col.names = F)

