mydata <- read.csv("/Users/Wenjin/Desktop/>INFO7390_DataScience/Assignments/Mid Term/Mid-Term-ling/problem2/p2-data-FillData.csv")
attach(mydata)

Y = V1559
Y = as.numeric(Y)
Y_class = ifelse(Y > 0,"ad","nonad") 

## Logistic regression 
# create training and validation data from given data
# use the training data to train the model and the testing data to validate model
library(caTools)
set.seed(88)
split <- sample.split(V1559, SplitRatio = 0.6)
# get training and test data
lgTraining <- subset(mydata, split == TRUE)
lgTesting <- subset(mydata, split == FALSE)
lg = glm(V1559~.,family=binomial(link='logit'),lgTraining)
summary(lg)
library(glmnet)
ff = V1559~.
mat = model.matrix(ff,model.frame(ff,mydata))
lg2 = glmnet(model.matrix(ff,model.frame(ff,mydata)),V1559)
summary(lg2)
lg.pred = predict(lg,lgTesting)
# confusion matrix
lg.table = table(lgTesting$V1559,lg.pred>0.5)
lg.table
lg.err = round((lg.table[2,1]+lg.table[1,2])/(lg.table[1,1]+lg.table[1,2]+lg.table[2,1]+lg.table[2,2])*100,digits = 2)
lg.err
# ROC Curve
library(ROCR)
lgPred <- prediction(lg.pred, lgTesting$V1559)
lgROCRperf <- performance(lgPred, 'tpr','fpr')
plot(lgROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
# lift chart
lgPerf <- performance(lgPred,"lift","rpp")
plot(lgPerf, main="lift curve", colorize=T)


## Neural net
library(neuralnet)
library(nnet)
set.seed(555) #Set the seed to make your partition reproductible
smp_size <- floor(0.6 * nrow(mydata)) #60% of the sample size
train_ind <- sample(seq_len(nrow(mydata)), size = smp_size)
train <- mydata[train_ind, ]
test <- mydata[-train_ind, ]
trainY = as.numeric(train$V1559)
trainY_class = ifelse(trainY==1,"ad","nonad")
#trainY_class = ifelse(V1559>0,"ad","nonad")
trainY_class = as.factor(trainY_class)
nn = nnet(trainY_class~.,train,size=1,MaxNWts=6500)
summary(nn)
net.pred = predict(nn,test,type = "class")
testY_class = ifelse(test$V1559>0,"ad","nonad")
testY_class = as.factor(testY_class)
net.table = table(net.pred,testY_class)
net.table
net.err = round((net.table[1,2]+net.table[2,1])/(net.table[1,1]+net.table[1,2]+net.table[2,1]+net.table[2,2])*100,digits = 2)
net.err
#net.err_ = round((net.table[1,1])/(net.table[1,1]+net.table[1,2])*100,digits = 2)
#net.err_
# ROC Curve
library(ROCR)
netPred <- prediction(predict(nn,test,type = "raw"),testY_class)
netROCRperf <- performance(netPred, 'tpr','fpr')
plot(netROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
# lift chart
netPerf <- performance(netPred,"lift","rpp")
plot(netPerf, main="lift curve", colorize=T)


## Classification tree
library(tree)
set.seed(2)
tree.train = sample(1:nrow(mydata),2000)
tree.test = mydata[-tree.train,]
Y_class.test = Y_class[-tree.train]
V1559_class = ifelse(V1559 > 0,"ad","nonad")
V1559_class = as.factor(V1559_class)
tree = tree(V1559_class~.,mydata,subset = tree.train)
summary(tree)
mytree = tree(V1559~.,mydata)
tree.pred = predict(tree,tree.test,type="class")
tree.table = table(tree.pred,Y_class.test) 
tree.table 
#tree.pred = predict(tree,mydata,type="class")
#tree.table = table(tree.pred,Y_class) 
tree.err = round((tree.table[2,1]+tree.table[1,2])/(tree.table[1,1]+tree.table[1,2]+tree.table[2,1]+tree.table[2,2])*100,digits = 2)
tree.err
# ROC Curve
library(ROCR)
#The predictions need to be continuous for ROCR's prediction function. 
treePred <- prediction(predict(mytree),V1559)
treeROCRperf <- performance(treePred, 'tpr','fpr')
plot(treeROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
# lift chart
treePerf <- performance(treePred,"lift","rpp")
plot(treePerf, main="lift curve", colorize=T)


## Output
row1 = c("Performance Matrics","","")
row2 = c("Logisctic Regression","","")
row3 = c("Overall Error",paste(lg.err,"%"),"")
row4 = c("","Ad","Non-ad")  
row5 = c("Ad",lg.table[1,1],lg.table[1,2])
row6 = c("Non-Ad",lg.table[2,1],lg.table[2,2])
row7 = c("","","")
row8 = c("Neural network","","")
row9 = c("Overall Error",paste(net.err,"%"),"")
#row9_ = c("Overall Error",paste(net.err_,"%"),"")
row10 = c("","Ad","Non-Ad")  
#row11_ = c("Ad","0","0")
#row12_ = c("Non-Ad",net.table[1,1],net.table[1,2])
row11 = c("Ad",net.table[1,1],net.table[1,2])
row12 = c("Non-Ad",net.table[2,1],net.table[2,2])
row13 = c("","","")
row14 = c("Classification tree","","")
row15 = c("Overall Error",paste(tree.err,"%"),"")
row16 = c("","Ad","Non-Ad")  
row17 = c("Ad",tree.table[1,1],tree.table[1,2])
row18 = c("Non-Ad",tree.table[2,1],tree.table[2,2])
results = rbind(row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,row11,row12,row13,row14,row15,row16,row17,row18)
write.table(results, "/Users/Wenjin/Desktop/>INFO7390_DataScience/Assignments/Mid Term/outputs_problem2.csv", sep = ",", row.names = F, col.names = F)

