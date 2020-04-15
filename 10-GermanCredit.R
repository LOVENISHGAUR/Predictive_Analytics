R-Code_for_Credit_Card_default 
-------------------------------------------
install.packages("adabag")

#1. Preliminary Work
====================

getwd()
setwd("/Users/lovenishgaur/Documents/Jantahack/")
list.files()
# Read in data
creditdata = read.csv("train_data.csv")
str(creditdata)
table(creditdata$default_payment_next_month)

#Converting categorical variables into factors
names = c(3,4,5,7,8,9,10,11,12)
creditdata[,names] = lapply(creditdata[,names], as.factor)
str(creditdata)
 
#Converting qualitative variables into binary dummies
default1 = model.matrix(~ ., data=creditdata[,c(3:5,7:12)])
str(as.data.frame(default1))
default = as.data.frame(cbind(default1, creditdata))
str(default)
default = default[,-c(71:73,75:80)]
str(default)


#Setting the baseline
table(default$default_payment_next_month)
baseline = 4645/(16355+4645) #22.11%
baseline

#Partitioning the dataset

library(caTools)
set.seed(1000)
split<- sample.split(default$default_payment_next_month,SplitRatio=0.7)
train= subset(default,split==TRUE)
valid= subset(default,split==FALSE)
str(train) #14700 observantion of 84 variables
str(valid) #6300 Observations of 84 variables


#2.Model
=====================================
  
#Logistics Regression
train.log <- glm(default_payment_next_month ~., data = train, family = "binomial")
options(scipen = 999)
summary(train.log)
train.log$fitted.values

predictTrain.log = predict(train.log, type="response", data=train)
predictTrain.log
table(train$default_payment_next_month, predictTrain.log>0.5)
(10908+1187)/(10908+1187+540+2065) #82.27%

predictValid.log = predict(train.log, type="response", newdata = valid)
table(valid$default_payment_next_month, predictValid.log>0.5)
(4681+486)/(4681+226+486+907) #82.01%

library(ROCR)
ROCRpred = prediction(predictValid.log, valid$default_payment_next_month)
ROCRperf = performance(ROCRpred, "tpr","fpr")
ROCRperf
plot(ROCRperf, col="black", lty=2, lwd=1)
plot(ROCRperf, col=rainbow(1))
as.numeric(performance(ROCRpred, "auc")@y.values)


#Classification Trees
library(rpart)
library(rpart.plot)

train.ct = rpart(RESPONSE1 ~ ., data=train, method ="class", control = rpart.control(minbucket=1))
prp(train.ct)

predictTrain.ct = predict(train.ct, data = train, type="class")
table(train$RESPONSE1, predictTrain.ct)
(116+465)/(116+465+94+25) #83.00%

predictValid.ct = predict(train.ct, newdata = valid, type="class")
table(valid$RESPONSE1, predictValid.ct)
(35+184)/(35+184+55+26) #73.00%

#prune the tree 
train.ct$cptable[which.min(train.ct$cptable[,"xerror"]),"CP"]
pruned.ct<- prune(train.ct, cp=train.ct$cptable[which.min(train.ct$cptable[,"xerror"]),"CP"])
prp(pruned.ct)

predictTrain.ct.pruned = predict(pruned.ct, data = train, type="class")
table(train$RESPONSE1, predictTrain.ct.pruned)
(94+454)/(116+36+94+454) #78.28%

predictValid.ct = predict(pruned.ct, newdata = valid, type="class")
table(valid$RESPONSE1, predictValid.ct)
(31+180)/(31+180+59+30) #70.33%


#Neural Net Modeling


#Scale data / Normalize the data
maxs = apply(train, 2, max) 
mins = apply(train, 2, min)
scaled = as.data.frame(scale(train, center = mins, scale = maxs - mins))
str(scaled)
train = scaled
str(train)

maxs = apply(valid, 2, max) 
mins = apply(valid, 2, min)
scaled1 <- as.data.frame(scale(valid, center = mins, scale = maxs - mins))
str(scaled1)
valid = scaled1
str(valid)

#neuralnet analysis
library(neuralnet)
library(grid)
library(MASS)
library(caret)
str(default)
train$response = train$RESPONSE1
train = train[,-25]
str(train)

valid$response = valid$RESPONSE1
valid = valid[,-25]
str(valid)

n = names(train)
f = as.formula(paste("response ~", paste(n[!n %in% "response"], collapse = " + ")))
train.nn = neuralnet(f, data=train, hidden=1, threshold = 0.01, linear.output=FALSE) #linear.output = False for classication and true for regression
summary(train.nn)
train.nn$result.matrix #summary of results: matrix containing the error, reached threshold, needed steps, AIC and BIC and estimated weights for each replication. 
plot(train.nn)

predict.train.nn = compute(train.nn, train[,1:45])
table(train$response, predict.train.nn$net.result>0.50)
(149+456)/(149+456+61+34) #86.42%

predict.valid.nn = compute(train.nn, valid[,1:45])
table(valid$response, predict.valid.nn$net.result>0.50)
(45+181)/(45+181+29+45) #75.33%


#3.Improving Predictive Accuracy using Voting based Ensembles
=============================================================
pred.log = as.data.frame(predictValid.log)
pred.log = ifelse(pred.log<0.5,0,1)

pred.ct = as.numeric(predictValid.ct)-1
pred.ct = as.data.frame(pred.ct)

pred.nn = as.data.frame(predict.valid.nn$net.result)
pred.nn = ifelse(pred.nn<0.5,0,1)

ensemble = cbind(actual = valid$response, log = pred.log, ct = pred.ct, nn = pred.nn)  
str(ensemble)  
names(ensemble)[1:4] = c("Actual","Logistic","ClassificationTree","NeuralNet")
ensemble$vote = ifelse((ensemble$Logistic+ensemble$ClassificationTree+ensemble$NeuralNet)>1,1,0)

table(ensemble$Actual,ensemble$vote)
(42+188)/(42+188+48+22) #76.67

table(ensemble$Actual, ensemble$Logistic)
(46+184)/(46+184+44+26) #76.67%

table(ensemble$Actual, ensemble$ClassificationTree)
(31+180)/(31+180+59+30) #70.33%

table(ensemble$Actual, ensemble$NeuralNet)
(45+181)/(45+181+29+45) #75.33%


#4. Improving Predictive Accuracy using Bagging and Boosting (For Tree Models)
==============================================================================
library(adabag)  
library(caTools)
set.seed(1000)
split<- sample.split(creditdata$RESPONSE,SplitRatio=0.7)
train= subset(creditdata,split==TRUE)
valid= subset(creditdata,split==FALSE)
str(train) #700 observantion of 32 variables
str(valid) #300 Observations or 32 variables

#single tree
ensemble.ct = rpart(RESPONSE ~ ., data=train)
pred = predict(ensemble.ct, valid, type="class")
confusionMatrix(pred, valid$RESPONSE) #72.00%

# bagging (Bagged Trees)
bag = bagging(RESPONSE ~ ., data = train)
pred = predict(bag, valid, type = "class")
confusionMatrix(as.factor(pred$class), valid$RESPONSE) #75.67%

# boosting (Boosted Trees)
boost = boosting(RESPONSE ~ ., data = creditdata)
pred = predict(boost, valid, type = "class")
confusionMatrix(as.factor(pred$class), valid$RESPONSE) #100.00%

boost = boosting(RESPONSE ~ ., data = train)
pred = predict(boost, valid, type = "class")
confusionMatrix(as.factor(pred$class), valid$RESPONSE) #75.33%

#5. Improving Predictive Accuracy using Oversampling
====================================================
  
creditdata_0 = subset(creditdata, creditdata$RESPONSE=='0')
str(creditdata_0)
creditdata_1 = subset(creditdata, creditdata$RESPONSE=='1')
str(creditdata_1)

set.seed(1000)
split_0 = sample.split(creditdata_0$RESPONSE, SplitRatio = 0.50)
train = subset(creditdata_0, split_0==TRUE)
str(train)

split_1 = sample.split(creditdata_1$RESPONSE, SplitRatio=0.215)
train1 = subset(creditdata_1, split_1==TRUE)
str(train1)
train = rbind(train, train1)
str(train)

valid = subset(creditdata_0, split_0==FALSE)
str(valid)

valid1 = subset(creditdata_1, split_1==FALSE)
str(valid1)

split_test = sample.split(valid1$RESPONSE, SplitRatio=0.63)
valid1 = subset(valid1, split_test==TRUE)
str(valid1)
valid = rbind(valid, valid1)
str(valid)

str(valid)

# Logistics Regression Model
train.log.model2 <- glm(RESPONSE ~., data = train, family = "binomial")
summary(train.log.model2)

predictTrain = predict(train.log.model2, type="response", data=train)
predictTrain
table(train$RESPONSE, predictTrain>0.5)
(116+121)/(116+121+34+29) #80.66%

predictValid = predict(train.log.model2, type="response", newdata=valid)
predictValid
table(valid$RESPONSE, predictValid >0.5)
(106+228)/(106+228+44+118) #75.60%

library(ROCR)
ROCRpred = prediction(predictValid, valid$RESPONSE)
ROCRperf = performance(ROCRpred, "tpr","fpr")
ROCRperf
plot(ROCRperf, col="black", lty=2, lwd=1)
plot(ROCRperf, col=rainbow(1))
as.numeric(performance(ROCRpred, "auc")@y.values)




