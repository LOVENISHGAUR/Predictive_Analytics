##R-Code for East West Airlines
##===============================

install.packages("neuralnet")
install.packages("grid")
install.packages("MASS")

install.packages("nnet")
install.packages("caret")

#1. Preliminary Work
====================

getwd()

#Read in data
default = read.csv("EastWestAirlinesNN.csv")
str(default)
summary(default)
default = default[,-1] #drop ID Variable
str(default)

default$Topflight = as.factor(default$Topflight)
default$cc1_miles. = as.factor(default$cc1_miles.)
default$cc2_miles. = as.factor(default$cc2_miles.)
default$cc3_miles. = as.factor(default$cc3_miles.)
default$Email = as.factor(default$Email)
default$Balance
default$Qual_miles
default$Bonus_miles
default$Bonus_trans
default$Flight_miles_12mo
default$Flight_trans_12
default$Online_12
default$Club_member
default$Any_cc_miles_12mo
default$Phone_sale

#Check for missing data
apply(default,2,function(x) sum(is.na(x)))

#Converting qualitative variables into binary dummies
default1 = model.matrix(~ ., data=default[,c(1,4:6,12)])
str(as.data.frame(default1))
default = as.data.frame(cbind(default1, default))
str(default)
default = default[,-c(7,10:12,18)]

#Scale data / Normalize the data
maxs = apply(default, 2, max)
mins = apply(default, 2, min)
scaled = as.data.frame(scale(default, center = mins, scale = maxs - mins))
str(scaled)

#Partioning the Dataset
library(caTools)
set.seed(1000)
split = sample.split(scaled$Phone_sale, SplitRatio = 0.70)
train = subset(scaled, split==TRUE)
test = subset(scaled, split==FALSE)
str(train)
str(test)

#Setting the Baseline
table(train$Phone_sale) 
baseline = 3031/(3031+458) #86.87%

#2. Neural Network Modeling (Package 1)
=========================================
library(neuralnet)
library(grid)
library(MASS)
library(caret)
str(train)
train = train[,-1]

n = names(train)
n
f = Phone_sale ~ Topflight1 + cc1_miles.1 + cc2_miles.1 + cc3_miles.1 + 
  Email1 + Balance + Qual_miles + Bonus_miles + Bonus_trans + 
  Flight_miles_12mo + Flight_trans_12 + Online_12 + Club_member + 
  Any_cc_miles_12mo # IN NEURAL NET WE NEED TO SPECIFY EACH VARIABLE

  dtrain.nn = neuralnet(f , data=train, hidden=(5,3), threshold = 0.01, linear.output=FALSE) #linear.output = False for classication and true for regression
summary(train.nn)
train.nn$result.matrix #summary of results: matrix containing the error, reached threshold, needed steps, AIC and BIC and estimated weights for each replication. 
plot(train.nn)
str(train)

predict.train.nn = compute(train.nn, train[,1:14])
table(train$Phone_sale, predict.train.nn$net.result>0.50)
(3030+5)/(3030+5+1+453) #86.98% for hidden nodes=1
(3001+35)/(3001+35+30+423)#87 for hidden nodes=2
(3013+57)/(3013+18+401+57)#87.99 for hidden nodes=5
str(test)
predict.test.nn = compute(train.nn, test[,2:15])
table(test$Phone_sale, predict.test.nn$net.result>0.50)
(1298+1)/(1298+1+196+1) #86.83% for hidden nodes=1
(1278+16)/(1278+21+181+16)#86 for hidden nodes=2
(1262+37)/(1262+37+179+18)#86.83 for hidden nodes=5


#3. Neural Network Modeling (Package 2)
========================================
library(caret)
library(nnet)

str(default)

train$Phone_sale = as.factor(train$Phone_sale)
test$Phone_sale = as.factor(test$Phone_sale)

train.nnet = nnet(Phone_sale ~ . , data=train, size =20, maxit=2000, Trace=T)
nnet
str(train.nnet)
summary(train.nnet)
topmodel = varImp(train.nnet) #Finding important predictors
head(topmodel,30)

predict.train.nnet = predict(train.nnet, data=train)
table(train$Phone_sale, predict.train.nnet>0.50)
(3030+12)/(3030+12+1+446) #88.93%

predict.test.nnet = predict(train.nnet, newdata=test, method="class", na.rm=TRUE)
table(test$Phone_sale, predict.test.nnet>0.50)
(1257+14)/(1257+14+42+183) #84.95%


#4. Plotting ROC Curve and Calculating AUC
library(ROCR)
predictROC = predict(train.nnet, newdata = test)
predictROC
pred = prediction(predictROC, test$Phone_sale)
perf = performance(pred, "tpr", "fpr")
perf
plot(perf, col=rainbow(10), lty=2)
as.numeric(performance(pred, "auc")@y.values)
