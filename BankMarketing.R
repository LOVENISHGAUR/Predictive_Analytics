##ABOUT THE DATASET###
##The data is related with direct marketing campaigns of a Portuguese banking institution. 
#The marketing campaigns were based on phone calls. Often, more than one contact 
#to the same client was required, in order to access if the product 
#(bank term deposit) would be ('yes') or not ('no') subscribed. 

#Dependent Variable: To predict if client will subscribe to the bank term deposit i.e. yes (1)
#Variables: From 2 to 15 are categorical variables rest are numeric 


#Unbalanced dataset 
install.packages("caTools")
install.packages("ROCR")
install.packages("gains")

install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("caret")
install.packages("ggplot2")
installed.packages("e1071")
getwd()
setwd("/Users/lovenishgaur/Documents/Predictive_Analytics_IIM_Raipur/CourseMaterial")


#1. Loading the dataset
data<- read.csv("bank-additional-full.csv")
str(data)
summary(data)

#Removing missing data
#data = data[data$marital != "unknown",]
#data = data[data$housing != "unknown",]
#data = data[data$loan != "unknown",]

summary(data)


#2. Partioning the dataset into training and testation set
#As there are less number of sunscribed customers than non-subscribed, thus the data is unbalanced.
#While splitting we need to consider the prortion so that both the splitted datasets are
#true replication of the main dataset.

library(caTools)
set.seed(1000)
split<- sample.split(data$y,SplitRatio=0.7)
train= subset(data,split==TRUE)
test= subset(data,split==FALSE)
str(train)
str(test)

table(train$y) 
baseline.train = 25584/(25584+3248) #88.73


#3. Run Classification Trees on Training Data and make Predictions on Test Dataset
library(rpart)
library(rpart.plot)
library(caret)

train.y.ct = rpart(y ~ . -pdays, data=train, method ="class", control = rpart.control(minbucket=1))
prp(train.y.ct)

predictTrain = predict(train.y.ct, data = train, type="class")
confusionMatrix(predictTrain, train$y) #91.29%

predictTest = predict(train.y.ct, newdata = test, type="class")
confusionMatrix(predictTest, test$y) #91.19%

library(ROCR)
predictROC = predict(train.y.ct, newdata = test)
predictROC
pred = prediction(predictROC[,2], test$y)
perf = performance(pred, "tpr", "fpr")
plot(perf, col=rainbow(10), lty=2)
as.numeric(performance(pred, "auc")@y.values) #86.58



#4. Improving Accuracy using Random Forests Modeling
#Make Sure that Outcome Variable is a Factor for conducting Random Forests
library(randomForest)
str(train)
#train$Attrition = as.factor(train$Attrition)
#test$Attrition = as.factor(test$Attrition)

train.Attrition = randomForest(Attrition ~ . -pdays, data=train, nodesize=200, ntree=2000)
train.Attrition

library(caret)
predictTrainForest = predict(train.Attrition, data = train)
confusionMatrix(train$Attrition, predictTrainForest)

predictTestForest = predict(train.Attrition, newdata = test)
confusionMatrix(test$Attrition, predictTestForest)


#5. Improving Accuracy using Cross validation
#============================================

library(caret)
library(e1071)

fitControl = trainControl(method="cv", number=10)
cartGrid = expand.grid(.cp=(1:100)*0.01)
train(y ~ ., data=train, method="rpart", trControl=fitControl, tuneGrid = cartGrid)

train.y.cv = rpart(y ~ ., data=train, method="class", control=rpart.control(cp=0.01))

predictTrain.cv = predict(train.y.cv, data = train, type ="class")
confusionMatrix(train$y, predictTrain.cv)
(24659+1670)/(24659+1670+925+1578) $91.31%

test.y.cv = predict(train.y.cv, newdata = test, type ="class")
confusionMatrix(test$y, test.y.cv) #91.19%
prp(train.y.cv)
