#R-Code for Parole
#===================

#1. Installing Packages and Running Libraries
install.packages("caTools")
install.packages("ROCR")
install.packages("gains")
install.packages("gplots")

library(caTools)
library(ROCR)
library(gains)


#2. Data Loading and Processing
getwd()

## Setting the Working Directory
setwd("/Users/lovenishgaur/Documents/Predictive_Analytics_IIM_Raipur/CourseMaterial")

parole = read.csv("Parole.csv")
str(parole)

#Data Cleaning 
parole$male = as.factor(parole$male)
parole$race = as.factor(parole$race)
parole$state = as.factor(parole$state)
parole$multiple.offenses = as.factor(parole$multiple.offenses)
parole$crime = as.factor(parole$crime)
parole$violator = as.factor(parole$violator)
parole$max.sentence = as.numeric(parole$max.sentence)

#Summary of data set
summary(parole)

#Summary for particular Variable
summary(parole$male)
summary(parole$race)
summary(parole$state)
summary(parole$multiple.offenses)
summary(parole$crime)
summary(parole$violator)

#3. Setting the Baseline and Splitting the Dataset into training and testing sets
#Set the Baseline
table(parole$violator)
baseline = 597/(597+78)
baseline

# Split the data using subset
set.seed(1000)
split = sample.split(parole$violator, SplitRatio = 0.60)
train = subset(parole, split==TRUE)
test = subset(parole, split==FALSE)
str(train)
str(test)

table(test$violator)
table(train$violator)

#4. Build a Classification Model: Logistics Regression Model 

# Training the Model
model1 = glm(violator ~ ., data=train, family=binomial) #lOGISTIC REGRESSION MODEL
summary(model1)
model1$fitted.values
predictTrain = predict(model1, type="response", data=train)
predictTrain
table(train$violator, predictTrain>0.5)
(348+17)/(348+10+30+17)

# Logistics Regression Model on Test Set
predictTest = predict(model1, type="response", newdata=test)
predictTest

# Confusion matrix with threshold of 0.5
table(test$violator, predictTest > 0.5)


# Accuracy
(231+12)/(231+12+8+19) #90.00%



#5. Judging Classification Performance: ROCR Curve 

# ROCR Curve and Test set AUC 
library(ROCR)
ROCRpred = prediction(predictTest, test$violator)
ROCRperf = performance(ROCRpred, "tpr","fpr")

plot(ROCRperf, col="black", lty=2, lwd=1)
plot(ROCRperf, col=rainbow(4))
as.numeric(performance(ROCRpred, "auc")@y.values)

# Writing ROCR Data to file
ROCRdata = as.data.frame(cbind(test$y, predictTest))
str(ROCRdata)
write.csv(ROCRdata, "ROCRdata.csv")


# Determining Optimal Cut-off from ROCR Curve
# It is given by Youden's Index = max(sensitivity+specificity-1)
str(ROCRpred)
ROCRpred@fp
ROCRpred@tp
ROCRpred@tn
ROCRpred@fn
str(ROCRperf)

Youden = (ROCRpred@tp[[1]]/(ROCRpred@tp[[1]]+ROCRpred@fn[[1]]) + ROCRpred@tn[[1]]/(ROCRpred@tn[[1]]+ROCRpred@fp[[1]]) - 1)
max(Youden)

a = ROCRpred@tp[[1]]/(ROCRpred@tp[[1]]+ROCRpred@fn[[1]])
b = ROCRpred@tn[[1]]/(ROCRpred@tn[[1]]+ROCRpred@fp[[1]])
c = Youden

z = as.data.frame(cbind(a,b,c))
max(c)

plot (Youden)

str(ROCRperf)
ROCRperf@alpha.values[[1]]
ROCRperf@y.values[[1]]
ROCRperf@x.values[[1]]
Youden = max(ROCRperf@y.values[[1]] - (ROCRperf@x.values[[1]]-1)-1)
Youden

table(test$violator, predictTest > 0.14636461)
(204+21)/(204+35+10+21)


Youdendata = as.data.frame(cbind(ROCRperf@alpha.name, Youden))
View(Youdendata)

#6. Judging Classification Performance: Building the Lift Chart
predictTest
test$violator

lift = as.data.frame(cbind(test$violator, predictTest))
lift

library(gains)
gain = gains(as.integer(test$violator), predictTest)

#Plot lift Chart
plot(c(0,gain$cume.pct.of.total*sum(as.numeric(test$violator)))~c(0,gain$cume.obs),
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(as.numeric(test$violator)))~c(0, dim(test)[1]), lty=2)

