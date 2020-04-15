##Session 3: Predictive Analytics using Regression
##================================================
#Installing Packages
install.packages("psych")
install.packages("car")
install.packages("lmtest")
install.packages("caTools")
install.packages("forecast")
install.packages("readxl")


##1. Opening and Processing the Dataset
getwd()

## Setting the Working Directory
setwd("/Users/lovenishgaur/Documents/Predictive_Analytics_IIM_Raipur/CourseMaterial")
#Read data from File
data = read_excel("ToyotaCorolla.xls")
data = read.csv("ToyotaCorolla.csv")
#Structure of data
str(data)
#Summary about the data
summary(data)
#Subsetting the data 
data = subset(data, Price<15000) 

data = subset(data, Price>15000)

#Setting Proper Variable Types
data$Model = as.character(data$Model)
data$Price = as.numeric(data$Price)
data$Met_Color = as.factor(data$Met_Color)
data$Automatic = as.factor(data$Automatic)
summary(data)
str(data)
data[81,10]

#Correccting the outlier
data$cc  <- ifelse(data$cc  == 16000, 1600, data$cc )
summary(data)

data= data[,3:13]
model1 = lm(data = data, Price ~ .)
summary(model1)

model1$residuals
model1$fitted.values
options(scipen = 999)
library(forecast)
accuracy(model1$fitted.values, data$Price)

##2.Running Exploratory Analysis
str(data)

plot(data[,c(1:3,5,8:11)])

#Plotting Histograms
hist(data$Price) #Slightly Skewed
hist(data$Age_08_04) #Skewed
hist(data$KM) #Slightly Skewed
hist(data$HP) #Skewed
hist(data$cc) #Not much variation
hist(data$Quarterly_Tax) # can't say
hist(data$Weight) #heavily Skewed

#Plotting log of Histograms
data$logPrice = log(data$Price)
hist(data$logPrice)

#Plotting Scatterplots between quantitative variables
plot(data$Age_08_04, data$Price)
plot(data$KM, data$Price)
plot(data$HP, data$Price)
plot(data$cc, data$Price)
plot(data$Quarterly_Tax, data$Price)
plot(data$Weight, data$Price)

#Examining Correlations
str(data)
cor(data[c(1:3,5,8:11)])
summary(model1)

##3. Running Basic Regression on the Data
data = data[,3:13]
model1 = lm(data = data, Price ~ .)
summary(model1)
accuracy(model1$fitted.values, data$Price)


##4. Test for Assumptions - Normality, Linearity, Multicollinearity, 
## Correlated Errors, Homoscedasticity, Influential Observations / Outliers

#Normality of Residuals
hist(data$Price)
qqnorm(data$Price) #Normality of Response variable
hist(data$logPrice)
qqnorm(data$logPrice)
hist(model1$residuals) #Distribution of Residuals
qqnorm(model1$residuals)
hist(model1$fitted.values) #Distribution of Fitted Values
qqPlot(model1, main="QQ Plot") # qq plot for residuals

#Multicollinearity
library(psych)
library(car)
psych::corr.test(data[c(1:3,5,8:11)]) #Not very high correlations
vif(model1) #Multicollinearity is absent
sqrt(vif(model1)) #It is greater than 2 for Fuel Type which may cause multicollinearity problem

#Correlated Errors
durbinWatsonTest(model1) #1.59 - Not much autocorrelation

#Homoscedasticity
plot(model1$fitted.values, model1$residuals)
plot(model1)
ncvTest(model1) #p-value is less than 0.5 which means heterescedasticity is present

#Testing for Linearity
crPlots(model1) #Plot of cc and weight may introduce problems
ceresPlots(model1)

#Testing Outliers
outlierTest(model1) # Bonferonni p-value for most extreme obs
qqPlot(model1, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(model1) # leverage plots

#Testing Influential Observations - Cook's D plot
cutoff <- 4/((nrow(data)-length(model1$coefficients)-2)) # identify D values > 4/(n-k-1) 
plot(model1, which=4, cook.levels=cutoff)


##5. Building Second Model on the Data

#Remove Heteroscedasticity by Dividing the dataset as the residual plot shows two clusters

#Remove Possible Multicollinear Variables (cc) and Non-Linear variables (Automatic)
str(data)
data = data[,-c(7,8)]
str(data)

#Remove Influential Observations
data = data[-c(192, 193, 222, 602, 1059),]
data = data[-c(222,961,602),]
str(data)

#Building new Model
model2 = lm(data = data, Price ~.)
summary(model2)

#Remove Insignificant variables
model2 = lm(data = data, Price ~. - Met_Color -KM)
summary(model2)


##6. Testing Assumptions on new model
vif(model2) #Multicollinearity
durbinWatsonTest(model2) #Autocorrelation
plot(model2$fitted.values, model2$residuals) #The plot does not show any relationship between the residuals and fitted values
ncvTest(model2) #Heteroscedasticity
plot(model2)
crPlots(model2) 
outlierTest(model2) 
qqPlot(model2, main="QQ Plot")

accuracy(model2$fitted.values, data$Price)


##7. Data Mining Approach 

#Processing the Dataset
data = read.csv("ToyotaCorolla.csv")
str(data)
data = data[,3:13]
data = subset(data, Price<15000)
data$Price = as.numeric(data$Price)
data$Met_Color = as.factor(data$Met_Color)
data$Automatic = as.factor(data$Automatic)
summary(data)

#Splitting the dataset
library(caTools)
set.seed(1000)
split = sample.split(data$Price,SplitRatio=0.7)
train= subset(data,split==TRUE)
test= subset(data,split==FALSE)
str(train)
str(test)

#Running Raw Regression on Training Dataset
modeldm1 = lm(data=train, Price ~ .)
summary(modeldm1) #Adj R2 = 72.90%
plot(modeldm1$residuals, modeldm1$fitted.values)
library(forecast)
accuracy(modeldm1$fitted.values, train$Price)

library(psych)
library(car)
vif(modeldm1) #Multicollinearity
durbinWatsonTest(modeldm1) #Autocorrelation
plot(modeldm1$fitted.values, modeldm1$residuals) #The plot does not show any relationship between the residuals and fitted values
ncvTest(modeldm1) #Heteroscedasticity
plot(modeldm1)
crPlots(modeldm1) 
outlierTest(modeldm1) 
qqPlot(modeldm1, main="QQ Plot")
leveragePlots(model1) # leverage plots


#Remove Multicollinear variables (cc) and Nonlinear variables (Automatic)
data = data[-c(222,961,602),]
str(train)
train = train[,-c(7,8)]
modeldm2 = lm(data=train, Price ~ .)
accuracy(modeldm2$fitted.values, train$Price)


train = train[-c(222,961,602),]
modeldm3 = lm(data=train, Price ~ .)
summary(modeldm3)
leveragePlots(model3)
crPlots(modeldm3) 
accuracy(modeldm3$fitted.values, train$Price)

#Predict on Test Data
predictTest = predict(modeldm2, newdata=test)
predictTest
accuracy(predictTest, test$Price)