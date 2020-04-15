##Session 3: Predictive Analytics using Regression
##================================================
#Installing Packages
install.packages("psych")
install.packages("lmtest")
install.packages("caTools")
install.packages("forecast")
install.packages("readxl")

##1. Opening and Processing the Dataset
getwd()

setwd("/Users/lovenishgaur/Documents/Predictive_Analytics_IIM_Raipur/Assignment")
#listing files
list.files()

data = read.csv("ClimateChangeData.csv")
#Structure of data
str(data)

#Summary about the data
summary(data)


data= data[,3:11]
data
model1 = lm(data = data, Temp ~ .)
model1 = lm(data = data, Temp ~ MEI)
model1 = lm(data = data, Temp ~ CO2)
model1 = lm(data = data, Temp ~ CH4)
model1 = lm(data = data, Temp ~ N2O)
model1 = lm(data = data, Temp ~ CFC.11)
model1 = lm(data = data, Temp ~ CFC.12)
model1 = lm(data = data, Temp ~ TSI)
model1 = lm(data = data, Temp ~ Aerosols)
summary(model1)
model1$residuals
model1$fitted.values
options(scipen = 999)
library(forecast)
accuracy(model1$fitted.values, data$Temp)

##2.Running Exploratory Analysis
str(data)


#Plotting Histograms
hist(data$Temp) #normal
hist(data$MEI) #normal
hist(data$CO2) #normal
hist(data$N2O) #normal
hist(data$CFC.11) #right skewed
hist(data$CFC.12) #rightskewed
hist(data$TSI) #left Skewed
hist(data$Aerosols)#highly skewed
hist(data$Temp)#Normal

#Plotting log of Histograms
data$logTemp = log(data$Temp)
hist(data$logTemp)


#Plotting Scatterplots between quantitative variables
plot(data$CO2, data$Temp)
plot(data$CH4, data$Temp)
plot(data$MEI, data$Temp)
plot(data$N2O, data$Temp)
plot(data$CFC.11, data$Temp)
plot(data$CFC.12, data$Temp)
plot(data$TSI, data$Temp)
plot(data$Aerosols, data$Temp)

#Examining Correlations
str(data)
cor(data)
summary(model1)

##4. Test for Assumptions - Normality, Linearity, Multicollinearity, 
## Correlated Errors, Homoscedasticity, Influential Observations / Outliers

#Normality of Residuals
hist(data$Temp)
qqnorm(data$Temp) #Normality of Response variable
hist(data$logTemp)
qqnorm(data$logTemp)
hist(model1$residuals) #Distribution of Residuals
qqnorm(model1$residuals)
hist(model1$fitted.values) #Distribution of Fitted Values
qqPlot(model1, main="QQ Plot") # qq plot for residuals

#Multicollinearity
library(psych)
psych::corr.test(data[c(1,4:7)]) #Not very high correlations
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

#Splitting the dataset
library(caTools)
set.seed(1000)
split = sample.split(data$Temp,SplitRatio=0.7)
train= subset(data,split==TRUE)
test= subset(data,split==FALSE)
str(train)
str(test)

#Running Raw Regression on Training Dataset
modeldm1 = lm(data=train, Temp ~ .)
summary(modeldm1) #Adj R2 = 74.53%
plot(modeldm1$residuals, modeldm1$fitted.values)
library(forecast)
accuracy(modeldm1$fitted.values, train$Temp)


library(psych)
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
data = data[-c(183,184,128,129),]
str(train)
train = train[,-c(7,8)]
modeldm2 = lm(data=train, Temp ~ .)
accuracy(modeldm2$fitted.values, train$Temp)


train = train[-c(183,184,128,129),]
modeldm3 = lm(data=train, Temp ~ .)
summary(modeldm3)
leveragePlots(modeldm3)
crPlots(modeldm3) 
accuracy(modeldm3$fitted.values, train$Temp)


#Predict on Test Data
predictTest = predict(modeldm2, newdata=test)
predictTest
accuracy(predictTest, test$Temp)