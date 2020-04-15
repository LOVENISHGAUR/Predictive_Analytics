
#Installing Packages

install.packages("ggplot2")
install.packages("gridExtra")
install.packages("dplyr")
install.packages('rpart')
install.packages("randomForest")
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("gbm")
install.packages("survival")
install.packages("pROC")
install.packages("DMwR")
install.packages("scales")

install.packages("tidyverse")
install.packages("MASS")
install.packages("car")
install.packages("e1071")
install.packages("cowplot")
install.packages("caTools")
install.packages("ggcorrplot")


#Improting Packages

library(ggplot2)
library(gridExtra)
library(dplyr)
library(rpart)
library(randomForest)
library(caret)
library(gbm)
library(survival)
library(pROC)
library(DMwR)
library(scales)
library(tidyverse) 
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(pROC)
library(ggcorrplot)


getwd()
setwd("/Users/lovenishgaur/Documents/Predictive_Analytics_IIM_Raipur/self/customer_churn/")

# Loading the CSV

telco <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")
str(telco)
glimpse(telco)


#Visualizing NAs in the columns:

options(repr.plot.width = 6, repr.plot.height = 4)

missing_data <- telco %>% summarise_all(funs(sum(is.na(.))/n()))

missing_data <- gather(missing_data, key = "variables", value = "percent_missing")

ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.3)+
  xlab('variables')+
  coord_flip()+ 
  theme_bw() 

#Removing Missing Valus
telco <- telco[complete.cases(telco),]

#Converting Variables into factors
telco$SeniorCitizen <- as.factor(ifelse(telco$SeniorCitizen==1, 'YES', 'NO'))


#EXPLORATORY DATA ANALYSIS:

theme1 <- theme_bw()+
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5),legend.position="none")
theme2 <- theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),legend.position="none")

#Checking data structure
glimpse(telco)

#VISUALIZING THE CATEGORICAL DATA FIRST WITH RESPECT TO CHURN:
telco %>% 
  group_by(Churn) %>% 
  summarise(Count = n())%>% 
  mutate(percent = prop.table(Count)*100)%>%
  ggplot(aes(reorder(Churn, -percent), percent), fill = Churn)+
  geom_col(fill = c("#FC4E07", "#E7B800"))+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.01,vjust = -0.5, size =3)+ 
  theme_bw()+  
  xlab("Churn") + 
  ylab("Percent")+
  ggtitle("Churn Percent")

options(repr.plot.width = 12, repr.plot.height = 8)
plot_grid(ggplot(telco, aes(x=gender,fill=Churn))+ geom_bar()+ theme1, 
          ggplot(telco, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(telco, aes(x=Partner,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(telco, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(telco, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(telco, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")


options(repr.plot.width = 12, repr.plot.height = 8)
plot_grid(ggplot(telco, aes(x=InternetService,fill=Churn))+ geom_bar(position = 'fill')+ theme1+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
          ggplot(telco, aes(x=OnlineSecurity,fill=Churn))+ geom_bar(position = 'fill')+theme1+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=OnlineBackup,fill=Churn))+ geom_bar(position = 'fill')+theme1+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=DeviceProtection,fill=Churn))+ geom_bar(position = 'fill')+theme1+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=TechSupport,fill=Churn))+ geom_bar(position = 'fill')+theme1+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")

plot_grid(ggplot(telco, aes(x=StreamingMovies,fill=Churn))+ 
            geom_bar(position = 'fill')+ theme1+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
          ggplot(telco, aes(x=Contract,fill=Churn))+ 
            geom_bar(position = 'fill')+theme1+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=PaperlessBilling,fill=Churn))+ 
            geom_bar(position = 'fill')+theme1+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=PaymentMethod,fill=Churn))+
            geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")

#Analyzing the three continuous variables w.r.t CHURN:

#Tenure: The median tenure for customers who have left is around 10 months.
options(repr.plot.width =6, repr.plot.height = 2)
ggplot(telco, aes(y= tenure, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")

#MonthlyCharges: Customers who have churned, have high monthly charges. The median is above 75.
ggplot(telco, aes(y= MonthlyCharges, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")

#TotalCharges:* The median Total charges of customers who have churned is low.
ggplot(telco, aes(y= TotalCharges, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")

#Checking the correlation between continuous variables

#Total Charges has positive correlation with MonthlyCharges and tenure.

options(repr.plot.width =6, repr.plot.height = 4)
telco_cor <- round(cor(telco[,c("tenure", "MonthlyCharges", "TotalCharges")]), 1)

ggcorrplot(telco_cor,  title = "Correlation")+theme(plot.title = element_text(hjust = 0.5))

#Checking for outliers in the continuous variables, and it seems none of the values are beyond the whiskers here.

options(repr.plot.width =4, repr.plot.height = 4)
boxplot(telco$tenure)$out
boxplot(telco$MonthlyCharges)$out
boxplot(telco$TotalCharges)$out

#DATA PREPARATION:

#Cleaning the Categorical features
telco <- data.frame(lapply(telco, function(x) {
  gsub("No internet service", "No", x)}))

telco <- data.frame(lapply(telco, function(x) {
  gsub("No phone service", "No", x)}))

#Standardising Continuous features
num_columns <- c("tenure", "MonthlyCharges", "TotalCharges")
telco[num_columns] <- sapply(telco[num_columns], as.numeric)

telco_int <- telco[,c("tenure", "MonthlyCharges", "TotalCharges")]
telco_int <- data.frame(scale(telco_int))

#Creating derived features

#max(telco$tenure)
#min(telco$tenure)
telco <- mutate(telco, tenure_bin = tenure)

telco$tenure_bin[telco$tenure_bin >=0 & telco$tenure_bin <= 12] <- '0-1 year'
telco$tenure_bin[telco$tenure_bin > 12 & telco$tenure_bin <= 24] <- '1-2 years'
telco$tenure_bin[telco$tenure_bin > 24 & telco$tenure_bin <= 36] <- '2-3 years'
telco$tenure_bin[telco$tenure_bin > 36 & telco$tenure_bin <= 48] <- '3-4 years'
telco$tenure_bin[telco$tenure_bin > 48 & telco$tenure_bin <= 60] <- '4-5 years'
telco$tenure_bin[telco$tenure_bin > 60 & telco$tenure_bin <= 72] <- '5-6 years'

telco$tenure_bin <- as.factor(telco$tenure_bin)

options(repr.plot.width =6, repr.plot.height = 3)
ggplot(telco, aes(tenure_bin, fill = tenure_bin)) + geom_bar()+ theme1

#Creating Dummy Variables
telco_cat <- telco[,-c(1,6,19,20)]

#Creating Dummy Variables
dummy<- data.frame(sapply(telco_cat,function(x) data.frame(model.matrix(~x-1,data =telco_cat))[,-1]))

head(dummy)


#Creating the final dataset by combining the numeric and dummy data frames.
#Combining the data
telco_final <- cbind(telco_int,dummy)
head(telco_final)


#Splitting the data into train and validation data.
#Splitting the data
set.seed(123)
indices = sample.split(telco_final$Churn, SplitRatio = 0.7)
train = telco_final[indices,]
validation = telco_final[!(indices),]
