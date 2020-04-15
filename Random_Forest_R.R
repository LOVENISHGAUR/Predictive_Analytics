#oading the required packages

install.packages("randomForest")
install.packages("MASS") #This package contain Boston dataset

require(randomForest)
require(MASS)
attach(Boston)
set.seed(101)

dim(Boston)


#Saperating Training and Test Sets

#training Sample with 300 observations
train=sample(1:nrow(Boston),300)
?Boston  #to search on the dataset

#Fitting the Random Forest

Boston.rf=randomForest(medv ~ . , data = Boston , subset = train)
Boston.rf  
# In output The above Mean Squared Error and Variance explained 
#are calculated using Out of Bag Error Estimation


#Plotting the Error vs Number of Trees Graph.

plot(Boston.rf) 

#This plot shows the Error and the Number of Trees. 
#We can easily notice that how the Error is dropping as we keep on adding more and more trees
#and average them.


oob.err=double(13)
test.err=double(13)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:13) 
{
  rf=randomForest(medv ~ . , data = Boston , subset = train,mtry=mtry,ntree=400) 
  oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
  
  pred<-predict(rf,Boston[-train,]) #Predictions on Test Set for each Tree
  test.err[mtry]= with(Boston[-train,], mean( (medv - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}


#Test Error

test.err

#Out of Bag Error Estimation

oob.err


#Plotting both Test Error and Out of Bag Error

matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))
