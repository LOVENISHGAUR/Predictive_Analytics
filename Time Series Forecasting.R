A. TIME SERIES FORECASTING - BASICS
======================================
  
#Preliminary Work
install.packages("forecast")
install.packages("zoo")
library(forecast)
getwd()  

#1. Loading the Dataset  
data = read.csv("Amtrak.csv")
data
data = data[1:159,]
str(data)
summary(data)
names(data)[3]="Ridership"

#2. Plotting the Series
# create time series object using ts()
# ts() takes three arguments: start, end, and freq.
# with monthly data, the frequency of periods per season is 12 (per year).
# arguments start and end are (season number, period number) pairs.
# here start is Jan 1991: start = c(1991, 1); end is Mar 2004: end = c(2004, 3)

ridership.ts = ts(data$Ridership, start=c(1991,1), end=c(2004,3), freq=12)
ridership.ts
plot(ridership.ts)
plot(ridership.ts, xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 2300))

#Plotting a smaller window (Jan 1997 to Dec 1999)
ridership.ts.3yrs = window(ridership.ts, start=c(1997,1), end =c(1999,12))
plot(ridership.ts.3yrs, xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 2300))

#Fitting a Linear Regression model to the time series
ridership.lm = tslm(ridership.ts ~ trend + I(trend^2))
plot(ridership.ts, xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 2300))
lines(ridership.lm$fitted, lwd=2)

plot(ridership.ts.3yrs, xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 2300))
lines(ridership.lm$fitted, lwd=2)

#3. Partitioning the Data into Training and Validation Set

#Partitioning in Time Series is done simply by dividing the data into two continuous time periods
#Naive forecast at any future time period t+k is the value at time t / or a seasonal time forecast

nValid =  36
nTrain = length(ridership.ts) - nValid

train.ts = window(ridership.ts, start = c(1991,1), end = c(1991, nTrain))
valid.ts = window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))

plot(train.ts)
plot(valid.ts)

#Setting the baseline
#Generating naive and seasonal naive forecast
naive.pred = naive(train.ts, h = 36)
snaive.pred = snaive(train.ts, h = nValid)

#Plotting the Ridership data
plot(ridership.ts, xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 2300))
lines(ridership.lm$fitted, lwd=1)

lines(naive.pred$mean, lwd =2, col = "blue", lty=1)
lines(snaive.pred$mean, lwd=2, col = "red", lty=3)

#Finding the accuracy
accuracy(naive.pred, valid.ts)
accuracy(snaive.pred, valid.ts)



#B. TIME SERIES FORECASTING WITH REGRESSION
#==========================================
  
#1. A Model with Linear Trend - Fit linear trend model to training set and create

#Linear Trend: y = B0 + B1t + e to Model Level (B0), Trend (B1), Noise (e) 
ridership.lm = tslm(ridership.ts~trend)
summary(ridership.lm)
plot(ridership.ts, xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 2300))
lines(ridership.lm$fitted, lwd=2)
  
#Fit linear trend model to training set and create forecasts
train.lm = tslm(train.ts~trend)
summary(train.lm)
train.lm.pred = forecast(train.lm, h=nValid, level = 0)

plot(train.ts, xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 2300))

#predicted values
plot(train.lm.pred, xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 2300), flty=2)
lines(train.lm.pred$fitted, lwd=1, col="blue")
lines(valid.ts, lwd=1, col="blue")
summary(train.lm)
summary(train.lm.pred)

#Plotting the residuals also
plot(train.lm.pred$residuals, ylab= "Forecast Errors", xlab="Time")
lines(valid.ts-train.lm.pred$mean, lwd=1, col="blue")

accuracy(train.lm.pred, valid.ts)

#2. An Exponential Trend - Multiplicative increase / decrease of the series over time
#Model: yt = ce^(exp(B1t + e)) 
#To fit an exponential trend, simply replace the outcome variable Y with Log Y
#Exponential trend reflect percentage growth
#To fit an exponential trend in R set lambda = o in function tslm()

#fit exponential trend using tslm() with argument lambda = 0
train.lm.expo.trend = tslm(train.ts ~ trend, lambda = 0)
summary(train.lm.expo.trend)
train.lm.expo.trend.pred = forecast(train.lm.expo.trend, h=nValid, level =0)

#fit linear trend using tslm() with argument lambda = 1
train.lm.linear.trend = tslm(train.ts ~ trend, lambda=1)
train.lm.linear.trend.pred = forecast(train.lm.linear.trend, h=nValid, level =0)

#plotting the graphs
plot(train.lm.expo.trend.pred, ylim=c(1300,2300), ylab="Ridership", xlab="Time")

lines(train.lm.expo.trend.pred$fitted, lwd=2, col="blue")
lines(train.lm.linear.trend.pred$fitted, lwd=2, col="black", lty=3)
lines(train.lm.linear.trend.pred$mean, lwd=2, col="black", lty=3)
lines(valid.ts)
lines(train.ts)

accuracy(train.lm.linear.trend.pred, valid.ts)
accuracy(train.lm.expo.trend.pred, valid.ts)

#3. Fitting a Polynomial Trend

#Model: yt = B0 + B1t + B2t^2 + e
#This is done by creating an additional predictor t2. Fit quadratic trend using fucntion I(), which treats an object "as is"

train.lm.poly.trend = tslm(train.ts ~ trend + I(trend^2))
summary(train.lm.poly.trend)
train.lm.poly.trend.pred = forecast(train.lm.poly.trend, h=nValid, level=0)
plot(train.lm.poly.trend.pred, ylim=c(1300, 2300), ylab="Ridership", xlab="Time")
lines(train.lm.poly.trend$fitted, lwd=2)
lines(valid.ts)

plot(train.lm.poly.trend$residuals, ylab="Forecast Errors", xlab="Time")
lines(valid.ts-train.lm.poly.trend.pred$mean, lwd=1, col="blue")

accuracy(train.lm.poly.trend.pred, valid.ts)


#4. Time Series model with Seasonality
  
#Seasonality is captured in a regression model by creating a new categorical variable that denotes the season for each value
#This categorical variable is then turned in dummies
#Extracting month from Date and then converting it into factor followed by repartitioning of data
  
data$season<-substr(data$Month, start = 1, stop = 3)
data$season = as.factor(data$season)
str(data)

train.lm.season = tslm(train.ts ~ season)
summary(train.lm.season)
train.lm.season.pred = forecast(train.lm.season, h=nValid, level=0)
plot(train.lm.season.pred, ylim=c(1300, 2300), ylab="Ridership", xlab="Time")
lines(train.lm.season$fitted, lwd=2)
lines(valid.ts)

plot(train.lm.season$residuals, ylab="Forecast Errors", xlab="Time")
lines(valid.ts-train.lm.season.pred$mean, lwd=1, col="blue")

accuracy(train.lm.season.pred, valid.ts)

#Such seasonlity is known as additive seasonality. We can also capture multiplicative seasonality where values in a season are higher or lower by a percentage amount as compared to another season
#To do so, we model Y as Log(Y and setting lambda = 0 in the tslm() function


#5. Time Series model with Trend and Seasonality

train.lm.trend.season = tslm(train.ts ~ trend + I(trend^2) + season)
summary(train.lm.trend.season)

train.lm.trend.season.pred = forecast(train.lm.trend.season, h=nValid, level=0)
plot(train.lm.trend.season.pred, ylim=c(1300, 2300), ylab="Ridership", xlab="Time")
lines(train.lm.trend.season$fitted, lwd=2)
lines(valid.ts)

plot(train.lm.trend.season$residuals, ylab="Forecast Errors", xlab="Time")
lines(valid.ts-train.lm.trend.season.pred$mean, lwd=1, col="blue")

accuracy(train.lm.trend.season.pred, valid.ts)


#6. Autocorrelation and ARIMA models (Incorporating Autocorrelation into Time Series Models)
  
#In ordinary regression models, autocorrelation is assumed to be absent
#Autocorrelation can be used to improve forecsting accuracy
#Example: High values are generally followed by high values and we can use this information to adjust forecasts
#To compute autocorrelation, we compute the correlation between the series and a lagged version of the series. 
#A lagged series is a copy of the original series which is moved forward one or more time periods.  
#A lagged series with lag-1 is the original series moved forward one time period; 
#A lagged series with lag-2 is the original series moved forward two time periods, etc  
 

#Identify which series to add (Depends on correlation)
Acf(ridership.ts, lag.max = 12, main = "") #Lag 12 is highly correlated

#Strong autocorrelation (positive or negative) at a lag k larger than 1 and its multiples (2k; 3k; : : :) typically reflects a cyclical pattern. 
#For example, strong positive lag-12 autocorrelation in monthly data will reflect an annual seasonality (where values during a given month each year are positively correlated).
#Positive lag-1 autocorrelation (called stickiness) describes a series where consecutive values move generally in the same direction. 
#In the presence of a strong linear trend, we would expect to see a strong and positive lag-1 autocorrelation.
#Negative lag-1 autocorrelation reflects swings in the series, where high values are immediately followed by low values and vice versa.

Acf(train.lm.trend.season$fitted, lag.max = 12, main = "")

#It is also important to examine autocorrelation of the residual series. If we have adequately modeled
#the seasonal pattern, then the residual series should show no autocorrelation at the seasons lag.

Acf(train.lm.trend.season$residuals, lag.max = 12, main = "") #Lag1 is highly correlated indicating a positive relationship between neighbouring residuals


#7. Improving Forecasts by Integrating Autocorrelation Information - (ARIMA: Building Autocorrelation into Regression Model)

#fit AR(1) model to training residuals
#use Arima() in the forecast package to fit an ARIMA model
help(Arima)
train.res.arima = Arima(train.lm.trend.season$residuals, order = c(1,0,0))
valid.res.arima.pred = forecast(train.res.arima, h = nValid)
summary(train.res.arima)

valid.res.arima.pred

plot(train.lm.trend.season$residuals, ylab = "Residuals", xlab = "Time", bty = "l", main = "")
lines(valid.res.arima.pred$fitted, lwd = 2, col = "blue")

plot(train.res.arima$residuals)
accuracy(valid.res.arima.pred, valid.ts)

Acf(valid.res.arima.pred$residuals, lag.max = 12, main = "")


#8. Evaluating Predictability of a Time Series

#One useful way to assess predictability is to test whether the series is a random walk. 
#A random walk is a series in which changes from one time period to the next are random.
#A random walk is a special case of an AR(1) model, where the slope coefficient is equal to 1:  
#Forecasts from such a model are basically equal to the most recent observed value 
#(the naive forecast), reflecting the lack of any other information.
#If a series is a random walk, it cannot be predicted (E.g., Stock Prices)

summary(train.res.arima)  

#The slope coefficient in the summary is (1-0.5997)/0.0712 = 5.6 std. errors away 1 and significant different from 1.
#If the slope coefficent is closer to 1, then the series is a random walk.



#C. TIME SERIES FORECASTING USING SMOOTHING METHODS
#==================================================
#Such methods rely on averaging over multiple periods to reduce the noise
#Moving Average and Exponential Smoothing are two simple smoothers suitable for forecasting series that do not contain any trend or seasonality
#Forecasts are averages of previous values of the series
#These methods are data driven and do not assume a pre-determined structure in a series
#Especially useful in series where patterns change over time

#1. Moving Average
  
#It consists of averaging values across a window of consecutive periods, thereby generating a series of averages.
#Types: Centered (Powerful for visuzaling trends) and Trailing moving average (Suitable for forecasting)
#The default choice of a window in a moving average is the seasonality of the data, so that both seasonality and noise are averaged out

str(data)
ridership.ts = ts(data$Ridership, start=c(1991,1), end=c(2004,3), freq=12)

#Centered Moving Average with window k=12
ridership.ma = ma(ridership.ts, order = 12, centre=TRUE)  
plot(ridership.ts)
lines(ridership.ma)

library(zoo)
#Trailing Moving Average with window k=12
ridership.ma = rollmean(ridership.ts, k = 12, align="right")
lines(ridership.ma, lty=3)

#Partition the data

nValid =  36
nTrain = length(ridership.ts) - nValid

train.ts = window(ridership.ts, start = c(1991,1), end = c(1991, nTrain))
valid.ts = window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))

#moving average on training
train.ma = rollmean(train.ts, k=12, align="right")

#obtain the last moving average in the training period
last.ma = tail(train.ma, 1)
last.ma

# create forecast based on last MA
train.ma.pred = ts(rep(last.ma, nValid), start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid), freq = 12)
plot(train.ts, ylim = c(1300, 2600), xlim= c(1991, 2006.25), ylab = "Ridership", xlab = "Time", bty = "l")
lines(train.ma, lwd = 2, col = "blue")
lines(train.ma.pred, lwd = 2, col = "blue", lty = 2)
lines(valid.ts)

#In general moving average should be used to forecast seies without trend and seasonality
#In case seasonality is present in the data, then it should be de-trended and de-seasonalized and after forcasting these can be added back


#2. Applying Moving Average to previous forecasted series

train.lm.trend.season = tslm(train.ts ~ trend + I(trend^2) + season)

# create single-point forecast
train.lm.trend.season.pred = forecast(train.lm.trend.season, h = 36, level = 0) #2004.163

# apply MA to residuals
train.ma = rollmean(train.lm.trend.season$residuals, k = 12, align = "right")
last.ma = tail(train.ma, 1) #30.71129

New_Forecast = 2004.163+30.71129 #2034.874

accuracy(train.lm.trend.season.pred, valid.ts)

#3. Exponential Smoothing

#Simple exponential smoothing is similar to forecasting with a moving average, except that instead of 
#taking a simple average over the w most recent values, we take a weighted average of all past values, 
#such that the weights decrease exponentially into the past. The idea is to give more weight to recent
#information, yet not to completely ignore older information.

# Model: Ft+1 = aYt + a(1 - a)Yt-1 + a(1-a)^2Yt-2 + ....
# where, a is a constant between 0 and 1 and is known as the smoothing parameter
# or
# Ft+1 = Ft + aEt, where Et is the forecast error at time t

#Alpha 'a' depends on whether past values influence more or recent values influence more
#Default values that have been shown to work well are around 0.1/0.2.

# get residuals
residuals.ts <- train.lm.trend.season$residuals
# run simple exponential smoothing
# use ets() with model = "ANN" (additive error (A), no trend (N), no seasonality (N))
# and alpha = 0.2 to fit simple exponential smoothing.
ses <- ets(residuals.ts, model = "ANN", alpha = 0.2)
ses.pred <- forecast(ses, h = nValid, level = 0)
plot(ses.pred, ylim = c(-250, 300), ylab = "Ridership", xlab = "Time", xlim = c(1991,2006.25), bty = "l")
lines(train.lm.trend.season.pred$fitted, lwd = 2, col = "blue")
lines(ses.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

accuracy(ses.pred, valid.ts)

#4. Advanced Exponential Smoothing

#This method can be used to capture trend and seasonality as well

#For series that contain a trend, we can use ???double exponential smoothing.???
#Unlike in regression models, the trend shape is not assumed to be global, but rather, it can change over time.

#Series with a trend
--------------------
#Both level and trend are estimated from the data and then combined to generate the forecast
#Ft+k = Lt + kTt
#The level and trend are updated through a pair of updating equations
#Lt = aYt + (1-a)(Lt-1 + Tt-1)
#Tt = b(Lt-Lt-1) + (1-b)Tt-1

#Series with a trend and seasonality using 'Holt-Winter's Exponential Smoothing' method
---------------------------------------------------------------------------------------  
#Ft+k = (Lt + kTt) St+k-m???

#The level, trend and seasonality are updated through a pair of updating equations
#Lt = aYt/St-m + (1-a)(Lt-1 + Tt-1)
#Tt = b(Lt-Lt-1) + (1-b)Tt-1
#St = yYt/Lt + (1-y)St-m

# run Holt-Winters exponential smoothing
# use ets() with option model = "MAA" to fit Holt-Winter's exponential smoothing
# with multiplicative error, additive trend, and additive seasonality.
hwin = ets(train.ts, model = "MAA")

# create predictions
hwin.pred <- forecast(hwin, h = nValid, level = 0)

# plot the series
plot(hwin.pred, ylim = c(1300, 2600), ylab = "Ridership", xlab = "Time", xlim = c(1991,2006.25), flty = 2)
lines(hwin.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

hwin


accuracy(hwin.pred, valid.ts)


