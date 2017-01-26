# Time Series ARIMA Models in R
# Copyright 2013 by Ani Katchova

# install.packages("tseries")
library(tseries)

mydata<- read.csv("C:/Users/ybsipka/Desktop/R_dir/apple_stocks.csv")
bigdata <-read.csv("C:/Users/ybsipka/Desktop/Reengen/hourly_data.csv")
attach(mydata)
attach(bigdata)

input <- Electricity.Facility..kWh..Hourly.
inputsmall <- input[1:1000]
time <- Date.Time[1:1000]


# Defining variables
#Y <- e_cons
Y <- inputsmall
d.Y <- diff(Y)
t <- time
#t <- Week

# Descriptive statistics and plotting the data
summary(Y)
summary(d.Y)

plot(t,Y)
plot(d.Y)

# Dickey-Fuller test for variable
adf.test(Y, alternative="stationary", k=0)
adf.test(Y, alternative="explosive", k=0)

#summary(lm(dppi ~ lppi, na.action=na.omit))
#summary(lm(dppi ~ lppi + trend, na.action=na.omit))

# Augmented Dickey-Fuller test
adf.test(Y, alternative="stationary")

# DF and ADF tests for differenced variable
adf.test(d.Y, k=0)
adf.test(d.Y)


# ACF and PACF
acf(Y)
pacf(Y)

acf(d.Y)
pacf(d.Y)

# ARIMA(1,0,0) or AR(1)
auto.arima(Y)

# ARIMA(2,0,0) or AR(2)
arima(Y, order = c(2,0,0))

# ARIMA(0,0,1) or MA(1)
arima(Y, order = c(0,0,1))

# ARIMA(1,0,1) or AR(1) MA(1)
arima(Y, order = c(1,0,1))

# ARIMA on differenced variable 
# ARIMA(1,1,0)
arima(d.Y, order = c(1,0,0))

# ARIMA(0,1,1)
arima(d.Y, order = c(0,0,1))

# ARIMA(1,1,1)
arima(d.Y, order = c(1,0,1))

# ARIMA(1,1,3)
arima(d.Y, order = c(1,0,3))

# ARIMA(2,1,3)
arima(d.Y, order = c(2,0,3))


# ARIMA(1,0,1) forecasting
mydata.arima101 <- arima(Y, order = c(2,0,1))
mydata.pred1 <- predict(mydata.arima101, n.ahead=12)
plot (Y)
lines(mydata.pred1$pred, col="blue")
lines(mydata.pred1$pred+2*mydata.pred1$se, col="red")
lines(mydata.pred1$pred-2*mydata.pred1$se, col="red")
mydata.pred1$pred

# ARIMA(1,1,1) forecasting
mydata.arima110 <- arima(d.Y, order = c(1,0,0))
mydata.pred1 <- predict(mydata.arima110, n.ahead=24)
plot (d.Y)
lines(mydata.pred1$pred, col="blue")



lines(mydata.pred1$pred+2*mydata.pred1$se, col="red")
lines(mydata.pred1$pred-2*mydata.pred1$se, col="red")
mydata.pred1$pred

actualdata <- input[1000:1023]
ts(actualdata)
lastday <- input[977:1000]
ts(lastday)
prediction <- lastday + mydata.pred1$pred
prediction
error <- actualdata - prediction
error
sum(error)
error(1)
error[1]
actualdata
lastday



plot(tsY)
lines(pred1$pred, col="blue")
prediction <-pred1$pred*5-10000 

lines(pred1$pred+2*pred1$se, col="red")
lines(pred1$pred-2*pred1$se, col="red")
