install.packages("forecast")
library(forecast)
library(xts)
plot(gas)
View(gas)
class(gas)
?periodicity
periodicity(gas)

#Make sure all the steps in a TS analysis is taken

#To find the trend line. (using abline- used in finding best fit line in linear regression which is y 'lm')
abline(reg=lm(gas~time(gas)))


#Assignment
plot(diff(diff(log(gas))))
abline(h=0)
kpss.test(diff(diff(log(gas))))

pacf(diff(diff(log(gas))))
acf(diff(diff(log(gas))))

#Assignment
plot(diff(sin(gas)))
abline(h=0)
kpss.test(diff(sin(gas)))

pacf(diff(sin(gas)))
acf(diff(sin(gas)))


#ARIMA
library(timeSeries)
kpss.test(gas)
plot(gas)
plot(log(gas))
plot(diff(log(gas)))
abline(h=0)
?kpss.test
kpss.test(diff(log(gas)))

acf(diff(log(gas)))
pacf(diff(log(gas)))
?pacf
pacf(diff(diff(log(gas))))
acf(diff(diff(log(gas))))
kpss.test(diff(diff(log(gas))))
#Model

model<-arima(log(gas),c(2,2,1),seasonal = list(order=c(2,2,1),period=12))
model
#using the above arima model let us predict the number of airpassengers for the next 1 yr.

pred<-predict(model,n.ahead=12*1)
predf<-2.718^pred$pred
predf
?accuracy

accuracy(model)

#Simple Exponential smoothing

library(forecast)
library(tseries)
class(nhtemp)
View(nhtemp)
plot(nhtemp)
kpss.test(nhtemp)
modelses<-HoltWinters(diff(log(nhtemp)),beta=F,gamma=F)
modelses
modelses<-HoltWinters(nhtemp,beta=F,gamma=F)
modelses
plot(modelses)
#The above graph indicates an overlap of the actual values with the forecasted values for thecurrent data set.
#forecast for hte next five periods
fses<-forecast(modelses,5)
plot(fses)
accuracy(fses)

#Normality test
shapiro.test((fses$residuals))

#Ensuring no autocorrelation between errors
Box.test(fses$residuals,type="Ljung-Box")


plot(AirPassengers)
kpss.test(AirPassengers)
modelses1<-HoltWinters(AirPassengers,beta=F,gamma=F)
modelses1
plot(modelses1)
#The above graph indicates an overlap of the actual values with the forecasted values for thecurrent data set.
#forecast for hte next five periods
fses1<-forecast(modelses1,12)
plot(fses1)
accuracy(fses1)

#Normality test
shapiro.test((fses1$residuals))

#Ensuring no autocorrelation between errors
Box.test(fses1$residuals,type="Ljung-Box")

#SES for only last 5 yrs
setwd("C:\\Users\\kausik\\Documents\\Sindhu\\PGP BABI\\Capstone\\Financial statement analysis\\Data\\")
getwd()
data2= read.csv(file.choose(),header=TRUE)
View(data2)

data2<-ts(data2,start=c(1956,1),end=c(1960,12),frequency = 12)
data2
class(data2)

plot(data2)
kpss.test(data2)
modelses2<-HoltWinters(data2,beta=F,gamma=F)
modelses2
plot(modelses2)
#The above graph indicates an overlap of the actual values with the forecasted values for thecurrent data set.
#forecast for hte next five periods
fses2<-forecast(modelses2,12)
plot(fses2)
accuracy(fses2)

#Normality test
shapiro.test((fses2$residuals))

#Ensuring no autocorrelation between errors
Box.test(fses2$residuals,type="Ljung-Box")


#ARIMA for last 5 yrs
setwd("C:\\Users\\kausik\\Documents\\Sindhu\\PGP BABI\\Capstone\\Financial statement analysis\\Data\\")
getwd()
data3= read.csv(file.choose(),header=TRUE)
View(data3)

data3<-ts(data3,start=c(1956,1),end=c(1960,12),frequency = 12)
data3
class(data3)

library(timeSeries)
kpss.test(data3)
plot(log(data3))
plot(diff(log(data3)))
abline(h=0)
kpss.test(diff(log(data3)))

acf(diff(log(data3)))

pacf(diff(log(data3)))

#Model

model<-arima(log(AirPassengers),c(1,1,1),seasonal = list(order=c(1,1,1),period=12))

#using the above arima model let us predict the number of airpassengers for the next 1 yr.

pred<-predict(model,n.ahead=12*1)
predf<-2.718^pred$pred
predf
accuracy(model)
