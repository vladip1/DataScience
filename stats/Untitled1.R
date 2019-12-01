#Linear regression

data<-Orange
head(data)
mod<-lm(age ~ circumference, data)
summary(mod)
yhat<-predict(mod,data)
plot(yhat ~ data$age)
scatter.smooth(yhat ~ data$age)




data<-iris
head(data)

data$y <- ifelse(data$Species == "versicolor", 1, 0)

data$Species <- NULL

rnd<-sample(seq(1,nrow(data)))

data<-data[rnd,]

mod <- glm(factor(y) ~ Sepal.Width + Petal.Length, family="binomial", data = data)

summary(mod)

library(car)
vif(mod)

pred<-predict(mod, data, type="response")
hist(pred)

yhat<-ifelse(pred>=0.60,1,0)
table(yhat=yhat, y=data$y)
#accuracy<-
#accuracy


dts<-ts(AirPassengers, start = c(1949,1), end=c(1960, 12), frequency = 12)
head(dts)

stationary_dts<-decompose(dts)
plot(stationary_dts)

acf(dts)

pacf(dts)

View(nottem)

head(nottem)

df<-nottem

dts <- ts(nottem, start = c(1920,1), end = c(1939,12), frequency = 12)

head(dts)

stationary_dts<-decompose(dts)

plot(stationary_dts)

acf(dts)

acf(dts)

dts_arima <- arima(dts, order=c(3,0,0))
dts_arima

library(forecast)
dts_fit <- forecast(dts_arima)
dts_fit

plot(dts_fit)

dts_autoarima <- auto.arima(dts)
dts_autoarima

dts_autoforecast <- forecast(dts_autoarima)
dts_autoforecast
plot(dts_autoforecast)
