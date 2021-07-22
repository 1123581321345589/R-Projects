library(TSA)

dataSet = data
plot(data, type = "l")
dataTS = ts
plot(dataTS[,2], type = "l")
dataTS = ts(diff(data[,2]))
plot(dataTS, type = "l")

acf(dataTS,30)
pacf(dataTS,30)

model_ma = auto.arima(dataTS)
model_ma
plot(forecast(model_ma))
yhat = forecast(model_ma)
points(yhat$mean,col="red",type="l")

train_ma = dataTS[1:274]
test_ma = dataTS[275:365]
model_train = arima(train_ma,order = c(0,0,2))

plot(dataTS, type = "l")

yhat1 = predict(model_train, n.ahead = 91, ci =.95)
points(yhat1$pred,col="Red",type="l")

rss = sum((test_ma-yhat1$mean)^2)
MSE = mean((test_ma-yhat1$mean)^2)