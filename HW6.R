library(forecast)
dataset <- c(6.4,5.6,7.8,8.8,11.0,11.6, 16.7, 15.3, 21.6, 22.4)

ESM = function(a, data) {
  len = length(data)
  Yt = rep(0, len)
  Yt[2] = data[1]
  for(i in 3:len){
    Yt[i] = a*data[i-1]+(1-a)*Yt[i-1]
  }
  return(Yt)
}

a = .4
re = ESM(a, dataset)
result = ts(re)
plot(dataset,type = "l")
points(result, col = "red", type = "l")
rmse = (sum((result-dataset)^2)/length(dataset))

ESM2 = function(a, data) {
  len = length(data)
  Yt = rep(0,len)
  Yt[2] = data[1]
  for(i in 3:len){
    Yt[i] = a*data[i-1]+(1-a)*Yt[i-1]
  }
prediction = rep(10,15)
h = 1
for(i in 10:15){
  if(i < 11){
    prediction[i] = a*dataset[10]+(1-a)*Yt[i-1]
    h + h + 1
  }
  else{
    prediction[i] = a*dataset[10]+(1-a)*prediction[i+h-1]
    h + h + 1
  }
}
 return(prediction)
}
a = .1

re2 = ESM2(a, dataset)
result2 = re2[11:15]
result2 = ts(re2)
dataset = ts(dataset)
plot(dataset, type = "l", xlim = c(1,15))
points(result2, col = "red", type = "l")

ESM3 = function(a, data) {
  len = length(data)
  Yt = rep(0, len)
  Yt[2] = data[1]
  for(i in 3:len){
    Yt[i] = a*data[i-1]+(1-a)*Yt[i-1]
  }
  rsme = (sum((Yt - data)^2))/length(data)
  
  Yt2 = rep(0,len)
  Yt2[2]=data[1]
  for(e in 1:8){
    ai = e/10
    
    for(i in 3:len){
      Yt2[i] = (a+ai)*data[i-1]+(1-(a+ai))*Yt2[i-1]
    }
    rsme2 = (sum((Yt2-data)^2))/length(data)
    
    if(rsme2 < rsme){
      rsme = rsme2
      Yt = Yt2
    }
  }
  
  return(Yt)
}

a = .1
re = ESM3(a, dataset)
result = ts(re)
plot(dataset,type = "l")
points(result, col = "red", type = "l")
rmse = (sum((result-dataset)^2)/length(dataset))

