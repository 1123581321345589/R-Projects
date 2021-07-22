data <- data.frame(x1 = c(64, 66, 68, 69, 73),
                   x2 = c(580, 570, 590, 660, 600),
                   x3 = c(29, 33, 37, 46, 55))

custom_covariance = function(x, y){
  rows=nrow(data)
  sum=0
  for(i in 1:rows){
    sum=sum+((x[i]-mean(x))*(y[i]-mean(y)))
  }
  return(sum/(rows-1))
}

covariance=data.frame(row.names=colnames(data))
for(j in colnames(data)){
  row=c()
  for(k in colnames(data)){
    row=c(row,(custom_covariance(data[[j]], data[[k]])))
  }
  covariance=rbind(covariance, row)
}
row.names(covariance)=colnames(data)
colnames(covariance)=colnames(data)

print(covariance)