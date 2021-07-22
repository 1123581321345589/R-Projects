setwd("C:/Users/maloneyi/Desktop/Summer 2021/Machine Learning/HW1")
datatest = read.csv("ohms_test.csv")
datatrain = read.csv("ohms_train.csv")

edistance <- function(x,y,n){
  d=0
  #n is the dimensions of the vector or # of components
  for (i in 1:n){
    d=d+(x[i]-y[i])^2
  }
  d=sqrt(d)
  return(d)
}

# Weighted Average Function, where X is the array of calculated distances,
# and Y is datatest_I (which is the test data's I values), and K is the K nearest neighbors
# The first for loop assigns the numerator of the weighted average, which would be e^x[i], runs for each KNN
# The second for loop assigns the denominator which is the e^{1..k} added together
# Once that loop exits, we put the numerator over the denominator, and multiple by it's true I value
# and then added to prior iterations of the function.
weightedAv <- function(x,y,k){
  numer=0
  denom =0
  
  ret = 0
  for(i in 1:k){
    
    numer = exp(x[i])
    
    for(j in 1:k){
      
      denom = denom + exp(x[j])
      
    }
    
    ret = ret + ((numer / denom)*y[i])
    
    denom = 0
  }
  return(ret)
}


#Calculates RMSE, takes expected values as well as the predicted 
RMSE <- function(exp,pred){
  return(mean((pred - exp)^2))
  
}

#Data tables for test and train without the I column
datatest_NI = datatest[,-2]
datatrain_NI = datatrain[,-2]

#Data tables with just the I column
datatest_I = datatest[,2]
datatrain_I = datatrain[,2]


#User Input for KNN
kk <- as.integer(readline(prompt=paste("How many Neighbors ")))

#Array of distances
ck <- integer()

#Formatted array (See code for further explanation)
d <- numeric()

#Length of the datatest set
nt <- length(datatest_NI)

#Length of the datatrain set
n <- length(datatrain_NI)

#Predicted Values
yhat <- integer()

for(i in 1:nt){
  j = 0
  
  #Calculates Euclidean Distance for all points
  for(j in 1:n) {
    ck[j] <- edistance(datatrain[j, ], datatest[i, ], 1)
  }
  
  #Formats and orders the distance with the real I values
  d <- as.data.frame(lapply(ck, unlist))
  d <- t(d)
  d <- cbind(datatest_I, d)
  d = d[order(d[,2]),]
  
  #Calculates the Weighted Average for the current d[i] value, Sends the array distance column 
  #as well as expected value
  wa = weightedAv(d[,2], d[,1], kk)
  
  #Array of the expected values
  yhat[i] = wa

}

#Data table with prediction vs actual
comp = cbind(yhat,datatest_I)

#RMSE function call
rmse = RMSE(yhat,datatest_I)
