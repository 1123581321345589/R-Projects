
data = read.csv("ohms_train.csv")


k = 3
n = length(data[,1])
setSize = n/k

xx=sample(1:n,setSize,replace=F)
dataF1 = data[xx,]
dataF1_NI = dataF1[,1]
datarest = data[-xx,]

xx2=sample(1:length(datarest[,1]),setSize,replace=F)
dataF2 = datarest[xx2,]
dataF2_NI = dataF2[,1]
dataF3 = datarest[-xx2,]
dataF3_NI = dataF3[,1]

dataF1_I = dataF1[,2]
dataF2_I = dataF2[,2]
dataF3_I = dataF3[,2]


edistance <- function(x,y,n){
  d=0
  #n is the dimensions of the vector or # of components
  for (i in 1:n){
    d=d+(x[i]-y[i])^2
  }
  d=sqrt(d)
  return(d)
}

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

RMSE <- function(exp,pred){
  return(mean((pred - exp)^2))
  
}

rmseArray = numeric()
slopeArray = numeric()
interceptArray = numeric()

#Array of distances
ck <- integer()

n = length(dataF1_NI)
#Formatted array (See code for further explanation)
d <- numeric()

#Length of the datatest set
nt <- length(datarest[,1])

#Predicted Values
yhat <- integer()

datatrain = numeric()
datatest = numeric()
z=0
i=0
for(z in 1:k){

  if(z == 1){
    datatest = as.data.frame(lapply(dataF1_NI, unlist))
    datatest = t(datatest)
    
    datatest_I = as.data.frame(lapply(dataF1_I, unlist))
    datatest_I = t(datatest_I)

    datatrain = c(dataF2_NI,dataF3_NI)
    datatrain = as.data.frame(lapply(datatrain, unlist))
    datatrain = t(datatrain)

    datatrain_I = c(dataF2_I,dataF3_I)
    datatrain_I = as.data.frame(lapply(datatrain_I, unlist))
    datatrain_I = t(datatrain_I)

  }
  else if (z == 2){
    datatest = as.data.frame(lapply(dataF2_NI, unlist))
    datatest = t(datatest)
    
    datatest_I = as.data.frame(lapply(dataF2_I, unlist))
    datatest_I = t(datatest_I)

    datatrain = c(dataF1_NI,dataF3_NI)
    datatrain = as.data.frame(lapply(datatrain, unlist))
    datatrain = t(datatrain)

    datatrain_I = c(dataF1_I,dataF3_I)
    datatrain_I = as.data.frame(lapply(datatrain_I, unlist))
    datatraint_I = t(datatrain_I)

  }
  else{
    datatest =as.data.frame(lapply(dataF3_NI, unlist))
    datatest = t(datatest)
    
    datatest_I = as.data.frame(lapply(dataF3_I, unlist))
    datatest_I = t(datatest_I)
    
    datatrain = c(dataF1_NI,dataF2_NI)
    datatrain = as.data.frame(lapply(datatrain, unlist))
    datatrain = t(datatrain)

    datatrain_I = c(dataF1_I,dataF2_I)
    datatrain_I = as.data.frame(lapply(datatrain_I, unlist))
    datatrain_I = t(datatrain_I)
  }

  
  
  for(i in 1:n){
    j = 0
  
    #Calculates Euclidean Distance for all points
    for(j in 1:nt) {
      ck[j] <- edistance(datatrain[j, ], datatest[i, ], 1)
    }
  
    #Formats and orders the distance with the real I values
    d <- as.data.frame(lapply(ck, unlist))
    d <- t(d)
    d <- cbind(datatrain_I, d)
    d = d[order(d[,2]),]
  
    #Calculates the Weighted Average for the current d[i] value, Sends the array distance column 
    #as well as expected value
    wa = weightedAv(d[,2], d[,1], n)
  
    #Array of the expected values
    yhat[i] = wa
  
  }
  
  model = lm(yhat~datatest_I)
  
  interceptArray[z] = coef(model)[1]
  
  slopeArray[z]= coef(model)[2]
  
  rmseArray[z] = RMSE(yhat,datatest_I)

}

avRMSE = (sum(rmseArray))/3

avINT = (sum(interceptArray))/3

avSLOPE = (sum(slopeArray))/3

print(avRMSE)
print(avINT)
print(avSLOPE)




