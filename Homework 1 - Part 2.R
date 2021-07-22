setwd("~/Machine Learning/MachineLearning/Homework 1")

training_data <- read.csv("winedata.csv")
training_data_NC = training_data[,-1]
test_data <- read.csv("winedata_test.csv")
test_data_NC = test_data[,-1]
n=length(training_data$c)
d=dim(training_data)
print(d)

edistance <- function(x,y,n){
  d=0
  #n is the dimensions of the vector or # of components
  for (i in 1:n){
    d=d+(x[i]-y[i])^2
  }
  d=sqrt(d)
  return(d)
}

accuracyColumn=c()
upperBound=100

for(numberOfNeighbors in 1:upperBound){
  
  d <- numeric()
  ck <- integer()
  cc <- integer()
  ckSort <- integer()
  nt <- length(test_data_NC$Alcohol)
  nc <- ncol(test_data_NC)
  n <- length(training_data_NC$Alcohol)
  datatest_class<-test_data[,1]
  datatrain_class<-training_data[,1]
  ckc = integer()
  prediction <- numeric()
  
  for(i in 1:nt){
    
    j = 0
    for(j in 1:n){
      ck[j]<- edistance(test_data_NC[i,], training_data_NC[j,],13)
    }
    ckSort <- as.data.frame(lapply(ck, unlist))
    ckSort = t(ckSort)
    ckSort = cbind(datatrain_class, ckSort)
    ckSort = ckSort[order(ckSort[,2]), ]
    
    ckc = ckSort[,1]
    
    classes <- numeric(numberOfNeighbors)
    for(k in 1:numberOfNeighbors){
      classes[ckc[k]] = classes[ckc[k]] + 1
    }
    k = 0
    max = 0
    retindex = 0
    for(k in 1:numberOfNeighbors){
      if(classes[k] > max){
        max = classes[k]
        retindex = k
      }
    }
    prediction[i] <- retindex
  }
  outPut= cbind(datatest_class, prediction)
  
  allresults <- data.frame(outPut[,1], outPut[,2])
  # colnames(allresults) <- c("actualc", "predictc")
  # write.table(allresults, file = "resultsinsects.csv", append = F, 
  #             quote = F, sep = ",",eol = "\n", na = "NA", 
  #             dec = ".", row.names = F,col.names = T)
  t <- table(allresults)
  # print(t)
  nd <- sum(diag(t))
  n <- sum(t)
  OA <- nd/n
  # print(OA)
  
  accuracyColumn=c(accuracyColumn,OA)
  print(paste("Completed KNN =",numberOfNeighbors))
  
  # A1_t <- sum(t[1,])
  # if(A1_t==0){A1_t=1}
  # A2_t <- sum(t[2,])
  # if(A2_t==0){A2_t=1}
  # A1_n <- t[1,1]
  # A2_n <- t[2,2]
  # Ac_1 <- A1_n/A1_t
  # Ac_2 <- A2_n/A2_t
  # print(Ac_1)
  # print(Ac_2)
  # #mathews correlation coefficient
  # topfactor <- (A1_n)*(A2_n)-t[1,2]*t[2,1]
  # f1 <- A1_n+t[1,2]
  # f2 <- A1_n+t[2,1]
  # f3 <- A2_n+t[1,2]
  # f4 <- A2_n+t[2,1] 
  # MCC <- topfactor/(sqrt(f1*f2*f3*f4))
  # print(MCC)
  # accresults <- data.frame(OA,Ac_1,Ac_2,MCC)
  # print(accresults)
  # colnames(accresults) <- c("Overall", "Acc_1","Acc_2","MCC")
  # write.table(accresults, file = "accintsects.csv", append = F, 
  #             quote = F, sep = ",",eol = "\n", na = "NA", 
  #             dec = ".", row.names = F,col.names = T)
  #file.remove("resultsintsects.csv")
}
print(accuracyColumn)
errorColumn=c()
for(i in 1:length(accuracyColumn)){
  errorColumn=c(errorColumn,1-accuracyColumn[i])
}
plot(c(1:upperBound), accuracyColumn, main="Accuracy by Number of Neighbors", xlab="Number of Neighbors", ylab="Overall Accuracy")
plot(c(1:upperBound), errorColumn, main="Error by Number of Neighbors", xlab="Number of Neighbors", ylab="Error")