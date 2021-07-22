setwd("~/")
insect <- read.csv("insect_new_class.csv")
n=length(insect$c)
d=dim(insect)
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
xx=sample(1:n,50,replace=F)
datatest <- insect[xx,]
datatest_NC <- datatest[,-1]
datatest_class <- datatest[,1]
datatrain <- insect[-xx,]
datatrain_class <- datatrain[,1]
datatrain_NC <- datatrain[,-1]

kk <- as.integer(readline(prompt=paste("How many Neighbors ")))
d <- numeric()
ck <- numeric()
cc <- integer()
nt <- length(datatest_NC$abd)
nc <- ncol(datatest_NC)
n <- length(datatrain_NC$abd)
#datatest_NC <- sample (smaller set)

#datatrain_NC <- the rest of the data set

for(i in 1:nt){
  j = 0
  for(j in 1:n) {
  ck[j] <- edistance(datatrain_NC[j,], datatest_NC[i,], 2)
  }
  d <- as.data.frame(lapply(ck, unlist))
  d <- t(d)
  d <- cbind(datatrain_class, d)
  d = d[order(d[,2]), ]
  
  e <- d[,1]
  
  sum1 <- 0
  sum2 <- 0
  
  for(k in 1:kk) {
    if(e[k] == 1){
      sum1 = sum1 + 1;
    }
    else if(e[k] == 2){
      sum2 = sum2 + 1;
    }
  }
  if(sum1 >= sum2){
    cc[i] <- 1
  }
  else{
    cc[i] <- 2
  }
  
  
}
cc <- cbind(datatest, cc)

#cc is the prediction for the class

#???????????????????????????

allresults <- data.frame(datatest$c, cc$cc)
colnames(allresults) <- c("actualc", "predictc")
write.table(allresults, file = "resultsintsects.csv", append = F, 
            quote = F, sep = ",",eol = "\n", na = "NA", 
            dec = ".", row.names = F,col.names = T)
t <- table(allresults)
print(t)
nd <- sum(diag(t))
n <- sum(t)
OA <- nd/n
print(OA)
A1_t <- sum(t[1,])
if(A1_t==0){A1_t=1}
A2_t <- sum(t[2,])
if(A2_t==0){A2_t=1}
A1_n <- t[1,1]
A2_n <- t[2,2]
Ac_1 <- A1_n/A1_t
Ac_2 <- A2_n/A2_t
print(Ac_1)
print(Ac_2)
#mathews correlation coefficient
topfactor <- (A1_n)*(A2_n)-t[1,2]*t[2,1]
f1 <- A1_n+t[1,2]
f2 <- A1_n+t[2,1]
f3 <- A2_n+t[1,2]
f4 <- A2_n+t[2,1] 
MCC <- topfactor/(sqrt(f1*f2*f3*f4))
print(MCC)
accresults <- data.frame(OA,Ac_1,Ac_2,MCC)
print(accresults)
colnames(accresults) <- c("Overall", "Acc_1","Acc_2","MCC")
write.table(accresults, file = "accintsects.csv", append = F, 
            quote = F, sep = ",",eol = "\n", na = "NA", 
            dec = ".", row.names = F,col.names = T)
#file.remove("resultsintsects.csv")
warnings()

