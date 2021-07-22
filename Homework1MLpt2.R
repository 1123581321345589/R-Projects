setwd("~/")
data("iris")
n=length(iris[,1])
d=dim(iris)
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

xx = sample(1:n,50,replace=F)
datatest <- iris[xx,]
datatest_NC <- datatest[,-5]
datatest_class <- datatest[,5]
datatrain <- iris[-xx,]
datatrain_class <- datatrain[,5]
datatrain_NC <- datatrain[,-5]

kk <- as.integer(readline(prompt=paste("How many Neighbors ")))
d <- numeric()
ck <- numeric()
cc <- integer()
nt <- length(datatest_NC$Sepal.Length)
nc <- ncol(datatest_NC)
n <- length(datatrain_NC$Sepal.Length)

prediction <- numeric()

for(i in 1:nt){
  j = 0
  for(j in 1:n) {
    ck[j] <- edistance(datatrain_NC[j, ], datatest_NC[i, ], 4)
  }
  d <- as.data.frame(lapply(ck, unlist))
  d <- t(d)
  d <- cbind(datatrain_class, d)
  d = d[order(d[,2]),]
  
  e <- d[,1]
  e <- cbind(e)
  k = 0
  
  classes <- numeric(kk)
  
  for(k in 1:kk){
    classes[e[k]] = classes[e[k]] + 1
  }
  k = 0
  max = 0
  retindex = 0
  for(k in 1:kk){
    if(classes[k] > max){
      max = classes[k]
      retindex = k
    }
  }
  
  prediction[i] <- retindex
  
}

test <- cbind(datatest_class, prediction)

allresults <- data.frame(test[,1], test[,2])
colnames(allresults) <- c("actualc", "predictc")
write.table(allresults, file = "resultsiris.csv", append = F, 
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
#added in for class 3
A3_t <- sum(t[3,])
if(A3_t==0){A3_t=1}
A3_n <- t[3,3]
Ac_3 <- A3_n/A3_t
print(Ac_3)

warnings()