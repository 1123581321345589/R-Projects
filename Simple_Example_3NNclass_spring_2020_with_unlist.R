setwd("~/Desktop/ML_videos")
#Very Simple example class k-NN
data1 <- read.csv("simpleClass_kNN.csv")
#print(data1)
n <- length(data1$x1)
#print(n)
data11 <- data1[which(data1[,1]==1),]
data22 <- data1[which(data1[,1]==2),] 
# Scatterplot
pdf("scatterplot.pdf")
plot(x1 ~ x2,data=data11,col="red",xlab="x1",
     ylab="x2", xlim=c(0,4),ylim=c(0,4), 
     main="plot")
points(x1 ~ x2,data=data22,col="blue")
dev.off()
test1 <- c(2.5,2.5)
#get rid of class to calculate distances
data_no_class <- data1[,-1]
print(data_no_class)
# distance function
edistance <- function(x,y,n){
  d=0
  #n is the dimensions of the vector or # of components
  for (i in 1:n){
    d=d+(x[i]-y[i])^2
  }
  d=sqrt(d)
  return(d)
}
#now loop over examples 
#to find distances from single test point
#declare an array for distance
nc <- ncol(data_no_class)
d <- numeric()
c <- integer()
#print(is.data.frame(d))
#declare an array to hold the top k neighbors
cc <- integer()

for (i in 1:n){
   x <- data_no_class[i,]
   y <- test1
   d[i]=edistance(x,y,nc)
}
#dresults <- data.frame(d=numeric(),c=integer())
#dresults <- data.frame(d,data1$y)
c <- data1$y
dresults <- cbind(d,c)
#print(dresults)
colnames(dresults) <- c("d", "c")
dresults <- dresults[order(unlist(d)),] #ascending order #need to type unlist because r will make d a list within the dataframe and dont want that
#dresults <- dresults[order(d),] #ascending order
#print(dresults)
#use k=3 or 3-NN
kk=3
#cc <- dresults$c[1:kk]
cc <- dresults[1:kk,2]
print(cc)
sum1=0
sum2=0
for (k in 1:kk){
  if (cc[k]==1){
    sum1=sum1+1
  }else{
    sum2=sum2+1
  }  
}
sumall <- cbind(sum1,sum2)
nm <- which(sumall==max(sumall)) #which will get the index of the max, and the index is the same as the class since class 1 is at index one and so on...
#the corresponding class
ck <- nm
print(paste("the class is ", ck))