setwd("~/Desktop/ML_videos/Linear_CV_4")
data_train <- read.csv(file = "ohms_train.csv", head = TRUE, sep = ",")
n <- nrow(data_train)
x0 <- numeric()
y <- data_train$V
x1 <- data_train$I
#add column of data which are all ones
for (i in 1:n){
  x0[i]=1
}
data_trainxx <- cbind(y,x0,x1)
data_trainx <- data.frame(data_trainxx)
colnames(data_trainx) <- c("y","x0","x1")
#4-Fold CV
index1=sample(1:n,4,replace=F)
subdata1 <- data.frame(value1=numeric(),value2=numeric(),
                       value3=numeric())
for (i in 1:4){
  #this is the index in the orginial data
  j=index1[i]
  #this grabs the jth row of the orginal data
  #and puts that row in the subset
  subdata1[i,] <- data_trainx[j,]
}
#rename columns again
colnames(subdata1) <- c("y","x0","x1")
#save them in csv file
write.table(subdata1, file = "data_sub1.csv", append = F, 
            quote = F, sep = ",",eol = "\n", na = "NA", 
            dec = ".", row.names = F,col.names = T)
#now we need to get rid of the lines form orginal
#data that we put in subset
#make a loop for this
for (i in 4:1){
  j=index1[i]
  data_trainx <- data_trainx[-j,]
}

#make sure dataint does not include those lines of 
#data by checking the length of one of the columns
#which we also need for next subset
n2 <- length(data_trainx$y)

#can also do dimensions, printing dimensions is just for checking to make sure it works properly.
d <- dim(data_trainx)
print(d)
index2=sample(1:n2,4,replace=F)
index2=sort(index2)

subdata2 <- data.frame(value1=numeric(),value2=numeric(),
                       value3=numeric())
for (i in 1:4){
  #this is the index in the orginial data
  j=index2[i]
  #this grabs the jth row of the orginal data
  #and puts that row in the subset
  subdata2[i,] <- data_trainx[j,]
}
#rename columns again
colnames(subdata2) <- c("y","x0","x1")
#save them in csv file
write.table(subdata2, file = "data_sub2.csv", append = F, 
            quote = F, sep = ",",eol = "\n", na = "NA", 
            dec = ".", row.names = F,col.names = T)

for (i in 4:1){
  j=index2[i]
  data_trainx <- data_trainx[-j,]
}
#make sure dataint does not include those lines of 
#data by checking the length of one of the columns
#which we also need for next subset
n3 <- length(data_trainx$y)
#can also do dimensions
d <- dim(data_trainx)
print(d)
subdata3 <- data.frame(value1=numeric(),value2=numeric(),
                       value3=numeric())
for (i in 1:n3){
  #this grabs the jth row of the orginal data
  #and puts that row in the subset
  subdata3[i,] <- data_trainx[i,]
}
#rename columns again
colnames(subdata3) <- c("y","x0","x1")
#save them in csv file
write.table(subdata3, file = "data_sub3.csv", append = F, 
            quote = F, sep = ",",eol = "\n", na = "NA", 
            dec = ".", row.names = F,col.names = T)

train <- data.frame(value1=numeric(),value2=numeric(),
                    value3=numeric())
test <- data.frame(value1=numeric(),value2=numeric(),
                   value3=numeric())


for (i in 1:3){
  #create the name for which subset to get
  name <- paste('data_sub',i,'.csv',sep ="")
  for (j in 1:3){
    if (i==j){
      #since we want this subset to be the test set make it
      #equl to test by reading in the csv file
      test <- read.csv(name,as.is=T)
    }else{
      #want the rest of the subsets to be in training set
      name2 <- paste('data_sub',j,'.csv',sep ="")
      #keep adding the lines of data by rbind which is row bind
      addtrain <- read.csv(name2,as.is=T) 
      train <- rbind(train,addtrain)
    }
  }
  #now save each training and test set in csv file
  name3 <- paste('train',i,'.csv',sep ="")
  write.table(train, file =name3, append = F, 
              quote = F, sep = ",",eol = "\n", na = "NA", 
              dec = ".", row.names = F,col.names = T)
  name4 <- paste('test',i,'.csv',sep ="")
  write.table(test, file =name4, append = F, 
              quote = F, sep = ",",eol = "\n", na = "NA", 
              dec = ".", row.names = F,col.names = T)
  #re-declare data frames to make them empty
  train <- data.frame()
  test <- data.frame()
}
train <- data.frame()
test <- data.frame()

linear <- function(x,theta){
  prod <- sum(x*theta)
  return(prod)
}


sumi=0
sums=0
sumR=0
for (j in 1:3){
  name <- paste('train',i,'.csv',sep ="")
  name2 <- paste('test',i,'.csv',sep ="")
  train <- read.csv(name,as.is=T)
  test <- read.csv(name2,as.is=T)
lin.R <- lm(train[,1] ~ train[,3])
print(lin.R)
inter=lin.R$coef[1]
slope=lin.R$coef[2]
print(slope)
print(inter)
sumi=sumi+inter
sums=sums+slope
dimm <- ncol(train)-1
theta <- matrix(0,nrow=1,ncol=dimm)
theta[1]=inter
theta[2]=slope
  for (j in 1:4) {
    x00 <- test[j,2]
    x11 <- test[j,3]
    yy <- test[j,1]
    x <- c(x00,x11)
    predy <- linear(x,theta)
    errorpred_sq=(yy-predy)^2
  }
  #Root Mean Squared Error
  RMS=sum(errorpred_sq)/4
  sqRMS=sqrt(RMS)
  sumR=sumR+RMS
  
  train <- data.frame()
  test <- data.frame()
  
}

sumi=sumi/3
sums=sums/3
sumR=sumR/3
print(paste("Average inter.",sumi))
print(paste("Average slope.",sums))
print(paste("Average RMS",sumR))
#NOTES:
#In r it will treat multiplication of vectors as dot product automatically.
#The train and test are "Data Frames" where you can take the data colum by colum or row by row and add to the train or testing "Data Frame"
#Using lm(y ~ x) x is input y is output but you write it output first then input!
#Use summary for all the info of a function or dataset
#abline is adding a line to a lm model. 
#RMS = sqrt((sum(i-m)((^yi-yi)^2)/m))