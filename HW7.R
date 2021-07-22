#start of 2
library(fpp2)
data("books")
data = books
plot(data[,1])
plot(data[,2])
a <- ses(data[,1])
b <- ses(data[,2])
plot(a)
plot(b)

paper<-data[,1]
hard<-data[,2]
m1 = a
m2 = b

RMSE_paperback<-sqrt(mean(m1$residuals^2))
RMSE_hard<-sqrt(mean(m2$residuals^2))

#start of 3

holt(paper,4)
holt(hard,4)

RMSE_paperbackHOLT<-sqrt(mean(holt(paper,4)$residuals^2))
RMSE_hardHOLT<-sqrt(mean(holt(hard,4)$residuals^2))

plot(holt(paper,4))
plot(holt(hard,4))

#start of 4

data2 = ts(Homework7Dataset, frequency = 365)

plot(data2[,2])

plot(holt(data2[,2], 10))

plot(holt(data2[,2], damped = TRUE))

points(predict(HoltWinters(data2),10),type = "l", col = "red")

#for part 1
yt1 = data[,1]
length(yt1)
t1 = 1:30

model1 = lm(yt1~t1)
summary(model1)
