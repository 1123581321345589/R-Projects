#install.packages("TSA")
#x<-c(1,2,3,4,5)
#c(1,2,3,4,5)->y
#y
library(TSA)
data("larain")
head(larain, 10)
mean(larain)
median(larain)
sd(larain)
var(larain)
plot(larain, col="red")

x = time(larain)
y= (larain)
larain.lm = lm(y~x)
summary(larain.lm)

#Judging by the Plot, there doesn't seem to be much correlation
#between the year and the rain fall.

#Part Four

qqnorm(larain.lm$residuals)
qqline(larain.lm$residuals , col = 'Green')

qqplot(x, y)

#the plots dont follow the qqline very well, so they do not come from the same distribution

