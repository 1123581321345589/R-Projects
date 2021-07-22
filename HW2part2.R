setwd("C:/Users/maloneyi/Desktop/Summer 2021/Machine Learning/HW2")
training_data = read.csv("winedata.csv")
test_data = read.csv("winedata_test.csv")


library(rpart)

cfit = rpart(c~., data=training_data, method = "class")

name1="decisionTREE_wine"
num=1
ext=".pdf"
name2=paste(name1,num,ext,sep='')
pdf(name2)
plot(cfit,uniform=T,main="Decision Tree for Wine data")
text(cfit,use.n=T,all=T,cex=.6)
dev.off()
pred1 <- predict(cfit,data=test_data,type="c")

t <- table(predict(cfit, test_data, type = "class"), test_data[, "c"])

print(t)
d <- sum(diag(t))
n <- sum(t)
overall_ac=d/n
print(overall_ac)
n1 <- sum(t[1,])
ns <- t[1,1]
Acc1=ns/n1
print(Acc1)
n2 <- sum(t[2,])
ns <- t[2,2]
Acc2=ns/n2
print(Acc2)
n3 <- sum(t[3,])
ns <- t[3,3]
Acc3=ns/n3
print(Acc3)