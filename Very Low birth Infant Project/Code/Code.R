### Raw Data 
Raw=read.csv("Infa.csv",header=T)

### Cleaned Data
Cleaned=read.csv("Infa1.csv",header=T)
head(Cleaned)
attach(Cleaned)

### MODE
mode=function(x){
ax=unique(x)
ax[which.max(tabulate(match(x,ax)))]
}
mode(race )
mode(twn)
mode(meth)
mode(delivery)
mode(vent)
mode(pneumo)
mode( pda)
mode(cld)
mode(pvh)
mode(ivh )
mode(ipe )
mode(dead)


### BARPLOT
par(mfrow=c(4,4))
barplot(table(race),main="race")
barplot(table(twn),main="twn")
barplot(table(meth),main="meth")
barplot(table(delivery),main="delivery")
barplot(table(vent),main="vent")
barplot(table(pneumo),main="pneumo")
barplot(table(pda),main="pda")
barplot(table(cld),main="cld")
barplot(table(pvh),main="pvh")
barplot(table(ivh),main="ivh")
barplot(table(ipe ),main="ipe ")
barplot(table(dead),main="dead")


### logistic regression
l=step(glm(dead~.,Cleaned,family="gaussian"))
summary(l)

### Significant variables from the logistic regression
x=Cleaned[c(1,3,6,7,9,10,16,17,18,27)]
attach(x)

### MEASURE OF CENTRAL TENDENCY
ds=Cleaned[c(1,2,3,4,5,9,10,15)]
head(ds)
mean=colMeans(ds)
vr=var(ds)
sd=sqrt(diag(vr))


### KURTOSIS AND SKEWNESS
k=kurtosis(ds)
sk=skewness(ds)
summary(ds)



### Balanced data using SMOOTE method
library(ROSE)
library(smotefamily)
table(x$dead)
o11=ovun.sample(dead~.,x,method="over")
o1=o11$data
nrow(o1)
table(o1$dead)
write.csv(o1,file="Smoote.csv")
X1=read.csv("Smoote.csv",header=T)
Smoote=X1[,-1]
head(Smoote)


### Balanced data using ROSE method
library(ROSE)
library(smotefamily)
table(x$dead)
r1=ROSE(dead~ ., data =x, seed = 2)
r=r1$data
nrow(r)
table(r$dead)
write.csv(r,file="Rose.csv")
X2=read.csv("Rose.csv",header=T);head(X2)
Rose=X2[,-1]
head(Rose)



### Splitting into Test and Train
set.seed(1234)
library(caTools)
sp=sample.split(x,SplitRatio = 0.70)
tr=subset(x,sp == TRUE)
head(tr)
length(tr$dead)
trx=tr[,-10]
head(trx)
try=tr[,10]
ts=subset(x,sp==FALSE)
head(ts)
length(ts$dead)
tsx=ts[,-10]
head(tsx)
tsy=ts[,10]


### IMBALANCED DATA

#1 logistic regression
g1=glm(dead~.,data=tr,family="binomial")
summary(g1)
p1=predict(g1,tsx,type = "response")
prob=as.data.frame(p1)
prob=round(prob,2)

p11=ifelse(prob>0.5,1,0)
library(caret)
t=table(tsy,p11)
ac1=sum(diag(t))/sum(t)*100
re1=t[1,1]/sum(t[1,])*100
pr1=t[2,2]/sum(t[2,])*100
c=confusionMatrix(as.factor(p11),as.factor(tsy))

#2 decision tree
library(rpart)
library(rpart.plot)
r=rpart(formula = dead~.,data=tr, method="class")
rpart.plot(r)
p2=predict(r,tsx,type="class")
library(caret)
t1=table(tsy,p2)
ac2=sum(diag(t1))/sum(t1)*100
re2=t1[1,1]/sum(t1[1,])*100
pr2=t1[2,2]/sum(t1[2,])*100
c1=confusionMatrix(as.factor(p2),as.factor(tsy))

#3 RandomForest
library(randomForest)
rf=randomForest(dead~.,data=tr)
p3=predict(rf,tsx)
p33=ifelse(p3>0.5,1,0)
library(caret)
t2=table(tsy,p33)
ac3=sum(diag(t2))/sum(t2)*100
re3=t2[1,1]/sum(t2[1,])*100
pr3=t2[2,2]/sum(t2[2,])*100
c2=confusionMatrix(as.factor(p33),as.factor(tsy))

#4 KNN
library(class)
k=knn(trx,tsx,try,k=3)
library(caret)
t3=table(tsy,k)
ac4=sum(diag(t3))/sum(t3)*100
re4=t3[1,1]/sum(t3[1,])*100
pr4=t3[2,2]/sum(t3[2,])*100
c3=confusionMatrix(as.factor(k),as.factor(tsy))

#5 SVM
library(e1071)
s=(tr$dead=as.factor(tr$dead))
sv=svm(dead~.,data=tr)
summary(sv)
p4=predict(sv,tsx)
library(caret)
t4=table(tsy,p4)
ac5=sum(diag(t4))/sum(t4)*100
re5=t4[1,1]/sum(t4[1,])*100
pr5=t4[2,2]/sum(t4[2,])*100
c4=confusionMatrix(as.factor(p4),as.factor(tsy))

# 6 Naive bayes
library(naivebayes)
nb=naive_bayes( dead~.,tr,usekernal=T)
p5=predict(nb,tsx,type="prob")
p55=predict(nb,tsx,type="class")
t5=table(tsy,p55)
ac6=sum(diag(t5))/sum(t5)*100
re6=t5[1,1]/sum(t5[1,])*100;re6
pr6=t5[2,2]/sum(t5[2,])*100
c5=confusionMatrix(as.factor(p55),as.factor(tsy))

#7 Linear Discriminant Analysis
library("MASS")
ld=lda(dead~.,data=tr)
summary(ld)
p7=predict(ld,tsx)$class
t7=table(tsy,p7)
ac8=sum(diag(t7))/sum(t7)*100
re8=t7[1,1]/sum(t7[1,])*100
pr8=t7[2,2]/sum(t7[2,])*100
c7=confusionMatrix(as.factor(p7),as.factor(tsy))

#8 Bagging
library(ipred)
cb=bagging(dead~.,data=tr)
p8=predict(cb,tsx)
t8=table(tsy,p8)
ac9=sum(diag(t8))/sum(t8)*100
re9=t8[1,1]/sum(t8[1,])*100
pr9=t8[2,2]/sum(t8[2,])*100
c8=confusionMatrix(as.factor(p8),as.factor(tsy))

