Auto=read.csv("/media/carson/60E810DFE810B56E/New/#STUDY/#12 2019 Winter/Stat 441/a1/Auto.csv",header=T) 
#library(ISLR)
attach(Auto)


#dim(Auto) #397
#create a vector length same as mpg,assign all elements as 0
mpg01=rep(0,397)
mpg01[mpg>median(mpg)]=1
newAuto=data.frame(Auto,mpg01)



#set odd year as train
train=(year%%2 == 0)
test = !train
data.train=newAuto[train,]
data.test=newAuto[test,]
mpg01.test = mpg01[test]
#fix(data.train)

#lda
library(MASS)
lda.fit=lda(mpg01~ mpg+weight+displacement+cylinders,data=newAuto,subset = train)
lda.pred = predict(lda.fit, data.test)
mean(lda.pred$class != mpg01.test)

#qda
qda.fit=qda(mpg01~ mpg+weight+displacement+cylinders,data=newAuto,subset = train)
qda.fit
qda.pred = predict(qda.fit, data.test)
mean(qda.pred$class != mpg01.test)

#logistic
#p160 in ISLR
glm.fit=glm(mpg01~ mpg+weight+displacement+cylinders,data=newAuto,subset = train)
glm.probs=predict(glm.fit,type="response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mean(glm.pred != test)
summary(glm.fit)
