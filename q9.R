layout(matrix(1))
library(ISLR)
names(Weekly)
pairs(Weekly)
cor(Weekly[,-9])
attach(Weekly)
plot(Lag1)

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Weekly,family = binomial)
summary(glm.fit)
plot(glm.fit)

glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]


contrasts(Direction)
glm.pred=rep("Down",1089)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction)
#train and test
train=(Year<2009)
test=Weekly[!train,]
Direction.test=Direction[!train]
glm.fit=glm(Direction~Lag2,data=Weekly,family = binomial,subset=train)
glm.probs=predict(glm.fit,test,type="response")
dim(test) #104 9
glm.pred=rep("Down",104)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.test)

library(MASS)
lda.fit=lda(Direction~Lag2,data=Weekly,subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit,test)
lda.class=lda.pred$class
table(lda.class,Direction.test)

#qda
qda.fit=qda(Direction~Lag2,data=Weekly,subset=train)
qda.fit
qda.class=predict(qda.fit,test)$class
table(qda.class,Direction.test)

#diff terms
glm.fit=glm(Direction~Lag6+Lag3*Lag2*Lag4,data=Weekly,family = binomial,subset=train)
glm.probs=predict(glm.fit,test,type="response")
dim(test) #104 9
glm.pred=rep("Down",104)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.test)
