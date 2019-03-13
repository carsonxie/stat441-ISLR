#stat 441 assignment1 code
#
#
Auto=read.csv("/media/carson/60E810DFE810B56E/New/#STUDY/#12 2019 Winter/Stat 441/a1/Auto.csv",header = T,na.strings="?") 
Auto = na.omit(Auto) #omit the 
attach(Auto)
autofit = lm(mpg ~ horsepower)
summary(autofit)

plot(horsepower,mpg,col="blue")
abline(autofit,col="red")


#new data for prediction
new.df = data.frame(horsepower=c(98))
predict(autofit,new.df)
predict(autofit,new.df,interval = "confidence")
predict(autofit,new.df,interval = "prediction")
layout(matrix(c(1,2,3,4),2,2))
plot(autofit)


#q4
Car = read.table("/media/carson/60E810DFE810B56E/New/#STUDY/#12 2019 Winter/Stat 441/a1/Carseats.txt",header=T,na.strings="?")
fix(Car)
attach(Car) #before call lm() must attach and fix the data
carfit = lm(Sales ~ Price+Urban+US)
summary(carfit)

carfit2=lm(Sales ~ Price+Urban)
summary(carfit2)
#CI for the coefficient
confint(carfit2,level=0.95)

#layout(matrix(c(1,2,3,4),2,2))
plot(carfit2)
library(car)
outlierTest(carfit2)


#q6
set.seed(1)
n=100
X = rnorm(n)
error = rnorm(n)
beta0 = 1
beta1 =2   
beta2=3
beta3=4
Y = beta0 + beta1*X + beta2*X^2 + beta3*X^3 + error

#need leaps library for regsubsets()
library(leaps)
XYdata = data.frame(X,X^2,X^3,X^4,X^4,X^5,X^6,X^6,X^7,X^8,X^9,X^10,Y)
regfit.full = regsubsets(Y~.,XYdata)
reg.summary = summary(regfit.full)
names(reg.summary)
#plot RSS
#par(mfrow=c(1,1))
layout(matrix(c(1,2,3,4),2,2))
plot(reg.summary$rss , xlab =" Number of Variables ", ylab =" RSS ",type ="l")
points (9, reg.summary$rss [9] , col =" red ", cex =2, pch =20)
which.min(reg.summary$rss) #9
plot(reg.summary$adjr2 ,xlab =" Number of Variables ",ylab =" Adjusted RSq ", type ="l")
which.max(reg.summary$adjr2)
points (5, reg.summary$adjr2 [5] , col =" red ", cex =2, pch =20)

#plot Cp and BIC
plot(reg.summary$cp,xlab="Number of Variables ", ylab =" Cp",type='l')
which.min (reg.summary$cp)
points (5,reg.summary$cp [5] , col =" red ", cex =2, pch =20)
coef(regfit.full,5)
plot(reg.summary$bic,xlab="Number of Variables ", ylab =" BIC",type='l')
which.min (reg.summary$bic) #output is 3 here
points (3,reg.summary$bic[3] , col =" red ", cex =2, pch =20)

plot(regfit.full,scale = "bic")
####
#part d
regfit.fwd = regsubsets(Y~.,XYdata,method="forward")
summary(regfit.fwd)
regfit.bwd = regsubsets(Y~.,XYdata,method="backward")
summary(regfit.bwd)
#plot the 
plot(regfit.bwd)


####
#part e 
#lasso
library(glmnet)
grid=10^seq(10,-2,length=100)
x=model.matrix(Y~.,XYdata)[,-1]
y=XYdata$Y
lasso.mod = glmnet(x,y,alpha=1,lambda = grid)
lasso.mod$lambda[50]
coef(lasso.mod)[1:50]

#set train and test data
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]
#cross validation
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
par(mfrow=c(1,1))
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
# fit the model using whole data and use the best lambda
out=glmnet(x,y,alpha = 1)
predict(out,type="coefficients",s=bestlam)[1:13,]

#q6 part f
newY = beta0 + 7*X^7 + error
newYX = data.frame(X^7,newY,error)
#best subset selection by cv
reg.best=regsubsets(newY~.,data=newYX)
coef(reg.best,2)
#lasso
x2=model.matrix(newY~.,newYX)[,-1]
y2=newYX$newY
lasso.mod2=glmnet(x2,y2,alpha = 1,lambda=grid)
plot(lasso.mod2)


