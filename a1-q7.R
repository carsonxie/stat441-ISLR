 ####q7
College=read.csv("/media/carson/60E810DFE810B56E/New/#STUDY/#12 2019 Winter/Stat 441/a1/College.csv",header=T,na.strings="?")
fix(College)
attach(College)
College=College[2:19]
#predict app use other, so Y is app
train = (Apps%%2 == 0)
test = !train
data.train=College[train,]
data.test=College[test,]

#LSE
lm.fit = lm(Apps~., data=data.train)
summary(lm.fit)
lm.pred = predict(lm.fit, data.test)
ols=mean((data.test[, "Apps"] - lm.pred)^2)
  
#ridge p252
library(glmnet)
grid=10^seq(10,-2,length=100)
#regfit.full=regsubsets(Apps~.,data.train)
#summary(regfit.full)
set.seed(1)
xtrain=model.matrix(Apps~.,data.train)[,-1]
ytrain=data.train$Apps
xtest=model.matrix(Apps~.,data.test)[,-1]
ytest=data.test$Apps

ridge.mod=glmnet(xtrain,ytrain,alpha=0,lambda=grid,thresh=1e-12)
cv.out=cv.glmnet(xtrain,ytrain,alpha=0)
plot(cv.out) 
bestlamdba=cv.out$lambda.min
ridge.pred=predict(ridge.mod,s=bestlamdba,newx =xtest )
bestlamdba
re=mean((ridge.pred-ytest)^2)
#lasso
lasso.mod=glmnet(xtrain,ytrain,alpha=1,lambda=grid,thresh=1e-12)
cv.out=cv.glmnet(xtrain,ytrain,alpha=1)
plot(cv.out)
bestlamdba=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlamdba,newx=xtest)
bestlamdba
la=mean((lasso.pred-ytest)^2)
out=glmnet(xtrain,ytrain,alpha=1,lambda = grid)
lasso.coef=predict(out,type="coefficients",s=bestlamdba)[1:20]
lasso.coef
