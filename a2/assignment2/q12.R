library(ISLR)
attach(Hitters)

#a remove those rows salary is NA 
hit = Hitters[!is.na(Hitters$Salary),]
#log transform the salary
hit$Salary = log(hit$Salary)
detach(Hitters)
attach(hit)

#b train and test
train = hit[1:200,]
test = hit[201:nrow(hit),]

#c gbm
library(gbm)
set.seed(1)
MSE.train = rep(0,11)
MSE.test = rep(0,11)
shrink = c(0.001, 0.003,0.005, 0.1, 0.15, 0.2, 0.3, 0.35, 0.4,0.45,0.5)
for (i in 1:length(shrink)){
  #shrink = shrink[i] use this access will return null in MSE
  boost.hit = gbm(Salary~., data = train, distribution = "gaussian",
                n.tree = 1000 , shrinkage = shrink[i])
  boost.pred = predict(boost.hit, newdata = train, n.trees = 1000)
  boost.pred.test = predict(boost.hit, newdata = test, n.trees = 1000)
  MSE.train[i] = mean((boost.pred- train$Salary)^2)
  MSE.test[i] = mean((boost.pred.test- test$Salary)^2)
  
}
MSE.test
MSE.train
par(lty = 2, pch=17)
plot(shrink, MSE.test,type = "b")
plot(shrink, MSE.train, type = "b")

#test base case for the loop
boost.hit = gbm(Salary~., data = train, distribution = "gaussian",
                n.tree = 1000 , shrinkage = 0.1)
boost.pred = predict(boost.hit, newdata = test, n.trees = 1000)
error = mean((boost.pred- test$Salary)^2)
##########

#e
#lasso ridge
grid=10^seq(10,-2,length=100)



########

#f
library(randomForest)
boost.hit = gbm(Salary~., data = train, distribution = "gaussian",
                n.tree = 1000 , shrinkage = 0.1)
summary(boost.hit)


#g
#bagging
set.seed(2)
bag.hit = randomForest(Salary~., data = hit, subset=train, mtry=19, importance = TRUE)
pred = predict(bag.hit, newdata = test)
MSE.bag = mean((test$Salary - pred)^2)
MSE.bag








