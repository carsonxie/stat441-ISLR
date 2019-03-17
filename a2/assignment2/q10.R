library(ISLR)
library(MASS)
library(tree)
attach(Carseats)

#regression tree
#High = ifelse(Sales<=8, "No","Yes")
Car = data.frame(Carseats,High)
set.seed(1)
train = sample(1:nrow(Carseats), nrow(Carseats)/2)
test = Carseats[-train]
tree.car = tree(Sales~., Carseats, subset=train)
summary(tree.car)
plot(tree.car)
text(tree.car, pretty =0)

#calculate MSE
pred = predict(tree.car, test)
MSE = mean((test$Sales - pred)^2)
#if want to get table, add type="class" in predict
#table(pred, test$High)

#c
#cv
set.seed(12)
car.cv = cv.tree(tree.car)
car.cv$dev

#best size is 15
prune = prune.tree(tree.car, best = 15)
pred = predict(prune, test)
MSE = mean((test$Sales - pred)^2)

#d
#bagging
library(randomForest)
set.seed(2)
#mtry =10 is num of predictors exclude sales
bag.car = randomForest(Sales~., data = Carseats, subset=train, mtry=10, importance = TRUE)
pred = predict(bag.car, newdata = test)
MSE = mean((test$Sales - pred)^2)
importance(bag.car)

#e
#RF method
#mtry set p/3 in regression tree, 12/3=4
rf.car = randomForest(Sales~., data = Carseats, subset=train, mtry=4, importance = TRUE)
pred = predict(rf.car, newdata = test)
MSE = mean((test$Sales - pred)^2)
importance(rf.car)







