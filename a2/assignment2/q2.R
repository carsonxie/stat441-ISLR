library(ISLR)
attach(OJ)
dim(OJ)
#View(OJ)
names(OJ)
#sepreate to training and tesing 
#find subset of data that contain 800 rows
#train = OJ[1:800,]
#test = OJ[801:1070,]
#ramdonly pick 800
#train = OJ[sample(nrow(OJ), 800), ]
set.seed(11)
train.index = sample(1:1000,800)
test.index = (1:1070)[-train.index]
train = OJ[train.index,]
test = OJ[test.index,]

####
library(e1071)
svmfit1 = svm(Purchase~., data = train, kernel="linear",cost=0.01,scale=FALSE)
summary(svmfit1)
#get predict value by this svm, using train data as input
#train$Purchase != svm1.train.pred return a T/F matrix whether pred match the model output
svm1.train.pred=predict(svmfit1,newdata = train)
mean(train$Purchase != svm1.train.pred)

#seq(0.01,10)
tune.out = tune(svm, Purchase~., data = train, kernel="linear", ranges=list(cost=c(0.01, 0.1, 1, 5, 10)))
summary(tune.out)
#fit with new cost
svmfit = svm(Purchase~., data = train, kernel="linear",cost=10 , scale=FALSE)
svm.test.pred = predict(svmfit,newdata = test)
mean(test$Purchase != svm.test.pred)

#radial kernal
svmfit = svm(Purchase~., data = train, kernel="radial",cost=0.01,scale=FALSE)
summary(svmfit)
svm.train.pred=predict(svmfit,newdata = train)
mean(train$Purchase != svm.train.pred)
svm.test.pred=predict(svmfit,newdata = test)
mean(test$Purchase != svm.test.pred)
tune.out = tune(svm, Purchase~., data = train, kernel="radial", ranges=list(cost=c(0.01, 0.1, 1, 5, 10)))
summary(tune.out)
#fit with new cost
svmfit = svm(Purchase~., data = train, kernel="radial",cost=0.1,scale=FALSE)
svm.train.pred=predict(svmfit,newdata = train)
mean(train$Purchase != svm.train.pred)
svm.test.pred=predict(svmfit,newdata = test)
mean(test$Purchase != svm.test.pred)

#polynomail kernal
svmfit = svm(Purchase~., data = train, kernel="polynomial",cost=0.01,degree=2)
summary(svmfit)
svm.train.pred=predict(svmfit,newdata = train)
mean(train$Purchase != svm.train.pred)
svm.test.pred=predict(svmfit,newdata = test)
mean(test$Purchase != svm.test.pred)

tune.out = tune(svm, Purchase~., data = train, kernel="polynomial",degree=2, ranges=list(cost=c(0.01, 0.1, 1, 5, 10)))
summary(tune.out)
#new cost
svmfit = svm(Purchase~., data = train, kernel="polynomial",degree=2,cost=5,scale=FALSE)
svm.train.pred=predict(svmfit,newdata = train)
mean(train$Purchase != svm.train.pred)
svm.test.pred=predict(svmfit,newdata = test)
mean(test$Purchase != svm.test.pred)





                