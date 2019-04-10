#generate data 
library(neuralnet)
sigmoid = function(x) {
  1 / (1 + exp(-x))
}
#function input x
x = seq(1,10,length=100)

X1 = rnorm(100)
X2 = rnorm(100)
X = matrix(c(X1,X2), nrow = 2, ncol=100)
#X = data.frame(X)
a1 = matrix(c(3,3), nrow=1,ncol=2)
a2 = matrix(c(3,-3), nrow=1,ncol=2)

Y = sigmoid(x) * (a1 %*% X) + (a2 %*% X)^2 + 0.3 * (1/sqrt(2*pi)*exp(-x^2/2))
Y = t(Y)
data = data.frame(x,Y)
###
#test data
x.test = seq(10,100,length=100)
X1.t = rnorm(100)
X2.t = rnorm(100)
X.t = matrix(c(X1,X2), nrow = 2, ncol=100)
y.test = sigmoid(x.test) * (a1 %*% X.t) + (a2 %*% X.t)^2 + 0.3 * (1/sqrt(2*pi)*exp(-x.test^2/2))
y.test = t(y.test)
data_test = data.frame(x.test, y.test)
#scale data
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled_data <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
#create formula f
n <- names(scaled_data)
#f <- as.formula(paste("Y ~", paste(n[!n %in% "medv"], collapse = " + ")))
f = Y ~ x
nn <- neuralnet(f,data=scaled_data, hidden=c(4,1),linear.output=T) 
plot(nn)

##
#5 fold cv for nn
set.seed(1)
cv.error <- NULL
k <- 5
library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:k){
  index <- sample(1:nrow(data),round(0.9*nrow(data)))
  train.cv <- scaled_data[index,]
  test.cv <- scaled_data[-index,]
  
  nn <- neuralnet(f,data=train.cv,hidden=c(4,1),linear.output=T)
  
  pr.nn <- compute(nn,test.cv[,1:2])
  pr.nn <- pr.nn$net.result*(max(data$Y)-min(data$Y))+min(data$Y)
  
  test.cv.r <- (test.cv$Y)*(max(data$Y)-min(data$Y))+min(data$Y)
  
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  
  pbar$step()
}

mean(cv.error)
cv.error

boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)

#b
#using deep learning
library(h2o) 
h2o.init() 
data.hex <- as.h2o(data)
set.seed(3)
k=5
train = sample(1:nrow(data), nrow(data))
folds = matrix(train,k,nrow(data)/k) #each row represents 1 fold
cv.error = rep(0,k)
for(i in 1:k){
  data.train <- data[as.vector(folds[-i,]),]
  data.test <-  data[as.vector(folds[i,]),]
  data.hex.train <- as.h2o(data.train)
  data.hex.test <- as.h2o(data.test)
  data.dl.train <- h2o.deeplearning(x = 1:4, y = 5, training_frame = data.hex.train) 
  predictions <- h2o.predict(data.dl.train, data.hex.test)
  cv.error[i] <- sum(as.vector(as.character(predictions$predict))!=as.character(iris.test$Species))/15
}
cv.error
mean(cv.error)

