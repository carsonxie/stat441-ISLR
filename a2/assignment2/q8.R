Boston = read.table("/media/carson/60E810DFE810B56E/New/#STUDY/#12 2019 Winter/Stat 441/assignment2/Boston.txt")
attach(Boston)
library(boot)
#a
mean(medv)
#b
sd(medv)/(sqrt(length(medv)))

#c
mean.fn = function(data, index){
  mean = mean(medv[index])
  return(mean)
}
set.seed(1)
mean.fn(Boston, sample(500, 500,replace = T))
boot(Boston,mean.fn, R=1000)

#d
t.test(medv)

#e
median(medv)
#f
median.fn = function(data, index){
  median = median(medv[index])
  return(median)
}
set.seed(1)
median.fn(Boston, sample(506, 500,replace = T))
boot(Boston,median.fn, R=1000)

#g
quantile(medv, c(0.1))

#h
quantile.fn = function(data, index){
  quantile = quantile(data[index], c(0.1))
  return(quantile)
}
set.seed(1)
quantile.fn(medv, sample(500, 500,replace = T))
boot(Boston, quantile.fn, R=1000)






