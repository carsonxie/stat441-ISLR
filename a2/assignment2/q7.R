library(boot)
set.seed (23)
y= rnorm (100)
x= rnorm (100)
y=x -2* x^2+ rnorm (100)
plot(x,y, col="darkgrey", cex=0.5)
title("Question7 part b")
data = data.frame(x,y)
#c
set.seed(53)
cv.error = rep(0,4)
for (i in 1:4){
  fit = glm(y~poly(x,i),data=data)

  #cv.error[i] = cv.glm(data, fit)$delta[1]
}

fit4 = glm(y~poly(x,4),data=data)
summary(fit4)




