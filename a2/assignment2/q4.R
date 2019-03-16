Boston = read.table("/media/carson/60E810DFE810B56E/New/#STUDY/#12 2019 Winter/Stat 441/assignment2/Boston.txt")
View(Boston)
attach(Boston)
fit = lm(nox~poly(dis,3), data=Boston)
coef(summary(fit))
#prepare for plot
dislims = range(dis)
dis.grid = seq(from=dislims[1],to=dislims[2])
preds=predict(fit,newdata=list(dis=dis.grid),se=TRUE)
plot(dis, nox, xlim=dislims, col="darkgrey",cex=0.5)
title("Cubic Poly",outer=T)
lines(dis.grid, preds$fit, lwd=2, col="blue")
#points(dis.grid,predict(fit,newdata = list(dis=dis.grid)),col="darkgreen",lwd=2,type="l")
abline(v=c(2,6,10),lty=2,col="darkgreen")

#(2)
res = c()
plot(dis, nox, xlim=dislims, col="darkgrey",cex=0.5)
title("Ten polys",outer=T)
for (d in c(1,2,3,4,5,6,7,8,9,10)){
  fit = lm(nox~poly(dis,d), data = Boston)
  preds = predict(fit, newdata = list(dis=dis.grid),se=TRUE)
  res[d] = sum(fit$residuals^2)
  #lines(dis.grid, preds$fit, lwd=1,lty=d)
  
  #fit.list[[d]] = c(lm(nox~poly(dis,d), data = Boston))
}
legend(x='topright',legend = 1:10,lty = ,lwd = c(2,2))

#find best degree
#anova(fit.list[1],fit.list[2],fit.list[3],fit.list[4])
fit.1 = lm(nox~poly(dis,1), data = Boston)
fit.2 = lm(nox~poly(dis,2), data = Boston)
fit.3 = lm(nox~poly(dis,3), data = Boston)
fit.4 = lm(nox~poly(dis,4), data = Boston)
fit.5 = lm(nox~poly(dis,5), data = Boston)
fit.6 = lm(nox~poly(dis,6), data = Boston)
fit.7 = lm(nox~poly(dis,7), data = Boston)
fit.8 = lm(nox~poly(dis,8), data = Boston)
fit.9 = lm(nox~poly(dis,9), data = Boston)
fit.10 = lm(nox~poly(dis,10), data = Boston)
anova(fit.1,fit.2,fit.3,fit.4,fit.5,fit.6,fit.7,fit.8,fit.9,fit.10)

#fit spline with deg=4
library(splines)
fit = lm(nox~bs(dis, df=4, knots=c(4,7,11)), data=Boston)
summary(fit)
pred = predict(fit, newdata = list(dis=dis.grid),se=T)
plot(dis, nox, col="gray")
lines(dis.grid, pred$fit, lwd=2)
lines(dis.grid, pred, lwd=2)
title("Regression spline")

#fit diff degree of freedom df
#
plot(dis, nox, xlim=dislims, col="darkgrey",cex=0.5)
title("Regression spline with different df",outer=F)
rss=c() #be careful that vector will not overwrite new data, 
for (d in c(3,4,5,6,7,8)){
  fit = lm(nox~bs(dis, df=d), data=Boston)
  preds = predict(fit, newdata = list(dis=dis.grid),se=TRUE)
  rss[d] = sum(fit$residuals^2) 
  lines(dis.grid, preds$fit, lwd=1,lty=d,col=d)
}

fit.3 = lm(nox~bs(dis, df=3), data=Boston)
fit.4 = lm(nox~bs(dis, df=4), data=Boston)
fit.5 = lm(nox~bs(dis, df=5), data=Boston)
fit.6 = lm(nox~bs(dis, df=6), data=Boston)
fit.7 = lm(nox~bs(dis, df=7), data=Boston)
fit.8 = lm(nox~bs(dis, df=8), data=Boston)
anova(fit.3,fit.4,fit.5,fit.6,fit.7,fit.8)




















