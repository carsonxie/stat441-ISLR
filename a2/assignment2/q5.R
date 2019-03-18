Boston = read.table("/media/carson/60E810DFE810B56E/New/#STUDY/#12 2019 Winter/Stat 441/assignment2/Boston.txt")
attach(Boston)
dis.grid = seq(from=dislims[1],to=dislims[2])
#prepare data
##
#the output fit has x and y
#use formula sum(nox-fit$y)^2

#part a and c use same formula, just cahnge "box" to "normal"
normal.rss = c()
fit = ksmooth(dis,nox, kernel = c("normal"), bandwidth = 0.1)
plot(dis, nox, xlim=dislims, col="darkgrey",cex=0.5)
title("normal kernel smoothing",outer=F)
for (w in c(0.1,1,2,3,4,5)){
  fit = ksmooth(dis,nox, kernel = c("normal"), bandwidth = w)
  #lines(ksmooth(dis,nox, "normal", bandwidth = w), lty=w, col=w)
  normal.rss[w] = sum((nox - fit$y)^2)
}
#try part b
fit1 = ksmooth(dis,nox, kernel = c("box"), bandwidth = 0.1)
fit2 = ksmooth(dis,nox, kernel = c("box"), bandwidth = 0.5)
plot(fit1,lwd=0.5,pch=1,type="b",lty=1)
points(fit2,lwd=0.3,pch=2,col='red')

#b, apply cv for normal kernel use
#ucv() or bcv() to find the best bandwidth
library(MASS)
cv = c()
cv
for (i in c(0.1,1,2,3,4,5)) {
  
}


#e 
#fit = loess(nox~dis, span=0.2, data = Boston)
rss.l=c()
plot(dis, nox, xlim=dislims, col="darkgrey",cex=0.5)
title("loess spline")
for (s in c(0.1,1,2,3,4,5)){
  fit = loess(nox~dis, span=s, data = Boston)
  preds = predict(fit, newdata = dis.grid,se=TRUE)
  rss.l[s] = sum(fit$residuals^2)
  lines(dis.grid, preds$fit, col=s, lty=s)
}
fit.0.1 = loess(nox~dis, span=0.1, data = Boston)
fit.1 = loess(nox~dis, span=1, data = Boston)
fit.5 = loess(nox~dis, span=5, data = Boston)
fit.50 = loess(nox~dis, span=50, data = Boston)
fit.100 = loess(nox~dis, span=100, data = Boston)
fit.200 = loess(nox~dis, span=200, data = Boston)
fit.500 = loess(nox~dis, span=500, data = Boston)
anova(fit.0.1,fit.1,fit.5,fit.50,fit.100,fit.200,fit.500)















