#a
set.seed(1)
x = matrix(rnorm(60*50), ncol = 50)
x[1:20, 1:50] = x[1:20, 1:50]  + 3
x[21:40, 20:40] = x[21:40, 20:40]  -4 
x[41:60, 40:50] = x[41:60, 40:50]  + 9

x2 = matrix(rnorm(50*2),ncol =2)
x2[1:25,1] = x2[1:25,1] + 3
x2[1:25,2] = x2[1:25,2] -4
pr.out3 = prcomp(x2)
plot(pr.out3$x[,1:2], col=1:3)

pr.out = prcomp(x)
pr.out2 = prcomp(USArrests)
names(pr.out)
biplot(pr.out, sacle=0)
plot(pr.out$x[,1:2], col=1:3)
plot(pr.out2$x[,1:2], col=1:3)

pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale = 0, col=c(1,2))
