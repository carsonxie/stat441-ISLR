library(ISLR)
attach(USArrests)

#a
hc.complete = hclust(dist(USArrests), method = "complete")
plot(hc.complete, main = "complete linkage",xlab="", sub = "", cex=0.9)
cut = cutree(hc.complete, 3)

#scale it 
scale_data = scale(USArrests)
plot(hclust(dist(scale_data), method = "complete"), main = "HC with scaled features")
