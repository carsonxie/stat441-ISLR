library(ISLR)
library(tree)
#attach(OJ)

#train = sample(1:nrow(OJ), 800)
#test = OJ[-train]
set.seed(11)
train.index = sample(1:1070,800)
test.index = (1:1070)[-train.index]
train = OJ[train.index,]
test = OJ[test.index,]


#b
tree.oj = tree(Purchase~., OJ, subset=train)
summary(tree.oj)
tree.oj

#c, d
plot(tree.oj)
text(tree.oj, pretty = 0)

#e
#in order to get secind row of the matrix, only need purcahse colum in test data 
tree.pred = predict(tree.oj, test, type = "class")
table(tree.pred, test$Purchase)

#f cv
set.seed(29)
cv.oj = cv.tree(tree.oj, FUN=prune.misclass)

#g plot
plot(cv.oj$size, cv.oj$dev,type="b")

#i prune tree
prune.oj = prune.tree(tree.oj, best = 5)
summary(prune.oj)

#k
prune.pred = predict(prune.oj, test, type = "class")
table(prune.pred, test$Purchase)





