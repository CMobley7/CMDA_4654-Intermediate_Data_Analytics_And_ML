#HW3
#Load applicable Libraries
library(ISLR)
library(MASS)
library(tree)
library(randomForest)
library(gbm)
library(glmnet)
#8.4 - 8
#a
set.seed(0)
n = nrow(Carseats)
p = ncol(Carseats) - 1
sample = sample(1:n,n/2)
train = Carseats[sample, ]
test = Carseats[-sample, ]
#b
tree.carseats = tree(Sales ~ ., data=train)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0)
tree.predict = predict(tree.carseats, newdata = test)
tree.mse = mean((test$Sales - tree.predict)^2)
print ("The Tree Model MSE is ")
print (tree.mse)
#c
cv.carseats = cv.tree(tree.carseats)
tree.min = which.min(cv.carseats$dev[5:NROW(cv.carseats$dev)]) + 5
plot(cv.carseats$size, cv.carseats$dev, type = "b")
points(tree.min, cv.carseats$dev[NROW(cv.carseats$dev) - tree.min + 1], pch = 20, col = "red", cex = 2)
print ("The Model Deviance Level Off With a Size of Approximatley")
print ((tree.min))
prune.carseats = prune.tree(tree.carseats, best=tree.min)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
prune.predict = predict(prune.carseats, newdata = test)
prune.mse = mean((prune.predict - test$Sales)^2)
print ("The Pruned Tree Model MSE is ")
print (prune.mse)
#d
bag.carseats = randomForest(Sales ~ ., data = train, mtry = p, ntree = 500, importance = TRUE)
plot (bag.carseats)
bag.predict = predict(bag.carseats, newdata = test)
bag.mse = mean((bag.predict - test$Sales)^2)
print ("The Bagged Tree Model MSE is ")
print (bag.mse)
bag.importance = importance(bag.carseats)
print (bag.importance)
#e
rf.carseats = randomForest(Sales ~ ., data = train, mtry=p/3, ntree = 500, importance=TRUE)
plot (rf.carseats)
rf.predict = predict(rf.carseats, newdata = test)
rf.mse = mean((rf.predict - test$Sales)^2)
print ("The Random Forest Model MSE is ")
print (rf.mse)
rf.importance = importance(rf.carseats)
print (rf.importance)
#8.4 - 10
#a
set.seed(0)
Hitters = na.omit(Hitters)
Hitters$Salary = log(Hitters$Salary)
#b
n = nrow(Hitters)
p = ncol(Hitters) - 1
sample = 1:200
train = Hitters[sample, ]
test = Hitters[-sample, ]
#c and d
lambda.seq = seq(.0001, 0.4, by = 0.001)
boost.train.error = rep(NA, length(lambda.seq))
boost.test.error = rep(NA, length(lambda.seq))

for (i in 1:length(lambda.seq)){
  boost.hitters = gbm(Salary ~ ., data=train, distribution = "gaussian", n.trees = 1000, shrinkage = lambda.seq[i])
  boost.train.predict = predict(boost.hitters, train, n.trees = 1000)
  boost.train.error[i] = mean((train$Salary - boost.train.predict)^2)
  boost.test.predict = predict(boost.hitters, newdata = test, n.trees = 1000)
  boost.test.error[i] = mean((test$Salary - boost.test.predict)^2)
}

plot(lambda.seq, boost.train.error, type = "b", pch=19, col="blue", xlab = "Lambda Value", ylab = "MSE")
lines(lambda.seq, boost.test.error, type = "b", pch=19, col="green")
legend("topright", pch=19, col=c("blue", "green"), c("Train MSE", "Test MSE"))
grid()

# e
lambda = lambda.seq[which.min(boost.test.error)]
boost.hitters = gbm(Salary ~ ., data=train, distribution = "gaussian", n.trees = 1000, shrinkage = lambda)
boost.predict = predict(boost.hitters, newdata = test, n.trees=1000)
boost.error = mean((test$Salary - boost.predict)^2)
print ("The Boosted Decision Tree MSE is ")
print (boost.error)

lm.hitters = lm(Salary ~ ., data=train)
lm.predict = predict(lm.hitters, newdata = test)
lm.error = mean((test$Salary - lm.predict)^2)
print ("The Linear Regression MSE is ")
print (lm.error)

xmat.train = model.matrix(Salary ~ .,data=train)
xmat.test = model.matrix(Salary ~ ., data=test)
cv.lasso.hitters = cv.glmnet(xmat.train, train$Salary, alpha = 1)
best.lambda = cv.lasso.hitters$lambda.min
lasso.hitters = glmnet(xmat.train, train$Salary, alpha=1)
lasso.predict = predict(lasso.hitters, s = best.lambda, newx = xmat.test)
lasso.error = mean((test$Salary - lasso.predict)^2)
print ("The Lasso MSE is ")
print (lasso.error)
# f
summary(boost.hitters)
#g

bag.hitters = randomForest(Salary ~ ., data = train, mtry = p, ntree = 1000, importance = TRUE)
plot (bag.hitters)
bag.predict = predict(bag.hitters, newdata = test)
bag.mse = mean((test$Salary - bag.predict)^2)
print ("The Bagged Tree Model MSE is ")
print (bag.mse)
bag.importance = importance(bag.hitters)
print (bag.importance)


