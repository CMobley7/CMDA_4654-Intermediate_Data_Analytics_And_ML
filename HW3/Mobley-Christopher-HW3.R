#HW3
#Load applicable Libraries
library(ISLR)
library(MASS)
library(leaps)
library(glmnet)
library(pls)
#6.8-8
#a
set.seed(0)
n = 100
X = rnorm(n)
epsilon = .1 * rnorm(n)
#b
beta.0 = 0.50
beta.1 = 0.75
beta.2 = 0.25
beta.3 = -0.25
Y  = beta.0 + beta.1*X + beta.2*X^2 + beta.3*X^3 + epsilon
#c
data.frame = data.frame(Y=Y, X=X, X2=X^2, X3=X^3, X4=X^4, X5=X^5, X6=X^6, X7=X^7,X8=X^8,X9=X^9, X10=X^10)
regfit.best = regsubsets(Y~., data=data.frame, nvmax=10)
regfit.summary = summary(regfit.best)
best.cp = which.min(regfit.summary$cp)
print ("Best model according to Cp is ")
print (best.cp)
best.bic = which.min(regfit.summary$bic)
print ("Best model according to BIC is ")
print(best.bic)
best.adjr2 = which.max(regfit.summary$adjr2)
print ("Best model according to R^2adj is ")
print (best.adjr2)
par = par(mfrow=c(1,3))
plot(regfit.summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
points(best.cp, regfit.summary$cp[best.cp], pch=20, col="red", lwd=3 )
plot(regfit.summary$bic,xlab="Number of Variables", ylab="BIC", type="l")
points(best.bic, regfit.summary$bic[best.bic], pch=20, col="red", lwd=3 )
plot(regfit.summary$adjr2,xlab="Number of Variables", ylab="adj R^2", type="l")
points(best.adjr2, regfit.summary$adjr2[best.adjr2], pch=20, col="red", lwd=3 )
coefi.best = coef(regfit.best, id=best.cp)
print (coefi.best)
#d
#forwards
regfit.forward = regsubsets(Y~., data=data.frame, nvmax=10, method="forward")
forward.summary = summary(regfit.forward)
forward.cp = which.min(forward.summary$cp)
print ("Best model according to Cp is ")
print (forward.cp)
forward.bic = which.min(forward.summary$bic)
print ("Best model according to BIC is ")
print(forward.bic)
forward.adjr2 = which.max(forward.summary$adjr2)
print ("Best model according to R^2adj is ")
print (forward.adjr2)
par = par(mfrow=c(1,3))
plot(forward.summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
points(forward.cp, forward.summary$cp[forward.cp], pch=20, col="red", lwd=3 )
plot(forward.summary$bic,xlab="Number of Variables", ylab="BIC", type="l")
points(forward.bic, forward.summary$bic[forward.bic], pch=20, col="red", lwd=3 )
plot(forward.summary$adjr2,xlab="Number of Variables", ylab="adj R^2", type="l")
points(forward.adjr2, forward.summary$adjr2[forward.adjr2], pch=20, col="red", lwd=3 )
coefi.forward = coef(regfit.forward, id=forward.cp)
print (coefi.forward)
#backwards
regfit.backward = regsubsets(Y~., data=data.frame, nvmax=10, method="backward")
backward.summary = summary(regfit.backward)
backward.cp = which.min(backward.summary$cp)
print ("Best model according to Cp is ")
print (backward.cp)
backward.bic = which.min(backward.summary$bic)
print ("Best model according to BIC is ")
print(backward.bic)
backward.adjr2 = which.max(backward.summary$adjr2)
print ("Best model according to R^2adj is ")
print (backward.adjr2)
par = par(mfrow=c(1,3))
plot(backward.summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
points(backward.cp, backward.summary$cp[backward.cp], pch=20, col="red", lwd=3 )
plot(backward.summary$bic,xlab="Number of Variables", ylab="BIC", type="l")
points(backward.bic, backward.summary$bic[backward.bic], pch=20, col="red", lwd=3 )
plot(backward.summary$adjr2,xlab="Number of Variables", ylab="adj R^2", type="l")
points(backward.adjr2, backward.summary$adjr2[backward.adjr2], pch=20, col="red", lwd=3 )
coefi.backward = coef(regfit.backward, id=backward.cp)
print (coefi.backward)
#e
grid = 10^seq(10,-2,length=100)
xmat=model.matrix(Y~.,data=data.frame, nvmax=10)
lasso.mod = glmnet(xmat, Y, alpha=1, lambda = grid)
cv.out = cv.glmnet(xmat, Y, alpha=1)
par = par(mfrow=c(1,1))
plot(cv.out)
best.lambda = cv.out$lambda.min
print ("Best model Lambda is ")
print (best.lambda)
coefi.lasso = predict(lasso.mod, type="coefficients", s=best.lambda)
print (coefi.lasso)
#f
#model creation
set.seed(0)
beta.7 = -0.25
Y1  = beta.0 + beta.7*X^7 + epsilon
#best subset
data.frame1 = data.frame(Y=Y1, X=X, X2=X^2, X3=X^3, X4=X^4, X5=X^5, X6=X^6, X7=X^7,X8=X^8,X9=X^9, X10=X^10)
regfit.best1 = regsubsets(Y1~., data=data.frame1, nvmax=10)
regfit.summary1 = summary(regfit.best1)
best.cp1 = which.min(regfit.summary1$cp)
print ("Best model according to Cp is ")
print (best.cp1)
best.bic1 = which.min(regfit.summary1$bic)
print ("Best model according to BIC is ")
print(best.bic1)
best1.adjr2 = which.max(regfit.summary1$adjr2)
print ("Best model according to R^2adj is ")
print (best1.adjr2)
par = par(mfrow=c(1,3))
plot(regfit.summary1$cp, xlab="Number of Variables", ylab="Cp", type="l")
points(best.cp1, regfit.summary1$cp[best.cp1], pch=20, col="red", lwd=3 )
plot(regfit.summary1$bic,xlab="Number of Variables", ylab="BIC", type="l")
points(best.bic1, regfit.summary1$bic[best.bic1], pch=20, col="red", lwd=3 )
plot(regfit.summary1$adjr2,xlab="Number of Variables", ylab="adj R^2", type="l")
points(best1.adjr2, regfit.summary1$adjr2[best1.adjr2], pch=20, col="red", lwd=3 )
coefi.best1 = coef(regfit.best1, id=best.cp1)
print (coefi.best1)
#lasso
lasso.mod1 = glmnet(xmat, Y1, alpha=1, lambda = grid)
cv.out1 = cv.glmnet(xmat, Y1, alpha=1)
par = par(mfrow=c(1,1))
plot(cv.out1)
best.lambda1 = cv.out1$lambda.min
print ("Best model Lambda is ")
print (best.lambda1)
coefi.lasso1 = predict(lasso.mod1, type="coefficients", s=best.lambda1)
print (coefi.lasso1)
#6.8-9
#a
set.seed(0)
n = dim(College)[1]
p = dim(College)[2]
train = sample(c(TRUE,FALSE), n, rep=TRUE)
test = (!train)
college.train = College[train,]
college.test = College[test,]
#b
lm.fit = lm(Apps~., data=college.train)
lm.pred = predict(lm.fit, newdata=college.test)
lm.error = mean((college.test$Apps - lm.pred)^2)
print("Linear Model Test Error is ")
print (lm.error)
#c
Y = college.train$Apps
train.mat = model.matrix(Apps~., data=college.train)
test.mat = model.matrix(Apps~., data=college.test)
cv.out2 = cv.glmnet(train.mat, Y, alpha=0)
best.lambda2 = cv.out2$lambda.min
print (best.lambda2)
ridge.mod = glmnet(train.mat, Y, alpha=0)
ridge.pred = predict(ridge.mod, newx=test.mat, s=best.lambda2)
ridge.error = mean((college.test$Apps - ridge.pred)^2)
print (ridge.error)
#d
cv.out3 = cv.glmnet(train.mat, Y, alpha=1)
best.lambda3 = cv.out3$lambda.min
print (best.lambda3)
lasso.mod2 = glmnet(train.mat, Y, alpha=1)
lasso.pred2 = predict(lasso.mod2, newx=test.mat, s=best.lambda3)
lasso.error2 = mean((college.test$Apps - lasso.pred2)^2)
print (lasso.error2)
coefi.lasso2 = predict(lasso.mod2, type="coefficients", s=best.lambda3)
print (coefi.lasso2)
# e
pcr.fit = pcr(Apps~., data=college.train, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
ncomp=17
pcr.pred = predict(pcr.fit, college.test, ncomp=ncomp)
pcr.error = mean((college.test$Apps - pcr.pred)^2)
print (pcr.error)
# f
pls.fit = plsr(Apps~., data=college.train, scale=TRUE, validation="CV")
validationplot(pls.fit, val.type="MSEP")
ncomp1=10
pls.pred = predict(pls.fit, college.test, ncomp=ncomp1)
pls.error = mean((college.test$Apps - pls.pred)^2)
print (pls.error)
#g
test.avg = mean(college.test[, "Apps"])
lm.r2 = 1 - (lm.error / mean((college.test[, "Apps"] - test.avg)^2))
print (lm.r2)
ridge.r2 = 1 - (ridge.error / mean((college.test[, "Apps"] - test.avg)^2))
print (ridge.r2)
lasso.r2 = 1 - (lasso.error2 / mean((college.test[, "Apps"] - test.avg)^2))
print (lasso.r2)
pcr.r2 = 1 - (pcr.error / mean((college.test[, "Apps"] - test.avg)^2))
print (pcr.r2)
pls.r2 = 1 - (pcr.error / mean((college.test[, "Apps"] - test.avg)^2))
print (pls.r2)
barplot(c(lm.r2, ridge.r2, lasso.r2, pcr.r2, pls.r2), col="blue", names.arg=c("OLS", "Ridge", "Lasso", "PCR", "PLS"), main="R^2 Results")

