# HW3
# Load applicable Libraries
library(ISLR)
library(MASS)
library(e1071)
library(ROCR)
library(plotrix)
# 9.7 - 1
X1 = seq(-15, 15, by = 0.1)
plot(X1, 1 + 3 * X1, type="l", lwd=2, col="blue", xlab="X1", ylab="X2")
text(-5, 20, "1 + 3X1 ??? X2 < 0", col="blue")
text(+5, -20, "1 + 3X1 ??? X2 > 0", col="blue")
lines(X1, 1 - X1 / 2, type="l", lwd=2, col="green")
text(-5, 15, "-2 + X1 + 2 * X2 > 0", col="green")
text(5, -15, "-2 + X1 + 2 * X2 < 0", col="green")
# 9.7 - 2
# a
plot(NA, NA, type="n", xlim=c(-4,2), ylim=c(-1,5), xlab="X1", ylab="X2", asp=1)
draw.circle(-1,2,2,nv=100,border="black", lwd=1)
# b
text(-1,2,"(1 + X1)2 + (2 ??? X2)2 <= 4", col="black")
text(-1,-0.5,"(1 + X1)2 + (2 ??? X2)2 > 4", col="black")
# c
plot(c(0, -1, 2, 3), c(0, 1, 2, 8), type="p", col=c("blue", "red", "blue", "blue"), xlim=c(-4,4), ylim=c(-1,9), xlab="X1", ylab="X2", asp=1)
draw.circle(-1, 2, 2, nv=100, border="black", lwd=1)
# 9.7 - 4
# Set Seed
set.seed(0)
# Create two non-linear separable classes
x=matrix(rnorm(100*2), ncol=2)
x[1:25,]=x[1:25,]+3
x[26:50,]=x[26:50,]-3
y=c(rep(1,50),rep(2,50))
# Plot Data
plot(x, col=y)
# Create Data Frame
data = data.frame(x=x, y=as.factor(y))
# Create Test and Train Data
sample = sample(100,50)
train = data[sample, ]
test = data[-sample, ]
# Create and Plot Support Vector Classifier
svc.fit = svm(y ~ ., data=train, kernel="linear", cost=10)
plot(svc.fit, train)
svc.train.confusionmatrix = table(train$y, predict(svc.fit, newdata=train))
print (svc.train.confusionmatrix)
cat("svc train error rate is", ((svc.train.confusionmatrix[1,2]+svc.train.confusionmatrix[2,1])/(sum(svc.train.confusionmatrix))))
svc.test.confusionmatrix = table(test$y, predict(svc.fit,newdata=test))
print (svc.test.confusionmatrix)
cat("svc test error rate is", ((svc.test.confusionmatrix[1,2]+svc.test.confusionmatrix[2,1])/(sum(svc.test.confusionmatrix))))
# Create and Plot Support Vector Machine
svm.fit = svm(y ~ ., data=train, kernel="radial", cost=10)
plot(svm.fit, train)
svm.train.confusionmatrix = table(train$y, predict(svm.fit, newdata=train))
print (svm.train.confusionmatrix)
cat("svm train error rate is", ((svm.train.confusionmatrix[1,2]+svm.train.confusionmatrix[2,1])/(sum(svm.train.confusionmatrix))))
svm.test.confusionmatrix = table(test$y, predict(svm.fit,newdata=test))
print (svm.test.confusionmatrix)
cat("svm test error rate is", ((svm.test.confusionmatrix[1,2]+svm.test.confusionmatrix[2,1])/(sum(svm.test.confusionmatrix))))
# 9.7 - 8
# a
set.seed(0)
n = nrow(OJ)
p = ncol(OJ)
sample = sample(n, 800)
train = OJ[sample, ]
test = OJ[-sample, ]
# b
svc.linear = svm(Purchase ~ ., data=train, kernel="linear", cost=0.01)
linear.summary = summary(svc.linear)
print (linear.summary)
# c
linear.train.confusionmatrix = table(train$Purchase, predict(svc.linear, newdata=train))
print (linear.train.confusionmatrix)
cat("linear svc train error rate is", ((linear.train.confusionmatrix[1,2]+linear.train.confusionmatrix[2,1])/(sum(linear.train.confusionmatrix))))
linear.test.confusionmatrix = table(test$Purchase, predict(svc.linear,newdata=test))
print (linear.test.confusionmatrix)
cat("linear svc test error rate is", ((linear.test.confusionmatrix[1,2]+linear.test.confusionmatrix[2,1])/(sum(linear.test.confusionmatrix))))
# d
tune.linear = tune(svm, Purchase ~ ., data=OJ, kernel="linear", ranges=list(cost=seq(0.1, 10, by=0.1)))
linear.tune.summary = summary(tune.linear)
print (linear.tune.summary)
# e
linear.tune.train.confusionmatrix = table(train$Purchase, predict(tune.linear$best.model, newdata=train))
print (linear.tune.train.confusionmatrix)
cat("tuned linear svc train error rate is", ((linear.tune.train.confusionmatrix[1,2]+linear.tune.train.confusionmatrix[2,1])/(sum(linear.tune.train.confusionmatrix))))
linear.tune.test.confusionmatrix = table(test$Purchase, predict(tune.linear$best.model, newdata=test))
print (linear.tune.test.confusionmatrix)
cat("tuned linear test error rate is", ((linear.tune.test.confusionmatrix[1,2]+linear.tune.test.confusionmatrix[2,1])/(sum(linear.tune.test.confusionmatrix))))
# f
 # b
svc.radial = svm(Purchase ~ ., data=train, kernel="radial", cost=0.01)
radial.summary = summary(svc.radial)
print (radial.summary)
 # c
radial.train.confusionmatrix = table(train$Purchase, predict(svc.radial, newdata=train))
print (radial.train.confusionmatrix)
cat("radial svc train error rate is", ((radial.train.confusionmatrix[1,2]+radial.train.confusionmatrix[2,1])/(sum(radial.train.confusionmatrix))))
radial.test.confusionmatrix = table(test$Purchase, predict(svc.radial,newdata=test))
print (radial.test.confusionmatrix)
cat("radial svc test error rate is", ((radial.test.confusionmatrix[1,2]+radial.test.confusionmatrix[2,1])/(sum(radial.test.confusionmatrix))))
 # d
tune.radial = tune(svm, Purchase ~ ., data=OJ, kernel="radial", ranges=list(cost=seq(0.1, 10, by=0.1)))
radial.tune.summary = summary(tune.radial)
print (radial.tune.summary)
 # e
radial.tune.train.confusionmatrix = table(train$Purchase, predict(tune.radial$best.model, newdata=train))
print (radial.tune.train.confusionmatrix)
cat("tuned radial svc train error rate is", ((radial.tune.train.confusionmatrix[1,2]+radial.tune.train.confusionmatrix[2,1])/(sum(radial.tune.train.confusionmatrix))))
radial.tune.test.confusionmatrix = table(test$Purchase, predict(tune.radial$best.model, newdata=test))
print (radial.tune.test.confusionmatrix)
cat("tune radial test error rate is", ((radial.tune.test.confusionmatrix[1,2]+radial.tune.test.confusionmatrix[2,1])/(sum(radial.tune.test.confusionmatrix))))
# g
  # b
svc.polynomial = svm(Purchase ~ ., data=train, kernel="polynomial", cost=0.01, degree=2)
polynomial.summary = summary(svc.polynomial)
print (polynomial.summary)
  # c
polynomial.train.confusionmatrix = table(train$Purchase, predict(svc.polynomial, newdata=train))
print (polynomial.train.confusionmatrix)
cat("polynomial svc train error rate is", ((polynomial.train.confusionmatrix[1,2]+polynomial.train.confusionmatrix[2,1])/(sum(polynomial.train.confusionmatrix))))
polynomial.test.confusionmatrix = table(test$Purchase, predict(svc.polynomial,newdata=test))
print (polynomial.test.confusionmatrix)
cat("polynomial svc test error rate is", ((polynomial.test.confusionmatrix[1,2]+polynomial.test.confusionmatrix[2,1])/(sum(polynomial.test.confusionmatrix))))
  # d
tune.polynomial = tune(svm, Purchase ~ ., data=OJ, kernel="polynomial", ranges=list(cost=seq(0.1, 10, by=0.1)), degree=2)
polynomial.tune.summary = summary(tune.polynomial)
print (polynomial.tune.summary)
  # e
polynomial.tune.train.confusionmatrix = table(train$Purchase, predict(tune.polynomial$best.model, newdata=train))
print (polynomial.tune.train.confusionmatrix)
cat("tuned polynomial svc train error rate is", ((polynomial.tune.train.confusionmatrix[1,2]+polynomial.tune.train.confusionmatrix[2,1])/(sum(polynomial.tune.train.confusionmatrix))))
polynomial.tune.test.confusionmatrix = table(test$Purchase, predict(tune.polynomial$best.model, newdata=test))
print (polynomial.tune.test.confusionmatrix)
cat("tune svc test error rate is", ((polynomial.tune.test.confusionmatrix[1,2]+polynomial.tune.test.confusionmatrix[2,1])/(sum(polynomial.tune.test.confusionmatrix))))