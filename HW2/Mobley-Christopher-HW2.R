#HW2
#Load applicable Libraries
library(ISLR)
library(MASS)
library(boot)
#4.7-10
#a
Direction = Weekly$Direction
Weekly$Direction = as.numeric(Weekly$Direction)
Weekly$Direction[Weekly$Direction==1] = -1
Weekly$Direction[Weekly$Direction==2] = 1
Weekly.cor = cor(Weekly)
summary(Weekly)
print(Weekly.cor)
pairs(Weekly)
#b
Weekly$Direction = Direction
logreg.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Weekly, family=binomial)
summary(logreg.fit)
#c
logreg.prob = predict(logreg.fit, newdata=Weekly, type="response")
logreg.pred = rep("Down", length(logreg.prob))
logreg.pred[logreg.prob>0.5] = "Up"
con.mat = table(pred=logreg.pred, truth=Direction) 
print (con.mat)
#d
train = (Weekly$Year < 2009)
test = (Weekly$Year >= 2009)
logreg.fit2 = glm(Direction ~ Lag2, data=Weekly, family=binomial, subset=train)
logreg2.prob = predict(logreg.fit2, newdata=Weekly[test,], type="response")
logreg2.pred = rep("Down", length(logreg2.prob))
logreg2.pred[logreg2.prob>0.5] = "Up"
con.mat2 = table(pred=logreg2.pred, truth=Weekly[test,]$Direction) 
print (con.mat2)
#e
train = (Weekly$Year < 2009)
test = (Weekly$Year >= 2009)
lda.fit = lda(Direction ~ Lag2, data=Weekly, subset=train)
lda.pred = predict(lda.fit, newdata=Weekly[test,])
con.mat3 = table(pred=lda.pred$class, truth=Weekly[test,]$Direction) 
print (con.mat3)
#f
train = (Weekly$Year < 2009)
test = (Weekly$Year >= 2009)
qda.fit = qda(Direction ~ Lag2, data=Weekly, subset=train)
qda.pred = predict(qda.fit, newdata=Weekly[test,])
con.mat4 = table(pred=qda.pred$class, truth=Weekly[test,]$Direction) 
print (con.mat4)
#g
train = (Weekly$Year < 2009)
test = (Weekly$Year >= 2009)
train.X = as.matrix(Weekly[train,]$Lag2)
test.X = as.matrix(Weekly[test,]$Lag2)
train.Direction = as.matrix(Weekly[train,]$Direction)
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k=1)
con.mat5 = table(pred=knn.pred, truth=Weekly[test,]$Direction)
print (con.mat5)
#i
#1 KNN = 9
train = (Weekly$Year < 2009)
test = (Weekly$Year >= 2009)
train.X = as.matrix(Weekly[train,]$Lag2)
test.X = as.matrix(Weekly[test,]$Lag2)
train.Direction = as.matrix(Weekly[train,]$Direction)
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k=9)
con.mat6 = table(pred=knn.pred, truth=Weekly[test,]$Direction)
print (con.mat6)
#2 Logreg Lag2 + Lag1
train = (Weekly$Year < 2009)
test = (Weekly$Year >= 2009)
logreg.fit3 = glm(Direction ~ Lag2 + Lag1, data=Weekly, family=binomial, subset=train)
logreg3.prob = predict(logreg.fit3, newdata=Weekly[test,], type="response")
logreg3.pred = rep("Down", length(logreg3.prob))
logreg3.pred[logreg3.prob>0.5] = "Up"
con.mat7 = table(pred=logreg3.pred, truth=Weekly[test,]$Direction) 
print (con.mat7)
#3 LDA Lag2 + Lag1
train = (Weekly$Year < 2009)
test = (Weekly$Year >= 2009)
lda.fit2 = lda(Direction ~ Lag2 + Lag1, data=Weekly, subset=train)
lda.pred2 = predict(lda.fit2, newdata=Weekly[test,])
con.mat8 = table(pred=lda.pred2$class, truth=Weekly[test,]$Direction) 
print (con.mat8)
#4 QDA Lag2 + Lag1
train = (Weekly$Year < 2009)
test = (Weekly$Year >= 2009)
qda.fit2 = qda(Direction ~ Lag2 + Lag1, data=Weekly, subset=train)
qda.pred2 = predict(qda.fit2, newdata=Weekly[test,])
con.mat9 = table(pred=qda.pred2$class, truth=Weekly[test,]$Direction) 
print (con.mat9)
#5.4 - 2
#g
n = 1:1e+05 
pr = 1 - ((n-1)/n)^n
plot (n, pr,type='l')
#h
store=rep(NA, 10000) 
for(i in 1:10000) { 
  store[i]=sum(sample (1:100, rep=TRUE)==4) >0
}
mean(store)
#5.4-6
set.seed(0)
#a
se = glm(default ~ income + balance, data = Default, family = binomial)
summary(se)
#b
boot.fn = function(data,index){
  logreg.fit4 = glm(default ~ income + balance, data=data, family="binomial", subset=index)
  return (coefficients(logreg.fit4))
}
#c
boot(Default, boot.fn, 1000)
#5.4-8
#a
set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x-2*x^2+rnorm(100)
#b
plot(x,y)
#c
dataframe = data.frame(x, y)
#i
logreg.fit5 = glm(y ~ x, data=dataframe)
cv.error = cv.glm(dataframe, logreg.fit5)$delta
#ii
logreg.fit6 = glm(y ~ x + I(x^2), data=dataframe)
cv.error2 = cv.glm(dataframe, logreg.fit6)$delta
#iii
logreg.fit7 = glm(y ~ x + I(x^2) +I(x^3), data=dataframe)
cv.error3 = cv.glm(dataframe, logreg.fit7)$delta
#iv
logreg.fit8 = glm(y ~ x + I(x^2) +I(x^3) + I(x^4), data=dataframe)
cv.error4 = cv.glm(dataframe, logreg.fit8)$delta
#d
set.seed(2)
logreg.fit9 = glm(y ~ x, data=dataframe)
cv.error5 = cv.glm(dataframe, logreg.fit9)$delta
#ii
logreg.fit10 = glm(y ~ x + I(x^2), data=dataframe)
cv.error6 = cv.glm(dataframe, logreg.fit10)$delta
#iii
logreg.fit11 = glm(y ~ x + I(x^2) +I(x^3), data=dataframe)
cv.error7 = cv.glm(dataframe, logreg.fit11)$delta
#iv
logreg.fit12 = glm(y ~ x + I(x^2) +I(x^3) + I(x^4), data=dataframe)
cv.error8 = cv.glm(dataframe, logreg.fit12)$delta
#f
lm.fit = lm(y ~ x, data=dataframe)
summary(lm.fit)
#ii
lm.fit2 = lm(y ~ x + I(x^2), data=dataframe)
summary(lm.fit2)
#iii
lm.fit3 = lm(y ~ x + I(x^2) +I(x^3), data=dataframe)
summary(lm.fit3)
#iv
lm.fit4 = lm(y ~ x + I(x^2) +I(x^3) + I(x^4), data=dataframe)
summary(lm.fit4)

