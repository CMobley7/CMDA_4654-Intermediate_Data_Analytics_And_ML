# Load and/or Install applicable Libraries
if( ! require("MASS") ){ install.packages("MASS") }
if( ! require("boot") ){ install.packages("boot") }
if( ! require("leaps") ){ install.packages("leaps") }
if( ! require("glmnet") ){ install.packages("glmnet") }
if( ! require("pls") ){ install.packages("pls") }
if( ! require("tree") ){ install.packages("tree") }
if( ! require("randomForest") ){ install.packages("randomForest") }
if( ! require("gbm") ){ install.packages("gbm") }
if( ! require("tgp") ){ install.packages("tgp") }

# Load Cleaned Data
subway_data = read.csv("data_set_5.csv")

# Create Data Frame
subway_data.frame = data.frame(Y=subway_data$ENTRIESn_hourly, hour=subway_data$hour, day_week=subway_data$day_week, weekday=subway_data$weekday, latitude = subway_data$latitude, longitude = subway_data$longitude, rain = subway_data$rain, fog = subway_data$fog, meanprecipi = subway_data$meanprecipi, meanpressurei = subway_data$meanpressurei, meantempi = subway_data$meantempi, meanwspdi = subway_data$meanwspdi)

# Get Data Frame Shape
n = nrow(subway_data.frame)
p = ncol(subway_data.frame) - 1
mst.full = sum((subway_data.frame$Y - mean(subway_data.frame$Y))^2)/(n-1)

# Linear Model

# Linear Model with k=10 K-Fold Cross-Validation
set.seed(0)
k = 10
folds = sample(1:k, n, replace=T)
lm.test.error = matrix(NA, k, 1)
lm.test.adjr2 = matrix(NA, k, 1)
lm.train.error = matrix(NA, k, 1)
lm.train.adjr2 = matrix(NA, k, 1)

for (i in 1:k){
  mst.test = sum((subway_data.frame$Y[folds==i] - mean(subway_data.frame$Y[folds==i]))^2)/(NROW(subway_data.frame$Y[folds == i])-1)
  mst.train = sum((subway_data.frame$Y[folds!=i] - mean(subway_data.frame$Y[folds!=i]))^2)/(NROW(subway_data.frame$Y[folds != i])-1)

  lm.subway_data = lm(Y ~ .-fog, data=subway_data.frame[folds!=i, ])

  lm.test.predict = predict(lm.subway_data, subway_data.frame[folds == i, ])
  lm.test.error[i] = sum((subway_data.frame$Y[folds == i] - lm.test.predict)^2)/(NROW(subway_data.frame$Y[folds == i])-p)
  lm.test.adjr2[i] = 1 - (lm.test.error[i]/mst.test)

  lm.train.predict = predict(lm.subway_data, subway_data.frame[folds !=i, ])
  lm.train.error[i] = sum((subway_data.frame$Y[folds != i] - lm.train.predict)^2)/(NROW(subway_data.frame$Y[folds != i])-p)
  lm.train.adjr2[i] = 1 - (lm.train.error[i]/mst.train)
}

#Calculate MSE of Linear Model
mean.lm.test.error = apply(lm.test.error, 2, mean)
mean.lm.train.error = apply(lm.train.error, 2, mean)

#Calculate AdjR2 of Linear Model
mean.lm.test.adjr2 = apply(lm.test.adjr2, 2, mean)
mean.lm.train.adjr2 = apply(lm.train.adjr2, 2, mean)

# Print MSE and AdjR2 for Linear Model
cat("The Linear Model MSE is ", mean.lm.test.error)
cat("The Linear Model AdjR2 is ", mean.lm.train.adjr2)

#Make Final Linar Model
lm.subway_data = lm(Y ~ .-fog, data=subway_data.frame)

#Show Parameters for Linear Model
lm.summary = summary(lm.subway_data)
print (lm.summary)

# PLSR
pls.subway_data = plsr(Y ~ ., data=subway_data.frame, scale=TRUE, validation="CV")
par(mfrow=c(1,1))
validationplot(pls.subway_data, val.type="MSEP")
pls.predict = predict(pls.subway_data, subway_data.frame, ncomp=4)
pls.error = sum((subway_data.frame$Y - pls.predict)^2)/(n-p)
pls.adjr2 = 1 - (pls.error/mst.full)
cat("The PLSR Model MSE is ", pls.error)
cat("The PLSE Model AdjR2 is ", pls.adjr2)
pls.summary = summary(pls.subway_data)

# Best Subset, Forward and Backward Stepwise Model
# Create function that performs prediction for a regsubsets fitted model
predict.regsubsets = function(object, newdata, id, ...){
  forms = as.formula(object$call[[2]])
  mat = model.matrix(forms, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[,xvars] %*% coefi
}

# Best Subset, Forward and Backward Stepwise with k=10 K-Fold Cross-Validation
set.seed(0)
k = 10
folds = sample(1:k, n, replace=T)
bestsubset.test.error = matrix(NA, k, p, dimnames=list(NULL, paste(1:p)))
forward.test.error = matrix(NA, k, p, dimnames=list(NULL, paste(1:p)))
backward.test.error = matrix(NA, k, p, dimnames=list(NULL, paste(1:p)))
bestsubset.test.adjr2 = matrix(NA, k, p, dimnames=list(NULL, paste(1:p)))
forward.test.adjr2 = matrix(NA, k, p, dimnames=list(NULL, paste(1:p)))
backward.test.adjr2 = matrix(NA, k, p, dimnames=list(NULL, paste(1:p)))
bestsubset.train.error = matrix(NA, k, p, dimnames=list(NULL, paste(1:p)))
forward.train.error = matrix(NA, k, p, dimnames=list(NULL, paste(1:p)))
backward.train.error = matrix(NA, k, p, dimnames=list(NULL, paste(1:p)))
bestsubset.train.adjr2 = matrix(NA, k, p, dimnames=list(NULL, paste(1:p)))
forward.train.adjr2 = matrix(NA, k, p, dimnames=list(NULL, paste(1:p)))
backward.train.adjr2 = matrix(NA, k, p, dimnames=list(NULL, paste(1:p)))

for (i in 1:k){
  mst.test = sum((subway_data.frame$Y[folds==i] - mean(subway_data.frame$Y[folds==i]))^2)/(NROW(subway_data.frame$Y[folds == i])-1)
  mst.train = sum((subway_data.frame$Y[folds!=i] - mean(subway_data.frame$Y[folds!=i]))^2)/(NROW(subway_data.frame$Y[folds != i])-1)
  bestsubset.subway_data = regsubsets(Y ~ ., data=subway_data.frame[folds != i, ], nvmax = p)
  forward.subway_data = regsubsets(Y ~ ., data=subway_data.frame[folds != i, ], nvmax = p, method="forward")
  backward.subway_data = regsubsets(Y ~ ., data=subway_data.frame[folds != i, ], nvmax = p, method="backward")

  for (j in 1:p){
    bestsubset.test.predict = predict(bestsubset.subway_data, subway_data.frame[folds == i, ], id = j)
    forward.test.predict = predict(forward.subway_data, subway_data.frame[folds == i, ], id = j)
    backward.test.predict = predict(backward.subway_data, subway_data.frame[folds == i, ], id = j)
    bestsubset.test.error[i, j] = sum((subway_data.frame$Y[folds == i] - bestsubset.test.predict)^2)/(NROW(subway_data.frame$Y[folds == i])-j)
    forward.test.error[i, j] = sum((subway_data.frame$Y[folds == i] - forward.test.predict)^2)/(NROW(subway_data.frame$Y[folds == i])-j)
    backward.test.error[i, j] = sum((subway_data.frame$Y[folds == i] - backward.test.predict)^2)/(NROW(subway_data.frame$Y[folds == i])-j)
    bestsubset.test.adjr2[i, j] = 1 - (bestsubset.test.error[i, j]/mst.test)
    forward.test.adjr2[i, j] = 1 - (forward.test.error[i, j]/mst.test)
    backward.test.adjr2[i, j] = 1 - (backward.test.error[i, j]/mst.test)
    bestsubset.train.predict = predict(bestsubset.subway_data, subway_data.frame[folds != i, ], id = j)
    forward.train.predict = predict(forward.subway_data, subway_data.frame[folds != i, ], id = j)
    backward.train.predict = predict(backward.subway_data, subway_data.frame[folds != i, ], id = j)
    bestsubset.train.error[i, j] = sum((subway_data.frame$Y[folds != i] - bestsubset.train.predict)^2)/(NROW(subway_data.frame$Y[folds != i])-j)
    forward.train.error[i, j] = sum((subway_data.frame$Y[folds != i] - forward.train.predict)^2)/(NROW(subway_data.frame$Y[folds != i])-j)
    backward.train.error[i, j] = sum((subway_data.frame$Y[folds != i] - backward.train.predict)^2)/(NROW(subway_data.frame$Y[folds != i])-j)
    bestsubset.train.adjr2[i, j] = 1 - (bestsubset.train.error[i, j]/mst.train)
    forward.train.adjr2[i, j] = 1 - (forward.train.error[i, j]/mst.train)
    backward.train.adjr2[i, j] = 1 - (backward.train.error[i, j]/mst.train)
  }
}

# Calculate MSE of Each Model for 1:p Number of Parameters
mean.bestsubset.test.error = apply(bestsubset.test.error, 2, mean)
mean.forward.test.error = apply(forward.test.error, 2, mean)
mean.backward.test.error = apply(backward.test.error, 2, mean)
mean.bestsubset.train.error = apply(bestsubset.train.error, 2, mean)
mean.forward.train.error = apply(forward.train.error, 2, mean)
mean.backward.train.error = apply(backward.train.error, 2, mean)

# Calculate MSE of Each Model for 1:p Number of Parameters
mean.bestsubset.test.adjr2 = apply(bestsubset.test.adjr2, 2, mean)
mean.forward.test.adjr2 = apply(forward.test.adjr2, 2, mean)
mean.backward.test.adjr2 = apply(backward.test.adjr2, 2, mean)
mean.bestsubset.train.adjr2 = apply(bestsubset.train.adjr2, 2, mean)
mean.forward.train.adjr2 = apply(forward.train.adjr2, 2, mean)
mean.backward.train.adjr2 = apply(backward.train.adjr2, 2, mean)

# Calculate Best Model
if (which.min(mean.bestsubset.test.error)==which.max(mean.bestsubset.test.adjr2)){
  bestsubset.model = which.max(mean.bestsubset.test.adjr2)
}else{
  bestsubset.model = which.max(mean.bestsubset.test.adjr2)
}
if (which.min(mean.forward.test.error)==which.max(mean.forward.test.adjr2)){
  forward.model = which.max(mean.forward.test.adjr2)
}else{
  forward.model = which.max(mean.forward.test.adjr2)
}
if (which.min(mean.backward.test.error)==which.max(mean.backward.test.adjr2)){
  backward.model = which.max(mean.backward.test.adjr2)
}else{
  backward.model = which.max(mean.backward.test.adjr2)
}
# Plot MSE for Each Model
par(mfrow=c(1,3))
plot(1:p, mean.bestsubset.test.error, type = 'b', pch=19, col="black", xlab="No. of Parameters", ylab="MSE", main="Best Subset Test MSE")
points(bestsubset.model, mean.backward.test.error[bestsubset.model], pch = 19, col = "red")

plot(1:p, mean.forward.test.error, type="b", pch=19, col="black", xlab="No. of Parameters", ylab="MSE", main="Forward Stepwise Test MSE")
points(forward.model, mean.forward.test.error[forward.model], pch = 20, col = "red")

plot(1:p, mean.backward.test.error, type="b", pch=19, col="black", xlab="No. of Parameters", ylab="MSE", main="Backward Stepwise Test MSE")
points(backward.model, mean.backward.test.error[backward.model], pch = 20, col = "red")

# Plot AdjR2 for Each Model
par(mfrow=c(1,3))
plot(1:p, mean.bestsubset.test.adjr2, type = 'b', pch=19, col="black", xlab="No. of Parameters", ylab="AdjR2", main="Best Subset Test AdjR2")
points(bestsubset.model, mean.backward.test.adjr2[bestsubset.model], pch = 19, col = "red", cex=2 )

plot(1:p, mean.forward.test.adjr2, type="b", pch=19, col="black", xlab="No. of Parameters", ylab="AdjR2", main="Forward Stepwise Test AdjR2")
points(forward.model, mean.forward.test.adjr2[forward.model], pch = 20, col = "red", cex = 2)

plot(1:p, mean.backward.test.adjr2, type="b", pch=19, col="black", xlab="No. of Parameters", ylab="AdjR2", main="Backward Stepwise Test AdjR2")
points(backward.model, mean.backward.test.adjr2[backward.model], pch = 20, col = "red", cex = 2)

# Print Best Size and MSE for Each Model
cat("Best Subset Model Size is ", bestsubset.model)
cat("Best Subset Cross-Validated MSE is ", mean.bestsubset.test.error[bestsubset.model])
cat("Best Subset Cross-Validated AdjR2 is ", mean.bestsubset.test.adjr2[bestsubset.model])
cat("Best Forward Stepwise Model Size is ", forward.model)
cat("Best Forward Stepwise Cross-Validated MSE is ", mean.forward.test.error[forward.model])
cat("Best Forward Stepwise Cross-Validated AdjR2 is ", mean.forward.test.adjr2[forward.model])
cat("Best Backward Stepwise Model Size is ", backward.model)
cat("Best Backward Stepwise Cross-Validated MSE is ", mean.backward.test.error[backward.model])
cat("Best Backward Stepwise Cross-Validated AdjR2 is ", mean.backward.test.adjr2[backward.model])

# Make Final Models
bestsubset.subway_data = regsubsets(Y ~ ., data=subway_data.frame, nvmax = bestsubset.model)
forward.subway_data = regsubsets(Y ~ ., data=subway_data.frame, nvmax = forward.model, method="forward")
backward.subway_data = regsubsets(Y ~ ., data=subway_data.frame, nvmax = backward.model, method="backward")

# Show Parameter Choosen for Each Model
bestsubset.coef = coef(bestsubset.subway_data, id = bestsubset.model)
print("Best Subset Parameters")
print(bestsubset.coef)
forward.coef = coef(forward.subway_data, id = forward.model)
print("Forward Stepwise Parameters")
print(forward.coef)
backward.coef = coef(backward.subway_data, id = backward.model)
print("Backward Stepwise Parameters")
print(backward.coef)

# PCR
pcr.subway_data = pcr(Y ~ ., data=subway_data.frame, scale=TRUE, validation="CV")
par(mfrow=c(1,1))
validationplot(pcr.subway_data, val.type="MSEP")
pcr.predict = predict(pcr.subway_data, data=subway_data.frame, ncomp=p)
pcr.error = sum((subway_data.frame$Y - pcr.predict)^2)/(n-p)
pcr.adjr2 = 1 - (pcr.error/mst.full)
cat("The PCR Model MSE is ", pcr.error)
cat("The PCR Model AdjR2 is ", pcr.adjr2)
pcr.summary = summary(pcr.subway_data)

# Full Regression Tree
set.seed(0)
k = 10
folds = sample(1:k, n, replace=T)
tree.test.error = matrix(NA, k, 1)
tree.test.adjr2 = matrix(NA, k, 1)
tree.train.error = matrix(NA, k, 1)
tree.train.adjr2 = matrix(NA, k, 1)
for (i in 1:k){
  mst.test = sum((subway_data.frame$Y[folds==i] - mean(subway_data.frame$Y[folds==i]))^2)/(NROW(subway_data.frame$Y[folds == i])-1)
  mst.train = sum((subway_data.frame$Y[folds!=i] - mean(subway_data.frame$Y[folds!=i]))^2)/(NROW(subway_data.frame$Y[folds != i])-1)

  tree.subway_data = tree(Y ~ ., data=subway_data.frame[folds != i, ])

  tree.test.predict = predict(tree.subway_data, newdata=subway_data.frame[folds == i, ])
  tree.test.error[i] = sum((subway_data.frame$Y[folds == i] - tree.test.predict)^2)/(NROW(subway_data.frame$Y[folds == i])-p)
  tree.test.adjr2[i] = 1 - (tree.test.error[i]/mst.test)

  tree.train.predict = predict(tree.subway_data, newdata=subway_data.frame[folds != i, ])
  tree.train.error[i] = sum((subway_data.frame$Y[folds != i] - tree.train.predict)^2)/(NROW(subway_data.frame$Y[folds != i])-p)
  tree.train.adjr2[i] = 1 - (tree.train.error[i]/mst.train)
}

# Calculate MSE of the Full Regression Tree
mean.tree.test.error = apply(tree.test.error, 2, mean)
mean.train.tree.error = apply(tree.train.error, 2, mean)

# Calculate AdjR2 of the Full Regression Tree
mean.tree.test.adjr2 = apply(tree.test.adjr2, 2, mean)
mean.tree.train.adjr2 = apply(tree.train.adjr2, 2, mean)

# Print Best MSE and AdjR2 of the Full Regression Tree
cat("The Full Regression Tree Model's MSE is", mean.tree.test.error)
cat("The Full Regression Tree Model's AdjR2 is", mean.tree.test.adjr2)

# Make Final Models
tree.subway_data = tree.subway_data = tree(Y ~ ., data=subway_data.frame)

# Display Model Features
summary(tree.subway_data)
par(mfrow=c(1,1))
plot(tree.subway_data)
text(tree.subway_data, pretty = 0)

# Pruned Regression Tree
# Create Initial Model
pruned.subway_data = cv.tree(tree.subway_data, FUN = prune.tree, K=10)

# Calculate Model With Lowest Deviance
pruned.min = pruned.subway_data$size[which.min(pruned.subway_data$dev)]
cat("The Model With the Lowest Deviance has", pruned.min, "Terminal Nodes")

# Plot Deviance Vs Model Size/Number of Terminal Nodes
par(mfrow=c(1,1))
plot(pruned.subway_data$size, pruned.subway_data$dev, type = "b", xlab = "Number of Terminal Nodes", ylab = "Deviance")
points(pruned.min, pruned.subway_data$dev[which.min(pruned.subway_data$dev)], pch = 20, col = "red", cex = 2)

# Create Final Model
prune.subway_data = prune.tree(tree.subway_data, best=pruned.min)

# Calculate MSE and AdjR2 of Pruned Regression Tree
prune.predict = predict(prune.subway_data, subway_data.frame)
prune.error = sum((subway_data.frame$Y - prune.predict)^2)/(n-p)
prune.adjr2 = 1 - (prune.error/mst.full)

# Show MSE and AdjR2 of Pruned Regression Tree
cat("The Pruned Regression Tree Model MSE is ", prune.error)
cat("The Pruned Regression Tree Model AdjR2 is", prune.adjr2)

# Display Pruned Regresssion Tree Parameters
summary(prune.subway_data)
par(mfrow=c(1,1))
plot(prune.subway_data)
text(prune.subway_data, pretty = 0)


#Random Forest
set.seed(0)
k = 10
folds = sample(1:k, n, replace=T)
rf.test.error = matrix(NA, k, 1)
rf.test.adjr2 = matrix(NA, k, 1)
rf.train.error = matrix(NA, k, 1)
rf.train.adjr2 = matrix(NA, k, 1)

for (i in 1:k){
  mst.test = sum((subway_data.frame$Y[folds==i] - mean(subway_data.frame$Y[folds==i]))^2)/(NROW(subway_data.frame$Y[folds == i])-1)
  mst.train = sum((subway_data.frame$Y[folds!=i] - mean(subway_data.frame$Y[folds!=i]))^2)/(NROW(subway_data.frame$Y[folds != i])-1)

  rf.subway_data = randomForest(Y ~ ., data=subway_data.frame[folds != i, ], mtry=p/3, ntree=500, importance=TRUE)

  rf.test.predict = predict(rf.subway_data, newdata=subway_data.frame[folds == i, ])
  rf.test.error[i] = sum((subway_data.frame$Y[folds == i] - rf.test.predict)^2)/(NROW(subway_data.frame$Y[folds == i])-p)
  rf.test.adjr2[i] = 1 - (rf.test.error[i]/mst.test)

  rf.train.predict = predict(rf.subway_data, newdata=subway_data.frame[folds != i, ])
  rf.train.error[i] = sum((subway_data.frame$Y[folds != i] - rf.train.predict)^2)/(NROW(subway_data.frame$Y[folds != i])-p)
  rf.train.adjr2[i] = 1 - (rf.train.error[i]/mst.train)
}

# Calculate MSE of the Random Forest
mean.rf.test.error = apply(rf.test.error, 2, mean)
mean.rf.train.error = apply(rf.train.error, 2, mean)

# Calculate AdjR2 of the Random Forest
mean.rf.test.adjr2 = apply(rf.test.adjr2, 2, mean)
mean.rf.train.adjr2 = apply(rf.train.adjr2, 2, mean)

# Print MSE and AdjR2 for Random Forest Model
cat("The Random Forest Model's MSE is", mean.rf.test.error)
cat("The Random Forest Model's AdjR2 is", mean.rf.test.adjr2)

# Make Final Models
rf.subway_data = randomForest(Y ~ ., data=subway_data.frame, mtry=p/3, ntree = 500, importance=TRUE)

# Display Model Features
plot (rf.subway_data)
rf.importance = importance(rf.subway_data)
print (rf.importance)

#Bagged Regression Tree
set.seed(0)
k = 10
folds = sample(1:k, n, replace=T)
bag.test.error = matrix(NA, k, 1)
bag.test.adjr2 = matrix(NA, k, 1)
bag.train.error = matrix(NA, k, 1)
bag.train.adjr2 = matrix(NA, k, 1)

for (i in 1:k){
  mst.test = sum((subway_data.frame$Y[folds==i] - mean(subway_data.frame$Y[folds==i]))^2)/(NROW(subway_data.frame$Y[folds == i])-1)
  mst.train = sum((subway_data.frame$Y[folds!=i] - mean(subway_data.frame$Y[folds!=i]))^2)/(NROW(subway_data.frame$Y[folds != i])-1)

  bag.subway_data = randomForest(Y ~ ., data=subway_data.frame[folds != i, ], mtry=p, ntree=500, importance=TRUE)

  bag.test.predict = predict(bag.subway_data, newdata=subway_data.frame[folds == i, ])
  bag.test.error[i] = sum((subway_data.frame$Y[folds == i] - bag.test.predict)^2)/(NROW(subway_data.frame$Y[folds == i])-p)
  bag.test.adjr2[i] = 1 - (bag.test.error[i]/mst.test)

  bag.train.predict = predict(bag.subway_data, newdata=subway_data.frame[folds != i, ])
  bag.train.error[i] = sum((subway_data.frame$Y[folds != i] - bag.train.predict)^2)/(NROW(subway_data.frame$Y[folds != i])-p)
  bag.train.adjr2[i] = 1 - (bag.train.error[i]/mst.train)
}

# Calculate MSE of the Bagged Regression Tree Model
mean.bag.test.error = apply(bag.test.error, 2, mean)
mean.bag.train.error = apply(bag.train.error, 2, mean)

# Calculate AdjR2 of the Bagged Regression Tree Model
mean.bag.test.adjr2 = apply(bag.test.adjr2, 2, mean)
mean.bag.train.adjr2 = apply(bag.train.adjr2, 2, mean)

# Print MSE and AdjR2 of the Bagged Regression Tree Model
cat("The Bagged Regression Tree Model's MSE is", mean.bag.test.error)
cat("The Bagged Regression Tree Model's AdjR2 is", mean.bag.test.adjr2)

# Make Final Models
bag.subway_data = randomForest(Y ~ ., data=subway_data.frame, mtry=p, ntree = 500, importance=TRUE)

# Display Model Features
plot (bag.subway_data)
bag.importance = importance(bag.subway_data)
print (bag.importance)

#Booseted Regression Tree Lambda Selection
set.seed(0)
lambda.seq = seq(0.001, 2, by = 0.001)
folds = sample(1:length(lambda.seq), n, replace=T)
boost.train.error = matrix(NA, length(lambda.seq), 1)
boost.test.error = matrix(NA, length(lambda.seq), 1)
boost.train.adjr2 = matrix(NA, length(lambda.seq), 1)
boost.test.adjr2 = matrix(NA, length(lambda.seq), 1)

for (i in 1:length(lambda.seq)){
  mst.test = sum((subway_data.frame$Y[folds==i] - mean(subway_data.frame$Y[folds==i]))^2)/(NROW(subway_data.frame$Y[folds == i])-1)
  mst.train = sum((subway_data.frame$Y[folds!=i] - mean(subway_data.frame$Y[folds!=i]))^2)/(NROW(subway_data.frame$Y[folds != i])-1)

  boost.subway_data = gbm(Y ~ ., data=subway_data.frame[folds != i, ], distribution = "gaussian", n.trees = 1000, shrinkage = lambda.seq[i])

  boost.test.predict = predict(boost.subway_data, newdata=subway_data.frame[folds==i, ], n.trees = 1000)
  boost.test.error[i] = sum((subway_data.frame$Y[folds==i] - boost.test.predict)^2)/(NROW(subway_data.frame$Y[folds == i])-p)
  boost.test.adjr2[i] = 1 - (boost.test.error[i]/mst.test)

  boost.train.predict = predict(boost.subway_data, newdata=subway_data.frame[folds != i, ], n.trees = 1000)
  boost.train.error[i] = sum((subway_data.frame$Y[folds != i] - boost.train.predict)^2)/(NROW(subway_data.frame$Y[folds != i])-p)
  boost.train.adjr2[i] = 1 - (boost.train.error[i]/mst.train)
}


# Plot Test and Train MSE for Lambda
plot(lambda.seq, boost.test.error, type = "b", pch=19, col="blue", xlab = "Lambda Value", ylab = "MSE")
#lines(lambda.seq, boost.train.error, type = "b", pch=19, col="green")
#legend("topright", pch=19, col=c("blue", "green"), c("Train MSE", "Test MSE"))
#grid()

# Plot Test and Train AdjR2 for Lambda
plot(lambda.seq, boost.test.adjr2, type = "b", pch=19, col="blue", xlab = "Lambda Value", ylab = "AdjR2")
#lines(lambda.seq, boost.train.adjr2, type = "b", pch=19, col="green")
#legend("topright", pch=19, col=c("blue", "green"), c("Train AdjR2", "Test AdjR2"))
#grid()

# Determine Appropriate Lambda
if(which.min(boost.test.error)==which.max(boost.test.adjr2)){
  lambda = lambda.seq[which.max(boost.test.adjr2)]
}else{
  lambda = lambda.seq[which.max(boost.test.adjr2)]
}
print (boost.test.error)
# Boosted Regression Tree with k=10 K-Fold Cross-Validation
set.seed(0)
k = 10
folds = sample(1:k, n, replace=T)
boost.train.2.error = matrix(NA, k, 1)
boost.test.2.error = matrix(NA, k, 1)
boost.train.2.adjr2 = matrix(NA, k, 1)
boost.test.2.adjr2 = matrix(NA, k, 1)

for (i in 1:k){
  mst.test = sum((subway_data.frame$Y[folds==i] - mean(subway_data.frame$Y[folds==i]))^2)/(NROW(subway_data.frame$Y[folds == i])-1)
  mst.train = sum((subway_data.frame$Y[folds!=i] - mean(subway_data.frame$Y[folds!=i]))^2)/(NROW(subway_data.frame$Y[folds != i])-1)

  boost.subway_data = gbm(Y ~ ., data=subway_data.frame[folds != i, ], distribution = "gaussian", n.trees = 1000, shrinkage = lambda)

  boost.test.2.predict = predict(boost.subway_data, newdata = subway_data.frame[folds==i, ], n.trees = 1000)
  boost.test.2.error[i] = sum((subway_data.frame$Y[folds==i] - boost.test.2.predict)^2)/(NROW(subway_data.frame$Y[folds == i])-p)
  boost.test.2.adjr2[i] = 1 - (boost.test.2.error[i]/mst.test)

  boost.train.2.predict = predict(boost.subway_data, subway_data.frame[folds != i, ], n.trees = 1000)
  boost.train.2.error[i] = sum((subway_data.frame$Y[folds != i] - boost.train.2.predict)^2)/(NROW(subway_data.frame$Y[folds != i])-p)
  boost.train.2.adjr2[i] = 1 - (boost.train.2.error[i]/mst.train)
}

#Calculate MSE of Boosted Regression Tree Model
mean.boost.test.error = apply(boost.test.2.error, 2, mean)
mean.boost.train.error = apply(boost.train.2.error, 2, mean)

#Calculate AdjR2 of Boosted Regression Tree Model
mean.boost.test.adjr2 = apply(boost.test.2.adjr2, 2, mean)
mean.boost.train.adjr2 = apply(boost.train.2.adjr2, 2, mean)

# Print MSE and AdjR2 for Boosted Regression Tree Model
cat("The Boosted Regression Tree Model MSE is ", mean.boost.test.error)
cat("The Boosted Regression Tree Model AdjR2 is ", mean.boost.train.adjr2)

#Make Final Boosted Regression Tree Model
boost.subway_data = gbm(Y ~ ., data=subway_data.frame, distribution = "gaussian", n.trees = 1000, shrinkage = lambda)

#Show Parameters for Boosted Regression Tree Model
boost.summary = summary(boost.subway_data)
print (boost.summary)

# Ridge Regression with k=10 K-Fold Cross-Validation
set.seed(0)
k = 10
folds = sample(1:k, n, replace=T)
ridge.test.error = matrix(NA, k, 1)
ridge.test.adjr2 = matrix(NA, k, 1)
ridge.train.error = matrix(NA, k, 1)
ridge.train.adjr2 = matrix(NA, k, 1)

for (i in 1:k){
  mst.test = sum((subway_data.frame$Y[folds==i] - mean(subway_data.frame$Y[folds==i]))^2)/(NROW(subway_data.frame$Y[folds == i])-1)
  mst.train = sum((subway_data.frame$Y[folds!=i] - mean(subway_data.frame$Y[folds!=i]))^2)/(NROW(subway_data.frame$Y[folds != i])-1)
  subway_data.matrix.train = model.matrix(Y ~ ., data=subway_data.frame[folds!=i, ])
  subway_data.matrix.test = model.matrix(Y ~ ., data=subway_data.frame[folds == i, ])

  ridge.lambda = cv.glmnet(subway_data.matrix.train, subway_data.frame$Y[folds!=i], alpha=0)$lambda.min
  ridge.subway_data = glmnet(subway_data.matrix.train, subway_data.frame$Y[folds!=i], alpha=0)

  ridge.test.predict = predict(ridge.subway_data, newx=subway_data.matrix.test, s=ridge.lambda)
  ridge.test.error[i] = sum((subway_data.frame$Y[folds == i] - ridge.test.predict)^2)/(NROW(subway_data.frame$Y[folds == i])-p)
  ridge.test.adjr2[i] = 1 - (ridge.test.error[i]/mst.test)

  ridge.train.predict = predict(ridge.subway_data, newx=subway_data.matrix.train, s=ridge.lambda)
  ridge.train.error[i] = sum((subway_data.frame$Y[folds != i] - ridge.train.predict)^2)/(NROW(subway_data.frame$Y[folds != i])-p)
  ridge.train.adjr2[i] = 1 - (ridge.train.error[i]/mst.train)
}

#Calculate MSE of Ridge Regression Model
mean.ridge.test.error = apply(ridge.test.error, 2, mean)
mean.ridge.train.error = apply(ridge.train.error, 2, mean)

#Calculate AdjR2 of Ridge Regression Model
mean.ridge.test.adjr2 = apply(ridge.test.adjr2, 2, mean)
mean.ridge.train.adjr2 = apply(ridge.train.adjr2, 2, mean)

# Print MSE and AdjR2 for Ridge Regression Model
cat("The Ridge Regression MSE is ", mean.ridge.test.error)
cat("The Ridge Regression AdjR2 is ", mean.ridge.train.adjr2)

#Make Final Ridge Regression Model
subway_data.matrix = model.matrix(Y ~ ., data=subway_data.frame)
ridge.subway_data = glmnet(subway_data.matrix, subway_data.frame$Y, alpha=0)

#Show Parameters for Ridge Regression Model
ridge.lambda = cv.glmnet(subway_data.matrix.train, subway_data.frame$Y[folds!=i], alpha=0)$lambda.min
ridge.coef = predict(ridge.subway_data, type="coefficients", s=ridge.lambda)
print (ridge.coef)

#Lasso with k=10 K-Fold Cross-Validation
set.seed(0)
k = 10
folds = sample(1:k, n, replace=T)
lasso.test.error = matrix(NA, k, 1)
lasso.test.adjr2 = matrix(NA, k, 1)
lasso.train.error = matrix(NA, k, 1)
lasso.train.adjr2 = matrix(NA, k, 1)

for (i in 1:k){
  mst.test = sum((subway_data.frame$Y[folds==i] - mean(subway_data.frame$Y[folds==i]))^2)/(NROW(subway_data.frame$Y[folds == i])-1)
  mst.train = sum((subway_data.frame$Y[folds!=i] - mean(subway_data.frame$Y[folds!=i]))^2)/(NROW(subway_data.frame$Y[folds != i])-1)
  subway_data.matrix.train = model.matrix(Y ~ ., data=subway_data.frame[folds!=i, ])
  subway_data.matrix.test = model.matrix(Y ~ ., data=subway_data.frame[folds == i, ])

  lasso.lambda = cv.glmnet(subway_data.matrix.train, subway_data.frame$Y[folds!=i], alpha=1)$lambda.min
  lasso.subway_data = glmnet(subway_data.matrix.train, subway_data.frame$Y[folds!=i], alpha=1)

  lasso.test.predict = predict(lasso.subway_data, newx=subway_data.matrix.test, s=lasso.lambda)
  lasso.test.error[i] = sum((subway_data.frame$Y[folds == i] - lasso.test.predict)^2)/(NROW(subway_data.frame$Y[folds == i])-p)
  lasso.test.adjr2[i] = 1 - (lasso.test.error[i]/mst.test)

  lasso.train.predict = predict(lasso.subway_data, newx=subway_data.matrix.train, s=lasso.lambda)
  lasso.train.error[i] = sum((subway_data.frame$Y[folds != i] - lasso.train.predict)^2)/(NROW(subway_data.frame$Y[folds != i])-p)
  lasso.train.adjr2[i] = 1 - (lasso.train.error[i]/mst.train)
}

#Calculate MSE of Lasso Model
mean.lasso.test.error = apply(lasso.test.error, 2, mean)
mean.lasso.train.error = apply(lasso.train.error, 2, mean)

#Calculate AdjR2 of Lasso Model
mean.lasso.test.adjr2 = apply(lasso.test.adjr2, 2, mean)
mean.lasso.train.adjr2 = apply(lasso.train.adjr2, 2, mean)

# Print MSE and AdjR2 for Lasso Model
cat("The Lasso MSE is ", mean.lasso.test.error)
cat("The Lasso AdjR2 is ", mean.lasso.train.adjr2)

#Make Final Lasso Model
subway_data.matrix = model.matrix(Y ~ ., data=subway_data.frame)
lasso.subway_data = glmnet(subway_data.matrix, subway_data.frame$Y, alpha=1)

#Show Parameters for Lasso Model
lasso.lambda = cv.glmnet(subway_data.matrix.train, subway_data.frame$Y[folds!=i], alpha=1)$lambda.min
lasso.coef = predict(lasso.subway_data, type="coefficients", s=lasso.lambda)
print (lasso.coef)

#Gaussian Processes with k=10 K-Fold Cross-Validation
set.seed(0)
k = 10
folds = sample(1:k, n, replace=T)
bgp.test.error = matrix(NA, k, 1)
bgp.test.adjr2 = matrix(NA, k, 1)
bgp.train.error = matrix(NA, k, 1)
bgp.train.adjr2 = matrix(NA, k, 1)

for (i in 1:k){
  mst.test = sum((subway_data.frame$Y[folds==i] - mean(subway_data.frame$Y[folds==i]))^2)/(NROW(subway_data.frame$Y[folds == i])-1)
  mst.train = sum((subway_data.frame$Y[folds!=i] - mean(subway_data.frame$Y[folds!=i]))^2)/(NROW(subway_data.frame$Y[folds != i])-1)

  bgp.subway_data = btgp(subway_data.frame[folds != i, ][,c(seq(2,p+1,by=1))], subway_data.frame$Y[folds != i])

  bgp.predict = predict(bgp.subway_data, subway_data.frame[folds == i, ][,c(seq(2,p+1,by=1))], pred.n=FALSE)
  bgp.test.error[i] = sum((subway_data.frame$Y[folds==i] - bgp.predict$ZZ.mean)^2)/(NROW(subway_data.frame$Y[folds==i]) - p)
  bgp.test.adjr2[i] = 1 - (bgp.test.error[i]/mst.test)
  
  bgp.train.error[i] = sum((subway_data.frame$Y[folds!=i] - bgp.subway_data$Zp.mean)^2) / (NROW(subway_data.frame$Y[folds!=i]) - p)
  bgp.train.adjr2[i] = 1 - (bgp.train.error[i]/mst.train)
}


#Calculate MSE of Gaussian Processes Model
mean.bgp.test.error = apply(bgp.test.error, 2, mean)
mean.bgp.train.error = apply(bgp.train.error, 2, mean)

#Calculate AdjR2 of Gaussian Processes Model
mean.bgp.test.adjr2 = apply(bgp.test.adjr2, 2, mean)
mean.bgp.train.adjr2 = apply(bgp.train.adjr2, 2, mean)

# Print MSE and AdjR2 for Gaussian Processes Model
cat("The Gaussian Processes Train MSE is ", mean.bgp.train.error)
cat("The Gaussian Processes Train AdjR2 is ", mean.bgp.train.adjr2)
cat("The Gaussian Processes Test MSE is ", mean.bgp.test.error)
cat("The Gaussian Processes Test AdjR2 is ", mean.bgp.test.adjr2)

# Plot Gaussian Processes Model
plot(bgp.subway_data)
