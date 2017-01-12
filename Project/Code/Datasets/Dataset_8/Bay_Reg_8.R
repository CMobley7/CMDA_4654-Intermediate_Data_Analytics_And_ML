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
if( ! require("coda") ){ install.packages("coda") }

# Load Cleaned Data
subway_data = read.csv("data_set_8.csv")

# Create Data Frame
subway_data.frame = data.frame(Y = subway_data$ENTRIESn_hourly, day_hour=subway_data$day_hour, latitude = subway_data$latitude, longitude = subway_data$longitude, rain = subway_data$rain, fog = subway_data$fog, meanprecipi = subway_data$meanprecipi, meanpressurei = subway_data$meanpressurei, meantempi = subway_data$meantempi, meanwspdi = subway_data$meanwspdi)

# Remove Test Point
test_point <- subway_data.frame[100, ,drop=FALSE]
subway_data.frame <- subway_data.frame[-100, ,drop=FALSE]

y <- subway_data.frame[ ,1]
x <- subway_data.frame[ ,c(2,3,4,5,6,7,8,9,10)]
x <- x[(y > 0),]
y <- y[(y > 0)]
y <- log(y)

#*** State Prior Specifications
m0 <- 1  #mu_0 
m1 <- 1  #mu_Z 
m2 <- 1  #mu_W 
m3 <- 1  #mu_M 
m4 <- 1  #mu_0 
m5 <- 1  #mu_Z 
m6 <- 1  #mu_W 
m7 <- 1  #mu_M 
m8 <- 1  #mu_0 
m9 <- 1  #mu_Z 

g <- 10
a <- 0.1
b <- 0.1

#*** Constant that we will want during the MCMC
n <- dim(x)[[1]]
p <- 9+1  #number of model coefficients

#*** Provide MCMC specifications
I <- 100000
betaStore <- matrix(0,I,p)
phiStore <- matrix(0,I,1)
yPredStore <- matrix(0,I,1)
colnames(betaStore) <- c("beta0","beta1","beta2","beta3","beta4","beta5","beta6","beta7","beta8","beta9")
colnames(phiStore) <- "Phi"

#*** Provide starting values
betaCur0 <- m0
betaCur1 <- m1
betaCur2 <- m2
betaCur3 <- m3
betaCur4 <- m4
betaCur5 <- m5
betaCur6 <- m6
betaCur7 <- m7
betaCur8 <- m8
betaCur9 <- m9
phiCur <- 1 

#*** Implement Gibbs using a for-loop
for (c in 1:I){
  
  #Draw Beta_0
  yStar0 <- y - betaCur1*x[,1] - betaCur2*x[,2] - betaCur3*x[,3] - betaCur4*x[,4] - betaCur5*x[,5] - betaCur6*x[,6] - betaCur7*x[,7] - betaCur8*x[,8] - betaCur9*x[,9]
  mnStar0 <- (sum(yStar0) + m0/g) / (n + (1/g))
  sdStar0 <- ((phiCur*n) + (phiCur/g))^(-1/2)
  betaCur0 <- rnorm(1,mnStar0,sdStar0)
  
  #Draw Beta_1
  yStar1 <- y - betaCur0 - betaCur2*x[,2] - betaCur3*x[,3] - betaCur4*x[,4] - betaCur5*x[,5] - betaCur6*x[,6] - betaCur7*x[,7] - betaCur8*x[,8] - betaCur9*x[,9]
  mnStar1 <- (sum(yStar1*x[,1]) + (m1/g)) / (sum(x[,1]^2) + (1/g))
  sdStar1 <- ((phiCur*sum(x[,1]^2)) + (phiCur/g))^(-1/2)
  betaCur1 <- rnorm(1,mnStar1,sdStar1)
  
  #Draw Beta_2
  yStar2 <- y - betaCur0 - betaCur1*x[,1] - betaCur3*x[,3] - betaCur4*x[,4] - betaCur5*x[,5] - betaCur6*x[,6] - betaCur7*x[,7] - betaCur8*x[,8] - betaCur9*x[,9]
  mnStar2 <- (sum(yStar2*x[,2]) + (m2/g)) / (sum(x[,2]^2) + (1/g))
  sdStar2 <- ((phiCur*sum(x[,2]^2)) + (phiCur/g))^(-1/2)
  betaCur2 <- rnorm(1,mnStar2,sdStar2)
  
  #Draw Beta_3
  yStar3 <- y - betaCur0 - betaCur1*x[,1] - betaCur2*x[,2] - betaCur4*x[,4] - betaCur5*x[,5] - betaCur6*x[,6] - betaCur7*x[,7] - betaCur8*x[,8] - betaCur9*x[,9]
  mnStar3 <- (sum(yStar3*x[,3]) + (m3/g)) / (sum(x[,3]^2) + (1/g))
  sdStar3 <- ((phiCur*sum(x[,3]^2)) + (phiCur/g))^(-1/2)
  betaCur3 <- rnorm(1,mnStar3,sdStar3)
  
  #Draw Beta_4
  yStar4 <- y - betaCur0 - betaCur1*x[,1] - betaCur2*x[,2] - betaCur3*x[,3] - betaCur5*x[,5] - betaCur6*x[,6] - betaCur7*x[,7] - betaCur8*x[,8] - betaCur9*x[,9]
  mnStar4 <- (sum(yStar4*x[,4]) + (m4/g)) / (sum(x[,4]^2) + (1/g))
  sdStar4 <- ((phiCur*sum(x[,4]^2)) + (phiCur/g))^(-1/2)
  betaCur4 <- rnorm(1,mnStar4,sdStar4)
  
  #Draw Beta_5
  yStar5 <- y - betaCur0 - betaCur1*x[,1] - betaCur2*x[,2] - betaCur3*x[,3] - betaCur4*x[,4] - betaCur6*x[,6] - betaCur7*x[,7] - betaCur8*x[,8] - betaCur9*x[,9]
  mnStar5 <- (sum(yStar5*x[,5]) + (m5/g)) / (sum(x[,5]^2) + (1/g))
  sdStar5 <- ((phiCur*sum(x[,5]^2)) + (phiCur/g))^(-1/2)
  betaCur5 <- rnorm(1,mnStar5,sdStar5)
  
  #Draw Beta_6
  yStar6 <- y - betaCur0 - betaCur1*x[,1] - betaCur2*x[,2] - betaCur3*x[,3] - betaCur4*x[,4] - betaCur5*x[,5] - betaCur7*x[,7] - betaCur8*x[,8] - betaCur9*x[,9]
  mnStar6 <- (sum(yStar6*x[,6]) + (m6/g)) / (sum(x[,6]^2) + (1/g))
  sdStar6 <- ((phiCur*sum(x[,6]^2)) + (phiCur/g))^(-1/2)
  betaCur6 <- rnorm(1,mnStar6,sdStar6)
  
  #Draw Beta_7
  yStar7 <- y - betaCur0 - betaCur1*x[,1] - betaCur2*x[,2] - betaCur3*x[,3] - betaCur4*x[,4] - betaCur5*x[,5] - betaCur6*x[,6] - betaCur8*x[,8] - betaCur9*x[,9]
  mnStar7 <- (sum(yStar7*x[,7]) + (m7/g)) / (sum(x[,7]^2) + (1/g))
  sdStar7 <- ((phiCur*sum(x[,7]^2)) + (phiCur/g))^(-1/2)
  betaCur7 <- rnorm(1,mnStar7,sdStar7)
  
  #Draw Beta_8
  yStar8 <- y - betaCur0 - betaCur1*x[,1] - betaCur2*x[,2] - betaCur3*x[,3] - betaCur4*x[,4] - betaCur5*x[,5] - betaCur6*x[,6] - betaCur7*x[,7] - betaCur9*x[,9]
  mnStar8 <- (sum(yStar8*x[,8]) + (m8/g)) / (sum(x[,8]^2) + (1/g))
  sdStar8 <- ((phiCur*sum(x[,8]^2)) + (phiCur/g))^(-1/2)
  betaCur8 <- rnorm(1,mnStar8,sdStar8)
  
  #Draw Beta_9
  yStar9 <- y - betaCur0 - betaCur1*x[,1] - betaCur2*x[,2] - betaCur3*x[,3] - betaCur4*x[,4] - betaCur5*x[,5] - betaCur6*x[,6] - betaCur7*x[,7] - betaCur8*x[,8]
  mnStar9 <- (sum(yStar9*x[,9]) + (m9/g)) / (sum(x[,9]^2) + (1/g))
  sdStar9 <- ((phiCur*sum(x[,9]^2)) + (phiCur/g))^(-1/2)
  betaCur9 <- rnorm(1,mnStar9,sdStar9)
  
  #Draw phi
  aStar <- n/2+p/2+a 
  
  bTemp1 <- .5*sum( (y - betaCur0 - betaCur1*x[,1] - betaCur2*x[,2] - betaCur3*x[,3] - betaCur4*x[,4] - betaCur5*x[,5] - betaCur6*x[,6] - betaCur7*x[,7] - betaCur8*x[,8] - betaCur9*x[,9])^2)
  bTemp2 <- .5*(1/g)*( (betaCur0-m0)^2+(betaCur1-m1)^2+(betaCur2-m2)^2+(betaCur3-m3)^2+(betaCur4-m4)^2+(betaCur5-m5)^2+(betaCur6-m6)^2+(betaCur7-m7)^2+(betaCur8-m8)^2+(betaCur9-m9)^2)
  bStar <- bTemp1+bTemp2+b  
  
  phiCur <- rgamma(1,aStar,bStar)
  
  #predY
  muPred <- betaCur0+betaCur1*test_point[1,1]+betaCur2*test_point[1,2]+betaCur3*test_point[1,3]+betaCur4*test_point[1,4]+betaCur5*test_point[1,5]+betaCur6*test_point[1,6]+betaCur7*test_point[1,7]+betaCur8*test_point[1,8]+betaCur9*test_point[1,9]
  sdPred <- sqrt(1/phiCur)
  yPredStore[c,1] <- rnorm(1, muPred, sdPred)
  
  #Store the draws
  betaStore[c,1] <- betaCur0
  betaStore[c,2] <- betaCur1
  betaStore[c,3] <- betaCur2
  betaStore[c,4] <- betaCur3
  betaStore[c,5] <- betaCur4
  betaStore[c,6] <- betaCur5
  betaStore[c,7] <- betaCur6
  betaStore[c,8] <- betaCur7
  betaStore[c,9] <- betaCur8
  betaStore[c,10] <- betaCur9
  phiStore[c,1] <- phiCur
}  

#remove Burn-in
BI <- 1000
noBiBeta <- betaStore[-c(1:BI),]
noBiPhi <- phiStore[-c(1:BI),]
noBiY <- yPredStore[-c(1:BI),]
par(mfrow=c(1,1))
plot(noBiBeta[,1], main="Beta0", type="l")
plot(noBiBeta[,2], main="Beta1", type="l")
plot(noBiBeta[,3], main="Beta2", type="l")
plot(noBiBeta[,4], main="Beta3", type="l")
plot(noBiBeta[,5], main="Beta4", type="l")
plot(noBiBeta[,6], main="Beta5", type="l")
plot(noBiBeta[,7], main="Beta6", type="l")
plot(noBiBeta[,8], main="Beta7", type="l")
plot(noBiBeta[,9], main="Beta8", type="l")
plot(noBiBeta[,10], main="Beta9", type="l")
plot(noBiPhi, main="Phi", type="l")
plot(noBiY, main="Y", type="l")

test <- geweke.diag(as.mcmc(noBiBeta[,1]), frac1=0.1, frac2=0.5)
pVal <- 1-pnorm(abs(test$z), 0, 1)

ex0 <- mean(noBiBeta[,1])
ex1 <- mean(noBiBeta[,2])
ex2 <- mean(noBiBeta[,3])
ex3 <- mean(noBiBeta[,4])
ex4 <- mean(noBiBeta[,5])
ex5 <- mean(noBiBeta[,6])
ex6 <- mean(noBiBeta[,7])
ex7 <- mean(noBiBeta[,8])
ex8 <- mean(noBiBeta[,9])
ex9 <- mean(noBiBeta[,10])
exPh <- mean(noBiPhi)
exY <- mean(noBiY)

ci0 <- quantile(noBiBeta[,1], prob=c(.025, .975))
ci1 <- quantile(noBiBeta[,2], prob=c(.025, .975))
ci2 <- quantile(noBiBeta[,3], prob=c(.025, .975))
ci3 <- quantile(noBiBeta[,4], prob=c(.025, .975))
ci4 <- quantile(noBiBeta[,5], prob=c(.025, .975))
ci5 <- quantile(noBiBeta[,6], prob=c(.025, .975))
ci6 <- quantile(noBiBeta[,7], prob=c(.025, .975))
ci7 <- quantile(noBiBeta[,8], prob=c(.025, .975))
ci8 <- quantile(noBiBeta[,9], prob=c(.025, .975))
ci9 <- quantile(noBiBeta[,10], prob=c(.025, .975))
ciPh <-quantile(noBiPhi, prob=c(.025, .975))

pr0 <- sum(noBiBeta[,1]>0)/(I-BI)
pr1 <- sum(noBiBeta[,2]>0)/(I-BI)
pr2 <- sum(noBiBeta[,3]>0)/(I-BI)
pr3 <- sum(noBiBeta[,4]>0)/(I-BI)
pr4 <- sum(noBiBeta[,5]>0)/(I-BI)
pr5 <- sum(noBiBeta[,6]>0)/(I-BI)
pr6 <- sum(noBiBeta[,7]>0)/(I-BI)
pr7 <- sum(noBiBeta[,8]>0)/(I-BI)
pr8 <- sum(noBiBeta[,9]>0)/(I-BI)
pr9 <- sum(noBiBeta[,10]>0)/(I-BI)
