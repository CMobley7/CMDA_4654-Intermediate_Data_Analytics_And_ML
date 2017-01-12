# Exercise 2.4
# 8
# a) Use the read.csv() function to read the data into R. Call the loaded data college.
#Make sure that you have the directory set to the correct location for the data.
college = read.csv("College.csv")
# b)	Look at the data using the fix() function. You should notice that the first
# column is just the name of each university. We don't really want R to treat this
# as data. However, it may be handy to have these names for later. Try the following commands:
fix(college)
rownames(college) = college[,1]
fix(college)
# You should see that there is now a row.names column with the name of each university recorded. 
# This means that R has given each row a name corresponding to the appropriate university. 
# R will not try to perform calculations on the row names. However, we still need to 
# eliminate the first column in the data where the names are stored. Try
college = college[,-1]
fix(college)
# Now you should see that the first data column is Private. Note that another column labeled 
# row.names now appears before the Private column. However, this is not a data column but 
# rather the name that R  is giving to each row.
# i. Use the summary() function to produce a numerical summary of the variables in the data set.
summary(college)
# ii. Use the pairs() function to produce a scatterplot matrix of the first ten columns or 
# variables of the data. Recall that you can reference the firstten columns of a matrix A using A[,1:10].
pairs(college[,1:10])
# iii. Use the plot() function to produce side-by-side boxplots of Outstate versus Private.
plot(college$Private, college$Outstate, col="red", varwidth=T,xlab="Private",ylab="Out-of-State Tuition")
# iv. Create a new qualitative variable, called Elite, by binning the Top10perc variable. 
# We are going to divide universities into two groups based on whether or not the 
# proportion of students coming from the top 
Elite = rep("No", nrow(college))
Elite[college$Top10perc>50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
# Use the summary() function to see how many elite universities there are.
summary(college$Elite)
# Now use the plot() function to produce side-by-side boxplots of Outstate versus Elite.
plot(college$Elite, college$Outstate, col="red", varwidth=T,xlab="Elite",ylab="Out-of-State Tuition")
# v. Use the hist() function to produce some histograms with differing numbers of bins
# for a few of the quantitative variables. You may find the command par(mfrow=c(2,2))
# useful: it will divide the print window into four regions so that four plots can be
# made simultaneously. Modifying the arguments to this function will divide the screen
# in other ways
par(mfrow=c(2,2))
hist(college$Top10perc)
hist(college$F.Undergrad)
hist(college$S.F.Ratio)
hist(college$Grad.Rate)
# vi. vi. Continue exploring( the data, and provide a brief summary of what you Discover.
par(mfrow=c(1,1))
# Colleges with low acceptance rate tend to have higher graduation ratio.
plot(college$Accept / college$Apps, college$Grad.Rate, xlab="Acceptance Rate", ylab="Graduation Rate")
# Colleges that accept the most students from the top 10% tend to have the highest graduation rates
plot(college$Top10perc, college$Grad.Rate, xlab="Percentage of Students from the top 10% of their high schools", ylab="Graduation Rate")

# Exercise 3.7
# 9
library(MASS)
library(ISLR)
fix(Auto)
# a. Produce a scatterplot matrix which includes all of the variables in the data set
pairs(Auto)
# b. Compute the matrix of correlations between the variables using the function cor(). 
# You will need to exclude the name variable, cor() which is qualitative.
cor(Auto[sapply(Auto, is.numeric)])
#c Use the lm() function to perform a multiple linear regression with mpg as the response 
# and all other variables except name as the predictors. Use the summary() function to print
# the results. Comment on the output. For instance:
lm.fit = lm(mpg~.-name, data=Auto)
summary(lm.fit)
# d) Use the plot() function to produce diagnostic plots of the linear regression fit. 
# Comment on any problems you see with the fit. Do the residual plots suggest any unusually
# large outliers? Does the leverage plot identify any observations with unusually high leverage?
par(mfrow=c(2,2))
plot(lm.fit)
# e) Use the * and : symbols to fit linear regression models with interaction effects.
# Do any interactions appear to be statistically significant?
lm.fit1 = lm(mpg~.-name+cylinders:displacement+cylinders:horsepower+cylinders:weight+cylinders:acceleration
             +cylinders:year+cylinders:origin+displacement:horsepower+displacement:weight+displacement:acceleration
             +displacement:year+displacement:origin+horsepower:weight+horsepower:acceleration+horsepower:year+horsepower:origin
             +weight:acceleration+weight:year+weight:origin+acceleration:year+acceleration:origin+year:origin, data=Auto)
summary(lm.fit1)
# f)	Try a few different transformations of the variables, such as log(X), ???X, X2. Comment on your findings.
lm.fit2 = lm(mpg~log(displacement)+sqrt(weight)+I(acceleration^2), data=Auto)
summary(lm.fit2)
# 13
# In this exercise you will create some simulated data and will fit simple linear regression models to it. 
# Make sure to use set.seed(1) prior to starting part (a) to ensure consistent results.
set.seed(1)
# a) Using the rnorm() function, create a vector, x, containing 100 observations drawn from a N(0, 1) distribution. 
# This represents a feature, X.
x = rnorm(100, mean=0, sd=1)
# b) Using the rnorm() function, create a vector, eps, containing 100 observations drawn from a N(0, 0.25) 
# distribution i.e. a normal distribution with mean zero and variance 0.25.
eps = rnorm(100, mean=0, sd=sqrt(0.25))
# c) Using x and eps, generate a vector y according to the model  What is the length of the vector y? 
# What are the values of ??0 and ??1 in this linear model?
y = -1 + 0.5*x + eps
# d) Create a scatterplot displaying the relationship between x and y. Comment on what you observe.
plot(x,y)
# e) Fit a least squares linear model to predict y using x. Comment on the model obtained. 
# How do ??^0 and ??^1 compare to ??0 and ??1?
lm.fit = lm(y~x)
summary(lm.fit)
# f)	Display the least squares line on the scatterplot obtained in (d). Draw the population regression 
# line on the plot, in a different color. Use the legend() command to create an appropriate legend.
plot(x,y)
abline(lm.fit, lwd=2, col="blue")
abline(-1.0, 0.5, lwd=2, col="green")
legend("topleft", c("least squares model","population regression"), lty=c(1,1),lwd=c(2.5,2.5),col=c("blue","green"))
# g)	Now fit a polynomial regression model that predicts y using x and x2. Is there evidence that
# the quadratic term improves the model fit? Explain your answer.
lm.fit1 = lm(y~x+I(x^2))
summary(lm.fit1)
# h)	Repeat (a)-(f) after modifying the data generation process in such a way that there is less noise in the data.
# The model (3.39) should remain the same. You can do this by decreasing the variance of the normal distribution 
# used to generate the error term in (b). Describe your results.
set.seed(1)
x1 = rnorm(100, mean=0, sd=1)
eps1 = rnorm(100, mean=0, sd=sqrt(0.025))
y1 = -1 + 0.5*x1 + eps1
plot(x1,y1)
lm.fit2 = lm(y1~x1)
summary(lm.fit2)
plot(x1,y1)
abline(lm.fit2, lwd=2, col="blue")
abline(-1.0, 0.5, lwd=2, col="green")
legend("topleft", c("least squares model","population regression"), lty=c(1,1),lwd=c(2.5,2.5),col=c("blue","green"))
# i) Repeat (a)-(f) after modifying the data generation process in such a way that there is more noise in the data.
# The model (3.39) should remain the same. You can do this by increasing the variance of the normal distribution
# used to generate the error term in (b). Describe your results.
set.seed(1)
x2 = rnorm(100, mean=0, sd=1)
eps2 = rnorm(100, mean=0, sd=sqrt(0.525))
y2 = -1 + 0.5*x2 + eps2
plot(x2,y2)
lm.fit3 = lm(y2~x2)
summary(lm.fit3)
plot(x2,y2)
abline(lm.fit3, lwd=2, col="blue")
abline(-1.0, 0.5, lwd=2, col="green")
legend("topleft", c("least squares model","population regression"), lty=c(1,1),lwd=c(2.5,2.5),col=c("blue","green"))
# j) What are the confidence intervals for ??0 and ??1 based on the original data set, the noisier data set, 
# and the less noisy data set? Comment on your results.
confint(lm.fit)
confint(lm.fit3)
confint(lm.fit2)
