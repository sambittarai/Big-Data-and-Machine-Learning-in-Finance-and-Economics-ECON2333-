#*******************3.6.2****************#

#rnorm generates a random value from the normal distribution. 
#runif generates a random value from the uniform distribution. rnorm(n, mean = , sd = ) is
#used to generate n normal random numbers with arguments mean and sd ;
#while runif(n, min = , max = ) is used to generate n uniform random numbers
#lie in the interval (min, max) .

setwd("G:/IIT_MADRAS_DD/Semesters/7th sem (UQ)/ECON2333 (Big Data and Machine learning in Finance and economics)/Tutorial_Sol/Tutorial_2")
library(MASS)
library(ISLR)

fix(Boston) #Boston Dataset
# medv - median house values
# rm - avg no. of rooms per house
# age - avg age of house
# lstat -  percent of households with low socioeconomic status
names(Boston)

#We will start by using the lm() function to ???t a simple linear regression
#model, with "medv" as the response and "lstat" as the predictor. 
#The basic syntax is lm(y???x,data)

attach(Boston)
lm.fit = lm(medv~lstat) #lm.fit = lm(medv~lstat,data=Boston)

lm.fit
#plot(medv,lstat)

summary(lm.fit)

names(lm.fit)
coef(lm.fit)

#In order to obtain a con???dence interval for the coe???cient estimates, we can use the confint() command
confint(lm.fit) #Confidence Interval

#The predict() function can be used to produce con???dence intervals and
#prediction intervals for the prediction of medv for a given value of lstat.

predict (lm.fit ,data.frame(lstat=c(5,10,15)), interval="confidence")
predict (lm.fit ,data.frame(lstat=c(5,10,15)), interval="prediction") 

plot(lstat,medv)
abline(lm.fit, col="Red", lwd=3)

par(mfrow=c(2,2))
plot(lm.fit)

plot(predict (lm.fit), residuals (lm.fit))
plot(hatvalues (lm.fit)) #On the basis of the residual plots, there is some evidence of non-linearity. Leverage statistics can be computed for any number of predictors using the hatvalues() function
which.max(hatvalues (lm.fit)) #The which.max() function identi???es the index of the largest element of a vector

#*******************3.6.3***************#
#Multiple Linear Regression 

lm.fit=lm(medv~lstat+age,data=Boston)
lm.fit

summary(lm.fit)

#The Boston data set contains 13 variables, and so it would be cumbersome 
#to have to type all of these in order to perform a regression using all of 
#the predictors. Instead, we can use the following short-hand: 
lm.fit=lm(medv~.,data=Boston) 
lm.fit
summary(lm.fit)
summary(lm.fit)$sigma

#The vif() can be used to compute variance inflation factor
library(car)
vif(lm.fit)

lm.fit1 = lm(medv~.-age, data=Boston) # The following syntax results in a regression using all predictors except age. 
summary(lm.fit1)

#Alternatively the update() function can be used to do the same operation, removing "age"
lm.fit1 = update(lm.fit, ~.-age)

#******************3.6.4*******************#
#Interaction Terms

#It is easy to include interaction terms in a linear 
#model using the lm() function. The syntax lstat:black 
#tells R to include an interaction term between lstat 
#and black. The syntaxlstat*age simultaneously includes
#lstat, age, and the interaction term lstat×age as 
#predictors; it is a shorthand for lstat+age+lstat:age.

lm(medv~lstat:age, data=Boston)
lm(medv~lstat*age, data=Boston) #lm(medv~lstat+age+lstat:age, data=Boston)

summary(lm(medv~lstat*age, data=Boston))

#****************3.6.5*******************#
#Non-linear transformation of the predictors

lm.fit2 = lm(medv~lstat+I(lstat^2))
lm.fit2

lm.fit = lm(medv~lstat)
anova(lm.fit, lm.fit2) #We use the anova() function to further quantify the extent to which the quadratic ???t is superior to the linear ???t. 

par(mfrow=c(2,2)) 
plot(lm.fit2)


lm.fit5=lm(medv~poly(lstat,5)) #produces a ???fth-order polynomial ???t
lm.fit5

#******************Exercise*****************#

#1
rm(list=ls())

n = 1000
a = 2.1
b = -0.9
x = runif(n)
y = b*x + rnorm(n, mean=a)

#2

lm.fit = lm(y~x)
lm.fit
