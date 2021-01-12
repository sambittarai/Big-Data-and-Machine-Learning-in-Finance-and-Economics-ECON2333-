rm(list=ls())
setwd = "G:/IIT_MADRAS_DD/Semesters/7th sem (UQ)/ECON2333 (Big Data and Machine learning in Finance and economics)/Tutorial_Sol/Tutorial_4"

#*******************4.6.1*******************#
library(ISLR)
names(Smarket)
View(Smarket)
dim(Smarket)
fix(Smarket)
pairs(Smarket)
cor(Smarket[,-9]) #The cor() function produces a matrix that contains all of the pairwise correlations among the predictors in a data set.

attach(Smarket)
plot(Volume) # Volume is increasing over time. In other words, the average number of shares traded daily increased from 2001 to 2005

#********************4.6.2**********************#
#Logistic Regression

# we will ???t a logistic regression model in order to predict Direction using
#Lag1 through Lag5 and Volume. The glm() function ???ts generalized linear models,
#a class of models that includes logistic regression. The syntax of the glm()
#function is similar to that of lm(), except that we must pass in the 
#argument family=binomial in order to tell R to run a logistic regression
#rather than some other type of generalized linear model

glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family=binomial)
glm.fits
summary(glm.fits)

coef(glm.fits)

#The predict() function can be used to predict the probability that the market 
#will go up, given values of the predictors. The type="response" option tells R
#to output probabilities of the form P(Y =1 |X).

#If no data is supplied to the predict() function  then the probabilities 
#are computed for the training data that was used to ???t the logistic regression
#model.  Here we have printed only the ???rst ten probabilities

glm.probs = predict(glm.fits, type="response")
glm.probs[1:10]
# these values correspond to the probability of the market going up, 
#rather than down, because the contrasts() function indicates that R has
#created a dummy variable with a 1 forUp.


contrasts(Direction)

#In order to make a prediction as to whether the market will go up or down on 
#a particular day, we must convert these predicted probabilities into class 
#labels, Up or Down.

glm.pred = rep("Down", 1250)
glm.pred[glm.probs>0.5] = "Up"

# table() function can be used to produce a confusion matrix in order to
#determine how many observations were correctly or incorrectly classi???ed.

table(glm.pred, Direction)

# The mean() function can be used to compute 
#the fraction of days for which the prediction was correct. 

mean(glm.pred==Direction) # In this case, logistic regression correctly predicted the movement of the market 52.2% ofthetime.

# this result is misleading because we trained and tested the model on the same set
# In order to better assess the accuracy of the logistic regression model in this setting, we can ???t
#the model using part of the data, and then examine how well it predicts 
#the held out data. 

# we will ???rst create a vector corresponding to the observations from 2001 
#through 2004. We will then use this vector to create a held out data set of 
#observations from 2005.

train = (Year<2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]

#We now ???t a logistic regression model using only the subset of the observations
#that correspond to dates before 2005, using the subset argument. We then obtain 
#predicted probabilities of the stock market going up for each of the days in our
#test set-that is, for the days in 2005. 

glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train)
glm.probs = predict(glm.fits, Smarket.2005, type="response")
glm.probs[1:10]

#Notice that we have trained and tested our 
#model on two completely separate data sets

glm.pred = rep("Down", 252)
glm.pred[glm.probs>.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005) #Test accuracy
mean(glm.pred!=Direction.2005) #Test error

#The results are rather disappointing: the test
#error rate is 52%, which is worse than random guessing

#Perhaps by removing the variables that appear not to be helpful in predicting Direction, we can obtain a more e???ective model.
#Below we have re???t the logistic regression using just Lag1 and Lag2.

glm.fits = glm(Direction~Lag1+Lag2, data=Smarket, family=binomial, subset=train)
glm.probs = predict(glm.fits, Smarket.2005, type="response")
glm.probs[1:10]
glm.pred = rep("Down", 252)
glm.pred[glm.probs>.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005) #Test Accuracy

#Suppose that we want to predict the returns associated with particular values
#of Lag1 and Lag2. In particular, we want to predict Direction on a day when 
#Lag1 and Lag2 equal 1.2 and 1.1, respectively, and on a day when they equal 
#1.5 and ???0.8. We do this using the predict() function. 

predict (glm.fits,newdata=data.frame(Lag1=c(1.2,1.5), Lag2=c(1.1,-0.8)),type="response") 

#******************4.6.5******************#
#KNN






#****************Exercise-2*******************#
rm(list=ls())
setwd = "G:/IIT_MADRAS_DD/Semesters/7th sem (UQ)/ECON2333 (Big Data and Machine learning in Finance and economics)/Tutorial_Sol/Tutorial_4"
LR1 = read.csv("G:/IIT_MADRAS_DD/Semesters/7th sem (UQ)/ECON2333 (Big Data and Machine learning in Finance and economics)/Tutorial_Sol/Tutorial_4/LR1.csv")
fix(LR1)
attach(LR1)

#start by programming a logistic function to use our p(x)
predp = function(x,b){
  p = 1/(1+exp(5-x*b)) #logistic function
  return(p)
}

#test
predp(2,1)

#Lets program the log likelihood function
loglik = function(b, y, x){
  n = length(y) #how big our data is
  ll = 0
  for (i in 1:n){
    ll = ll + log(predp(x[i],b))*(y[i]==1) + log(1-predp(x[i],b))*(y[i]==0)
  } #Summing up our p(x) functions for all data points
  return(ll)
}

#Lets look what is the best value of b 
#do a grid
G=1000 
bg = seq(-5, 5, length=G) #We think the best b will be between 5 and -5
#look b/w these 2 points

llstore = rep(0, G)
#this is what I'll store my loglikelihood values for different values for different b's that I pick
#Calculate the log likihood for all the b's; to a loop
for (g in 1:G){
  llstore[g] = loglik(bg[g], y, x)
}
plot(bg, llstore)

#Lets select the b with the highest log liklihood
bg[which.max(llstore)]
#Compare to the glm formula
glm(y ~ x, family = "binomial")
#Very similar



















