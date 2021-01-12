#************4.6.1**************#
rm(list=ls())

setwd("C:/Sambit/Tut_4")

library(ISLR)
#the data we will use is smarket
View(Smarket) #Up is success (1) , down is failure (0)
?Smarket
pairs(Smarket[,5:9], cex=0.6) #cex just makes the points small                      DBT

#**********4.6.2************#
# Fit a logistic regression bc we want to predict direction
#diretion is categorical (up and down)
#Predict the probability of up and place into category
attach(Smarket) #We can use the column names
glm.fits = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial) #General linear model
summary(glm.fits)
#Lets predict the Smarket going up
glm.probs = predict(glm.fits, type = "response") #type = "response" says give us the probabilities
#because we gave the predict function no new data it just predict for each of the X in our data set (fitted values)
contrasts(Direction)
#glm.probs[1]>.5

#Lets predict the categories that each prediction fits into
glm.pred = rep("Down", 1250)
glm.pred[glm.probs>.5] = "Up" #We have now classified each prediction of the prob into up or down classification
#head(glm.probs)
#head(glm.pred)

table(glm.pred, Direction) #Lets compare our prediction to actual data 

#Direction == glm.pred

#sum(Direction == glm.pred) #Total no of actual true predictions that we have predicted
mean(glm.pred == Direction) #Mean of total no of actual true predictions that we have predicted
#sum(glm.pred == Direction)/1250

#View(Smarket)
train = (Year<2005)
Smarket.2005 = Smarket[!train, ]
#View(Smarket.2005)
Direction.2005 = Direction[!train] #test data for direction

#%%%% Retrain the model on only the training set
glm.fits = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial, subset = train)
summary(glm.fits)
glm.probs = predict(glm.fits,Smarket.2005, type = "response") #Use the 2005 X variables(test data) tp predict Direction

glm.pred = rep("Down", 252)
glm.pred[glm.probs>.5] = "Up" #

table(glm.pred, Direction.2005) 
mean(glm.pred == Direction.2005) #Mean of total no of actual true predictions that we have predicted

# You can try changing the model and seeing how it performs
#See pg 160


#****************4.6.5********************#
#KNN

library(class) #a package with classification function
train.X = cbind(Lag1, Lag2)[train,]
#cbind is column bind; you can put 2 vectors together as the columns of the a matrix
test.X = cbind(Lag1, Lag2)[!train,]
train.Direction = Direction[train]
#Lets fit using KNN; K=3
knn.pred = knn(train.X, test.X, train.Direction, k=3)
