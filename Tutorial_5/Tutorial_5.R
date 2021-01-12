#*********************Tutorial_5**********************#

rm(list=ls())
setwd = "G:/IIT_MADRAS_DD/Semesters/7th sem (UQ)/ECON2333 (Big Data and Machine learning in Finance and economics)/Tutorial_Sol/Tutorial_5"

# 4.6.3 Linear Discriminant Analysis

#We will perform LDA on the Smarket data. In R, we fit an LDA model using the lda() function, which is the part of the MASS library.
#We fit the model using only the observations before 2005.

library(MASS)
names(Smarket)
View(Smarket)
attach(Smarket)

train = (Year<2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]


lda.fit=lda(Direction ~ Lag1+Lag2 , data=Smarket, subset=train)
lda.fit
plot(lda.fit)


lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)
# "class" - contains LDA's predictions about the movement of the market.
# "posterior" - matrix whose kth column contains the posterior probability that the corresponding observation belongs to the kth class
# "x" - contains the linear discriminants

lda.class = lda.pred$class
table(lda.class, Direction.2005)

mean(lda.class==Direction.2005)

sum(lda.pred$posterior[,1] >= .5)
sum(lda.pred$posterior[,1] < .5)

lda.pred$posterior[1:20, 1]
lda.class[1:20]

#No days in 2005 meet the threshold! In fact the greatest posterior probability of decrease in all of 2005 was 50.02%
sum(lda.pred$posterior[,1] > .9)


#  4.6.4 Quadratic Discriminant Analysis

qda.fit = qda(Direction~Lag1 + Lag2, data = Smarket, subset=train)
qda.fit

qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)

mean(qda.class==Direction.2005)


#  4.6.5 K Nearest Neighbors

library(class)
train.X = cbind(Lag1, Lag2)[train,]
test.X = cbind(Lag1, Lag2)[!train,]
train.Direction = Direction[train]

set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)
#(83+43)/252 = 0.5
#The results using K=1 are not very good, since only 50% of the observations are correctly predicted.

#K=3


#Data Preprocessing (zero mean, unit variance)

train.standardized = scale(train[,-3]) #New training set
var(train.standardized[,1])
var(train.standardized[,2])

test.standardized = scale(test[,-3]) #New testing set
var(test.standardized[,1])
var(test.standardized[,2])


knn.pred = knn(train.standardized, test.standardized, train$y, k=1)
table(knn.pred, test$y)

