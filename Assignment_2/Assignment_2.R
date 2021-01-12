rm(list=ls())
setwd = "G:/IIT_MADRAS_DD/Semesters/7th sem (UQ)/ECON2333 (Big Data and Machine learning in Finance and economics)/Assignment_2"

data1 = read.csv('G:/IIT_MADRAS_DD/Semesters/7th sem (UQ)/ECON2333 (Big Data and Machine learning in Finance and economics)/Assignment_2/Assign2.csv')
View(data1)

attach(data1)

# 1
plot(x1,x2,'p',col='green')
points(x1[y==1],x2[y==1],col='black')

# 2

#Splitting the dataset into training and test set
#Training Set - 80% , Test Set - 20%

#Randomly splitting the dataset into train & test
indexes = sample(1:nrow(data1), size=0.2*nrow(data1))
test=data1[indexes,]
dim(test) # (200,3)
train=data1[-indexes,]
dim(train) # (800,3)

#Viewing the data
View(train)
View(test)

#Linear Model
lm.fit = lm(y~x1+x2, data=train)
lm.fit
#Predictions on the test set
prediction = predict(lm.fit, newdata = test, se.fit = FALSE, type = "response")

table(prediction>.51, test$y)


mean(prediction)

# 3

# (a) Logistic Regression

glm.fit = glm(y ~ x1+x2, data=train, family=binomial)
glm.fit

prediction1 = predict(glm.fit, newdata = test, se.fit = FALSE, type = 'response')
table(prediction1>.51, test$y)


# (b) Linear Discriminant Analysis
lda.fit = lda(y~x1+x2, data=train)
lda.fit

prediction2 = predict(lda.fit, newdata = test, se.fit = FALSE, type = 'response')
names(prediction2)

table(prediction2$class, test$y)

# (c) Quadratic Discriminant Analysis
qda.fit = qda(y~x1 + x2, data = train)
qda.fit
prediction3 = predict(qda.fit, newdata = test, se.fit = FALSE, type = 'response')
table(prediction3$class, test$y)

# 4 KNN

#Normalizing the data (zero mean and unit variance)
train.X = cbind(train$x1, train$x2)
train.Y = train$y
test.X = cbind(test$x1, test$x2)
test.Y = test$y
train.X = scale(train.X) 
test.X = scale(test.X)
var(train.X[,1]) #Now the variance is 1

a = matrix(0,20,200)
b = rep(1,20)

for (i in 1:20) {
  a[i,] = knn(train.X, test.X, train.Y, k=i)
  #b[i] = table(a[i,], test.Y)
  
}
a=a-1
table(a[20,], test.Y)

#Prediction
knn.pred = knn(train.X, test.X, train.Y, k=20)
knn.pred
table(knn.pred, test.Y)

# k=1
knn.pred1 = knn(train.X, test.X, train.Y, k=1)
knn.pred1
table1 = table(knn.pred1, test.Y)
table1

# k=2
knn.pred2 = knn(train.X, test.X, train.Y, k=2)
knn.pred2
table2 = table(knn.pred2, test.Y)
table2

#k=3
knn.pred3 = knn(train.X, test.X, train.Y, k=3)
knn.pred3
table3 = table(knn.pred3, test.Y)
table3

#k=4
knn.pred4 = knn(train.X, test.X, train.Y, k=4)
knn.pred4
table4 = table(knn.pred4, test.Y)
table4

#k=5
knn.pred5 = knn(train.X, test.X, train.Y, k=5)
knn.pred5
table5 = table(knn.pred5, test.Y)
table5

#k=6
knn.pred6 = knn(train.X, test.X, train.Y, k=6)
knn.pred6
table6 = table(knn.pred6, test.Y)
table6

#k=7
knn.pred7 = knn(train.X, test.X, train.Y, k=7)
knn.pred7
table7 = table(knn.pred7, test.Y)
table7

#k=8
knn.pred8 = knn(train.X, test.X, train.Y, k=8)
knn.pred8
table8 = table(knn.pred8, test.Y)
table8

#k=9
knn.pred9 = knn(train.X, test.X, train.Y, k=9)
knn.pred9
table9 = table(knn.pred9, test.Y)
table9

#k=10
knn.pred10 = knn(train.X, test.X, train.Y, k=10)
knn.pred10
table10 = table(knn.pred10, test.Y)
table10

#k=11
knn.pred11 = knn(train.X, test.X, train.Y, k=11)
knn.pred11
table11 = table(knn.pred11, test.Y)
table11

#k=12
knn.pred12 = knn(train.X, test.X, train.Y, k=12)
knn.pred12
table12 = table(knn.pred12, test.Y)
table12

#k=13
knn.pred13 = knn(train.X, test.X, train.Y, k=13)
knn.pred13
table13 = table(knn.pred13, test.Y)
table13

#k=14
knn.pred14 = knn(train.X, test.X, train.Y, k=14)
knn.pred14
table14 = table(knn.pred14, test.Y)
table14

#k=15
knn.pred15 = knn(train.X, test.X, train.Y, k=15)
knn.pred15
table15 = table(knn.pred15, test.Y)
table15

#k=16
knn.pred16 = knn(train.X, test.X, train.Y, k=16)
knn.pred16
table16 = table(knn.pred16, test.Y)
table16

#k=17
knn.pred17 = knn(train.X, test.X, train.Y, k=17)
knn.pred17
table17 = table(knn.pred17, test.Y)
table17

#k=18
knn.pred18 = knn(train.X, test.X, train.Y, k=18)
knn.pred18
table18 = table(knn.pred18, test.Y)
table18

#k=19
knn.pred19 = knn(train.X, test.X, train.Y, k=19)
knn.pred19
table19 = table(knn.pred19, test.Y)
table19

#k=20
knn.pred20 = knn(train.X, test.X, train.Y, k=20)
knn.pred20
table20 = table(knn.pred20, test.Y)
table20

# 5
library(boot)

#Linear Model
lm.fit1 = glm(y~x1+x2, data=train)

cv.error.lm = cv.glm(train, lm.fit1, K=10)
cv.error.lm$delta[1]
cv.error.lm$delta[2]


#Logistic Model
glm.fit1 = glm(y~x1+x2, data=train, family=binomial)

cv.error.glm = cv.glm(train, glm.fit1, K=10)
cv.error.glm$delta[1]
cv.error.glm$delta[2]

#KNN
knn.cv = knn.cv(data = train.X, label = train.Y, k=7, p=10, method="classification")
names(knn.cv)









