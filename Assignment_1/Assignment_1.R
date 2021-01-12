#*******************Assignment-1*********************#

rm(list=ls())
setwd = "G:/IIT_MADRAS_DD/Semesters/7th sem (UQ)/ECON2333 (Big Data and Machine learning in Finance and economics)/Assignment_1"

# Exercise 1

# Y = exp(Beta)*X + epsilon
Beta = 0.6
n = 1000

X = rnorm(n)
epsilon = rnorm(n)

# (1) 
Y = exp(Beta)*X + epsilon
summary(Y)

# (2)

# (a) (b) (c)

lm1.fit = lm(Y~X) #Linear Regression
lm1.fit

par(mfrow=c(2,2)) 
plot(lm1.fit)

lm2.fit = lm(Y~poly(X,2)) #Quadratic Regression
lm2.fit
par(mfrow=c(2,2)) 
plot(lm2.fit)

lm3.fit = lm(Y~poly(X,3)) #Cubic Regression
lm3.fit
par(mfrow=c(2,2)) 
plot(lm3.fit)

# (d)

plot(X,Y, xlab = "X-values", ylab = "Y-values")
abline(lm1.fit, col="Red", lwd=1)
lines(X, predict(lm2.fit), col="blue", lwd=2)
lines(X, predict(lm3.fit), col="green", lwd=3)



# (3)
#Let say that the function takes the form of, f = exp(beta)*x

predp = function(beta,x){
  f = exp(beta)*x #f is the estimate for y
  return(f)
}

#test
predp(2,3)

b = seq(-1.5,1.5, length=100)

#function for sum of squared errors (se)
se = function(y,b,x){
  Q1 = rep(0,1000)
  Q=0
  for (i in 1:1000) {
    Q1[i] = (y[i] - predp(b,x[i]))^2
    Q = sum(Q1)
  }
  return(Q)
}
Q_b = rep(0,100)

for (i in 1:100) {
  Q_b[i] = se(Y,b[i],X)
  
}
# (a)
plot(b,Q_b)



# (b)
beta = b[which.min(Q_b)] #Optimum value of beta

#  (c)
#Plot Predicted  values v/s Actual data
predicted_values = predp(beta,X)
#length(predicted_values)

plot(X,Y)
abline(X,predicted_values, col="blue", lwd=2)


# (d)
lm.fit = lm(Y~X)
lm.fit
plot(X,Y)
abline(lm.fit, col="red", lwd=2)

# (4)
# (a)

#actual model
# Y = exp(b)*x

#MSE() is the function which calculates mean squared error
MSE = function(y,f){
  err = rep(0,1000)
  for (i in 1:1000) {
    err[i] = ((Y[i] - f[i])^2)
    
  }
  mse = sum(err)/1000
  return(mse)
}

actual.fit = predp(beta,X);
plot(X,Y)
abline(actual.fit,X, col="red", lwd=2) #plotting the actual model

mse = MSE(Y,actual.fit);


#linear Model
# Y = a1*X + b1
lm1.fit
b1 = lm1.fit$coefficients[1]
a1 = lm1.fit$coefficients[2]

linearModel = function(a1,b1,X1){
  f = a1*X1 + b1
  return(f)
}
  

yhat1 = rep(0,1000)

yhat1 = linearModel(a1,b1,X)

mse1 = MSE(Y,yhat1) 

#Quadratic Model
# Y = a2*X^2 + b2*X + c2
lm2.fit
c2 = lm2.fit$coefficients[1]
b2 = lm2.fit$coefficients[2]
a2 = lm2.fit$coefficients[3]


QuadModel = function(a2,b2,c2,X2){
  
  f = a2*(X2^2) + b2*X2 + c2
  
  return(f)
}

yhat2 = rep(0,1000)

yhat2 = QuadModel(a2,b2,c2,X)

mse2 = MSE(Y,yhat2)/1000

#Cubic Model
# Y = a1*X^3 + b1*X^2 + c1*X + d
lm3.fit
d3 = lm3.fit$coefficients[1]
c3 = lm3.fit$coefficients[2]
b3 = lm3.fit$coefficients[3]
a3 = lm3.fit$coefficients[4]


cubicModel = function(a3,b3,c3,d3,X3){
  f = a3*(X3^3) + b3*(X3^2) + c3*X3 + d3
  return(f)
}


yhat3 = rep(0,1000)

yhat3 = cubicModel(a3,b3,c3,d3,X)

mse3 = MSE(Y,yhat3)/1000

# (b) 

#Generate 100 new samples, lets call that X_test and Y_test

test_values = 100
X1 = rnorm(test_values)
epsilon1 = rnorm(test_values)

Y1 = exp(Beta)*X1 + epsilon1

MSE_t = function(y,f){
  err = rep(0,100)
  for (i in 1:100) {
    err[i] = ((Y[i] - f[i])^2)
    
  }
  mse = sum(err)/100
  return(mse)
}

#Model_1
#y = exp(b)*x
y1_test = predp(beta,X1);
test_mse = MSE_t(Y1, y1_test); #MSE for model y = exp(b)*x

#Model_2
#y = a*x*x + b*x + c
y2_test = linearModel(a1,b1,X1)
test_mse1 = MSE_t(Y1, y2_test); #MSE for model y = a*x + b

#Model_3
#y = a*x*x + b*x + c
y3_test = QuadModel(a2,b2,c2,X1)
test_mse2 = MSE_t(Y1, y3_test)/100 #MSE for model y = a*x*x + b*x + c

#Model_4
#y = a*x*x*x + b*x*x + c*x + d
y4_test = cubicModel(a3,b3,c3,d3,X1)
test_mse3 = MSE_t(Y1, y4_test)/100 #MSE for model y = a*x*x*x + b*x*x + c*x*x + d



#***************Exercise-2********************#
#KNN classificiation

X1 = c(1,1,-3,2,3,4,4)
X2 = c(2,3,1,2,2,1,3)
Y = c(0,0,0,1,1,1,1)

predicty = function(a,b){
  d = rep(0,7) #Distance
  for (i in 1:7) {
    X1hat = (X1[i] - a)^2
    X2hat = (X2[i] - b)^2
    d[i] = (X1hat + X2hat)^0.5
  }
  
  y = Y[which.min(d)]

  return(y)
}

#Testing on 3 random points
predicty(490,3)
predicty(4,398)
predicty(-400000000000000,-9999999999993)




