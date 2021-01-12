setwd("")
LR2 = read.csv("LR2.csv")

#***************5.3.4*************#
#To bootstrap we cam use the package called boot and the function boot()
library(boot)
library(ISLR)
View(Portfolio)
data = Portfolio


#data$X #X variable from the portfolio dataset

#mysample = c(3,5,1)
#myindex = sample(1:3, 1, replace=T)

#myindex = sample(1:3, 3, replace=T) #index
#mysample[myindex]

#Create a function to calculate alpha that we put into the boot function
alpha.fn = function(data, index){
  X = data$X[index] #new bootstrap X variable
  Y = data$Y[index] # 
  return((var(Y) - cov(X,Y))/(var(X) + var(Y) - 2*cov(X,Y)))
  #calculate and return alpha
}

alpha.fn(data = Portfolio, 1:100) #Select the entire dataset
alpha.fn(data = Portfolio, sample(1:100, 100, replace = T)) #Let's sample from the original

#Let's bootstrap with the boot function
boot(Portfolio, alpha.fn, R = 100) #R is the number of replication

#Let's do one more example with linear regression
boot.fn = function(data, index){
  return(coef(lm(mpg ~ horsepower, data=Auto, subset = index))
}
boot.fn(Auto, 1:392)

#let's do manually for beta0
bs = rep(0, 1000)
for (i in 1:1000) {
  randomind = sample(1:392, 392, replace = T)
  bs = boot.fn(Auto, randomind)[1]
  
}
hist(bs, main = "Bootstraped B0 values")
sd(bs) #Estimated standard deviation of b0
#Let's compare to...
attach(Auto)
summary(lm(mpg ~ horsepower, sample = Auto)) # compared the se(b0)
# now let's get bootstrapped se with the boot function


#*****************Exercise 1***************#

LR2 = read.csv("LR2.csv")
attach(LR2)
plot(x,y)



#Copy and paste from tutorial4 the predp function and the loglik_logit function

predp = function(x,b)
{
  return( 1/(1+exp(b[1] + b[2]*x + b[3]*x^2)) )
}

# loglikelihood
loglik_logit2 = function(b, y, x)
{
  n = length(y)
  loglik = 0
  for (i in 1:n)
  {
    loglik = loglik + log(predp(x[i],b))*(y[i]==1) + log(1-predp(x[i],b))*(y[i]==0)
    
  }
  return(-loglik)
}

B = 100 #100 bootstrapped samples
b_boot = matrix(0, B, 3) #store 3 coefficients each time
n = 1000 #Length of our data

for (i in 1:B) {
  ind_ = sample(1:n, n, replace = T)
  xb = x[ind_] #new bootsrap sample
  yb = y[ind_]
  obj = optim(c(0,0,0), loglik_logit2, x=xb, y=yb)
  b_boot[i,] = obj$par
  
}

plot(b_boot[,1], type="l")
abline(h=-5, col=2, lwd=1.5)
sd(b_boot[,1])

plot(b_boot[,2], type="l")
abline(h=2, col=2, lwd=1.5)
sd(b_boot[,2])

plot(b_boot[,3], type="l")
abline(h=1, col=2, lwd=1.5)
sd(b_boot[,3])
























