#*********** Exercise - 2****************

# PART 1

x = read.csv("MC1.csv") #MC1 dataset
x = as.matrix(x) #It changes from a data frame to a matrix
str(x)
view(x)
N = 1000 #Sample size
b0 = -1 #Population intercept
b1 = 5.1
#Lets make our Y sample

y = b0 + b1*x +rnorm(N)
plot(x,y, pch = 20, col = "cyan4", cex = 0.9)
# pch changes the points
# cex changes the size of points

# PART 2

fit = lm(y~x)
summary(fit)
b0hat = fit$coefficients[1] #Just stores the estimate for b0
b1hat = fit$coefficients[2]
shat = sd(fit$residuals) # standard deviation of the residuals

# PART 3

M = 100 #Create 100 different samples
#We want to store the beta estimtes each time; so
b0m = rep(0,M)
b1m = rep(0,M)
  
#Lets loop over the 100 samples
  
for(i in 1:M){
  y = b0 + b1*x +rnorm(N) #different sample each loop
  fitm = lm(y ~ x)
  b0m[i] = fitm$coefficients[1] #Storing the coefficient of the estimates
  b1m[i] = fitm$coefficienta[2]
  sm[i] = sd(fitm$residuals)
}


# PART 4

mean(b0m)
mean(b1m)
mean(sm)

# PART 5

plot(b0m, type = "l")
abline(b0, 0, col = "red", lwd = "1.5")

# PART 6

X = matrix(c(rep(1,N), x), N, 2)
(shat^2)*solve(t(X)%*%X) #The variance of the beta estimates
#This is just the formula
# var(b) = sigma^2*(X'X)^-1
var(b0m)
var(b1m)
cov(b0m, b1m)