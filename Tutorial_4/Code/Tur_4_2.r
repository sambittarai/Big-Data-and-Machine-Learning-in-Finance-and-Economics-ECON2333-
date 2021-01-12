#**********Exercise 2**************#
#data
LR1 = read.csv("LR1.csv")
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
#y[1]
#y[1]==1

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