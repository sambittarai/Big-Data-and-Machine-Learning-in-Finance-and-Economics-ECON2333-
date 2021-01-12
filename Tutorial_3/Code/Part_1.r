#Set the working directory
#Change bck slash to forward slash, if needed


setwd("H:/Tut3")

library(ISLR)
View(Carseats)
summary(Carseats$ShelveLoc)  #3 categories
str(Carseats$ShelveLoc) #is a factor (Categorical variable)

attach(Carseats)
str(ShelveLoc)

#Lets regress sales on all other variables plus some interaction terms
lm.fit = lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)
contrasts(ShelveLoc) #Tells us how the factor is converted to dummy variables


# ******* WRITING FUNCTION / ALSO LOOPS **********

#Lets make a function that does y=x^2 for us

myxsquared = function(x){
  y = x^2
  print("The Calculation is done...  the result is ...")
  return(y)
}

myxsquared(99)


#Lets make a function that loads libraries for us
loadlibraries = function(){
  library(ISLR)
  library(MASS)
  print("The libraries ISLR and MASS are now loaded")
  
}

loadlibraries()

#Lets do a loop

mydata = c(10, 15, 8, 2, 1, 2)
N = length(mydata)
myoutput = rep(0, N) #rep - repeat

for (i in 1:N){
  myoutput[i] = mydata[i]^2
}
