A=1
a=2

library(ISLR)
?Auto


x = c(1,23,3,5)
y = c(1,2,3,4)
length(x)
x+y

ls() #Gives all the variables that we created in the working environment

rm(A) #Removes the variable "A" from the working environment
rm(list=ls()) #Removes all the variables

a = matrix(c(1,2,3,4),2,2) #Fills the matrix by column
a

b= matrix(c(1,2,3,4),2,2,byrow = TRUE) #Fills the matrix by row
b

a^2 #Elementwise mu;tiplication
a%*%a #Matrix multiplication
sqrt(a) #sqrt of matrix a

c = rnorm(10) #Generates random numbers from a standard normal distribution
d = rnorm(10,mean=2,sd=4) #Normal distribution with mean 2 and standard deviation 4

mean = mean(c)
var = var(c)
sd = sqrt(var(d))


x = rnorm(100)
y = rnorm(100)
plot(x,y,"l",xlab="x-axis",ylab="y-axis",main="Plot x vs y")


zz = seq(0,1,length=10) #Ten values b/w 0 and 1 with equispaced


