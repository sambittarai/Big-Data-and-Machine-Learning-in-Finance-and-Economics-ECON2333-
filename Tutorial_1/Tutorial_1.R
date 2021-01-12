#************ 2.3.1***********#


x = c(1,2,3,4)
length(x)
ls() #Shows us all the list of variables/ojects that we have so far
rm(list=ls()) #It can be used to delete any of the variables

?matrix #learn abbout the matrix function
x = matrix(c(1,2,3,4),2,2)#x = matrix(data=c(1,2,3,4),nrow=2,ncol=2)
y = matrix(c(1,2,3,4),2,2,byrow=TRUE) #fill the matrix row wise

x = x^2 #Element wise square  sqrt(x)

x = rnorm(10) #n=10 is the sample size, rnorm generates a vector of random normal variables with size n
# By default rnorm has 0 mean and unit variance

y = x+rnorm(10, 50,.1) #x+rnorm(10, mean = 50, sd = .1)
cor(x,y) #Correlation b/w x and y

#Sometimes we want our code to reproduce the exact same set of random numbers; we can use the set.seed() function to do this
set.seed(1303)
rnorm(50)

?set.seed()


#************* 2.3.2 ******************#
#GRAPHICS

x = rnorm(100)
y = rnorm(100)
plot(x,y, xlab="x-axis", ylab="y-axis", main="plot of x v/s y", col ="green")

x = seq(1,10,length=10) #  makes a sequence of 10 numbers that are equally spaced between 1 and 10. 
x = seq(-pi,pi,length=10)

#Contour plot
y = x
f=outer(x,y,function(x,y)cos(y)/(1+x^2))
contour(x,y,f) 
image(x,y,f) #Image() function works the same way as contour(), except it produces a heatmap
image(x,y,f,theta=30,phi=20) #theta and phi controls the angles at which the plot is viewed

#**************2.3.3************#
#Indexing the data

A = matrix(1:16,4,4)
A[2,3] = 11
A[c(2,3),c(1,4)] #2nd and 3rd row & 1st and 4th column
A[c(1:3),c(1:4)]
A[1:2,]
A[-c(1:3),]

#*************2.3.4*****************#
#Loading the data

rm(list=ls())

#Set working directory
setwd("G:/IIT_MADRAS_DD/Semesters/7th sem (UQ)/ECON2333 (Big Data and Machine learning in Finance and economics)/Tutorial_Sol/Tutorial_1")
library(ISLR)

#Auto = read.table("Auto.csv")
# Using the option header=T (or header=TRUE) in the read.table() function tells R that the 
#???rst line of the ???le contains the variable names, and using the option 
#na.strings tells R that any time it sees a particular character or set of 
#characters (such as a question mark), it should be treated as a missing element
#of the data matrix

Auto = read.csv("Auto.csv", header=T,na.string="?")
fix(Auto) #View the spreadsheet
dim(Auto) #Dimension of Auto
names(Auto) #Variable names


#**************2.3.5***************#
#Additional graphical summary

plot(Auto$mpg,Auto$cylinders)

#The attach function tells the R that make the variables in this data visible by there name
attach(Auto)
plot(mpg,displacement)


cylinders =as.factor(cylinders) #The as.factor() function converts quantitative variables into qualitative

plot(cylinders , mpg , col="red", varwidth =T, xlab="cylinders", ylab="MPG")

hist(mpg) 
hist(mpg ,col="green")
hist(mpg ,col="green",breaks =15)

pairs(Auto) #The pairs() function creates a scatterplot matrix i.e. a scatterplot for every pair of variables for any given data set
pairs(??? mpg + displacement + horsepower + weight + acceleration , Auto)

plot(horsepower ,mpg) 
identify (horsepower ,mpg ,name)

summary(Auto)



#************** Exercise 8 ***************#


setwd("G:/IIT_MADRAS_DD/Semesters/7th sem (UQ)/ECON2333 (Big Data and Machine learning in Finance and economics)/Tutorial_Sol/Tutorial_1")

#  a

college = read.csv("college.csv", header=T,na.string="?")
fix(college)

#   b

rownames(college)  = college[,1] #There is a new column name row.names, R will not perform any caliculation on this column
fix(college) 

college = college[,-1] #Eliminate the first column of the data where the names are stored
fix(college)
#Now you should see that the ???rst data column is Private. 
#Note that another column labeled row.names now appears before the Private 
#column. However, this is not a data column but rather the name thatR is 
#giving to each row. 

#   c
# (1)
summary(college)

# (2)
pairs(college[,1:10])

# (3)
attach(college)
plot(Outstate,Private) #plot(college$Outstate,college$Private)

# (4)
Elite = rep("No", nrow(college)) #Repeat the number of rows same as that of college with entries="No"
Elite[Top10perc>50] = "Yes" # Elite[college$Top10perc>50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college,Elite)

summary(Elite) #Number of Elite universities

plot(Elite, Outstate)

# (5)
par(mfrow=c(2,2)) 
hist(college$Outstate, nclass=4)
hist(college$Outstate, nclass=10)
hist(college$Outstate, nclass=20)
hist(college$Outstate, nclass=100)
