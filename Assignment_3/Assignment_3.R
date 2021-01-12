rm(list=ls())
setwd = "G:/IIT_MADRAS_DD/Semesters/7th sem (UQ)/ECON2333 (Big Data and Machine learning in Finance and economics)/Assignment_3/"

#Reading the data
data1 = read.csv('G:/IIT_MADRAS_DD/Semesters/7th sem (UQ)/ECON2333 (Big Data and Machine learning in Finance and economics)/Assignment_3/Assign3.csv')
View(data1)
attach(data1)

# 1 Linear Regression
#Actual Dataset visualization
lm.fit = lm(y~x, data=data1)
lm.fit
z = predict(lm.fit, data=data1)
#Scatter plot visualization
scatter.smooth(x=x,y=y, col="blue", pch=19, main="Linear Regression")

#Fitting a linear regression model
abline(lm.fit, col="red", lwd=2)

# 2 Quadratic Regression
qm.fit = lm(y~poly(x,2), data=data1)
qm.fit
z = predict(qm.fit, data=data1)
#Scatter plot visualization
scatter.smooth(x=x,y=y, col="green", lwd=2)
#Fitting a linear regression model
#plot(x,y)
points(x, z, col="red", lwd=2)

# 3 a)
#Sorting the data
set.seed(1)
data2 = data1[order(data1$x),]
#Selecting a random uniform number
U = runif(1, min(data1$x), max(data1$x))
#Selecting the cut points
v = c(min(data1$x), U, max(data1$x))
#Fitting the step function
step_fit = lm(data2$y ~ cut(data2$x,v))
#Predictions
preds = predict(step_fit, data2)
length(preds)
#Plots
scatter.smooth(data2$x,data2$y, col="green", lwd=2)
lines(data2$x, preds, col="red", lwd=2)

# 3 b)
step = function(input, data){
  #This function takes as input 2 arguments i.e. 'input' and 'data'.
  
  #Input Arguments
  # data - It is the data to which we are trying to fit the step function model.
  # input - It is either a scaler/vector of values for which the function will return the predicted values.
  
  #Output
  #This function returns a scaler/vector of predicted values for the given inputs
  
  #Sorting the data
  data1 = data[order(data$x),]
  
  #Selecting 1 random uniform number
  U = runif(1, min(data$x), max(data$x))
  #Defining 3 cut points
  v = c(min(data$x), U, max(data$x))
  #Fitting the step function
  step_fit = lm(data1$y ~ cut(data1$x,v))
  preds = predict(step_fit, data1)
  unique_value = unique(preds)
  #Initialize the output
  output = matrix(0,1,length(input))
  for (i in 1:length(input)) {
    if(input[i]<=U){
      output[i] = unique_value[2]
    }
    if(input[i]>U){
      output[i] = unique_value[3]
    }
    
  }
  return(output)
}

# 3 c)
input1 = c(-2,-3,-0.7,-0.1,0.4,0.9, 2,3)
input2 = c(-0.1,-0.2,-0.3,-0.4,-0.5,-0.6)
input3 = c(-0.98,-0.81,-0.5,-0.4,-0.3,0.3,0.4,0.5,0.81,0.98)
output1 = step(input1, data1)
output2 = step(input2, data1)
output3 = step(input3, data1)

scatter.smooth(data2$x,data2$y, col="green", lwd=2)
lines(input1,output1, col="red", lwd=3, type='l')
scatter.smooth(data2$x,data2$y, col="green", lwd=2)
lines(input2,output2, col="red", lwd=3, type='l')
scatter.smooth(data2$x,data2$y, col="green", lwd=2)
lines(input3,output3, col="red", lwd=3, type='l')

View(data1)
