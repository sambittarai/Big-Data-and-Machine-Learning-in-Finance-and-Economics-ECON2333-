library(MASS)
fix(Boston)
attach(Boston)
lm(medv~lstat)
lm.fit=lm(medv~lstat)
summary(lm.fit)
names(lm.fit)
lm.fit$coefficients
confint(lm.fit)
lm.fit$residuals
predict(lm.fit,data.frame(lstat=c(1,5,10)),interval = "confidence")
predict(lm.fit,data.frame(lstat=c(1,5,10)),interval = "prediction")
predict(lm.fit)
plot(lstat,medv)
abline(lm.fit,lwd=3,col="red")
plot(predict(lm.fit), residuals(lm.fit))
par(mfrow=c(2,2)) #Divide the graph space into 4 parts
plot(lm.fit)

#Multiple regression
lm(medv~lstat+age)
summary(lm(medv~lstat+age))
lm(medv~., data=Boston) #Include all the vaariables
summary(lm(medv~.,data=Boston))
lm(medv~. -age,data=Boston) #Include all variables except the variables "age"
library(car)
lm.fit1 = lm(medv~., data=Boston)
vif(lm.fit1)
plot(predict(lm.fit1),residuals(lm.fit1))
lm(medv~lstat*age) # Equivalent to lm(medv~lstat+age+lstat:age)
lm(medv~lstat:age)
lm(medv~lstat+I(lstat^2))
par(mfrow=c(2,2))
plot(lm(medv~lstat+I(lstat^2)))
lm(medv~poly(lstat,5))
