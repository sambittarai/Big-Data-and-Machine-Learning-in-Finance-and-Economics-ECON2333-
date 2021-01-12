#rm(list=ls())

Auto = read.csv("Auto.csv", header=T,na.string="?")
fix(Auto)
dim(Auto)
Auto[1:4,]
Auto = na.omit(Auto)
dim(Auto)

names(Auto)


#plot(Auto$cylinder,Auto$mpg)

attach(Auto)
#plot(cylinders,mpg,)

cylinders = as.factor(cylinders)
plot(cylinders,mpg)

hist(mpg) #histogram for "mpg"

hist(mpg, breaks=15)
pairs(Auto)
