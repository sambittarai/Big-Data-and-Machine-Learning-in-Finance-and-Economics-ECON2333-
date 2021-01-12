#a

college = read.csv(file="College.csv", header=TRUE, sep=",")

#b

rownames(college)=college[,1]
fix(college)

college = college[,-1]
fix(college)

#c
summary(college)

#
pairs(college[,1:10])

#
plot(college$Private, college$outstate)

#
Elite = rep("No",nrow(college))
Elite[college$Top10perc>50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)

summary(Elite)

plot(Elite, college$Outstate)

#
par(mfrow=c(2,2))
hist(college$outstate, nclass=4)
hist(college$outstate, nclass=10)
hist(college$outstate, nclass=20)
hist(college$outstate, nclass=100)

#


