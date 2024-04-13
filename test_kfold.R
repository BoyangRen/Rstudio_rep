#Ref:https://www.youtube.com/watch?v=vTa74Ja8R8A&ab_channel=DataSciencewithYan


install.packages("ISLR2")
library(ISLR2)
data("Auto")


#1.training and testing set method.
n=length(Auto$mpg)
n
set.seed(1)
Z=sample(n,n/2)
training=Auto[Z,]
testing=Auto[-Z,]
model1=lm(mpg~weight+horsepower+acceleration,
          data=training)
summary(model1)
pred.test=predict(model1,testing)
plot(testing$mpg,pred.test)
mean((pred.test-testing$mpg)^2)

#2.leave one out cross validation
