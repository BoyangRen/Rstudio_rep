#Ref:https://www.youtube.com/watch?v=vTa74Ja8R8A&ab_channel=DataSciencewithYan


install.packages("ISLR2")
library(ISLR2)
data("Auto")


#1.training and testing set method.###################
n=length(Auto$mpg)
n
set.seed(1)
Z=sample(n,n/2)
training=Auto[Z,]
testing=Auto[-Z,]
model1=glm(mpg~weight+horsepower+acceleration,
          data=training)
summary(model1)
pred.test=predict(model1,testing)
plot(testing$mpg,pred.test)
mean((pred.test-testing$mpg)^2)

#2.leave one out cross validation######################
install.packages("boot")
library(boot)

##1)Full set glm model. FULL SET! NOT TRAINING SET#####
  model1_glm_full<-glm(mpg~weight+horsepower+acceleration,
                      data=Auto)
##2)cross validation
  cv.err=cv.glm(data=Auto,model1_glm_full)
  #folds, default is sample size
  cv.err$K
  #raw error and adjusted error of cross-validation####
  cv.err$delta

#3.k-fold cross validation#############################
#k-fold means data was randomly split into k-fold.
set.seed(1)
cv.err.10=cv.glm(data=Auto, model1_glm_full,K=10)
cv.err.10
cv.err.10$delta

#Test without local save


