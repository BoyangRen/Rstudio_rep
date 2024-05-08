#he caret package (short for Classification And REgression Training)
install.packages("caret", dependencies = c("Depends", "Suggests"))
library(caret)
data(mtcars)
set.seed(42)
model_cv<-train(mpg ~hp,data=mtcars,method="lm",
                trContrl=trainControl(method="cv",
                                      number=10,
                                      verboseIter=T))
model_cv
summary(model_cv)

model_lm<-glm(mpg~hp, data=mtcars)
summary(model_lm)

















#https://www.youtube.com/watch?v=BQ1VAZ7jNYQ&ab_channel=DavidCaughlin

