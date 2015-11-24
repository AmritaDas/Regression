library(ISLR)
attach(Default)

#(a) logistic regression model using income and balance to predict default
IBfit = glm(default ~ income + balance, data = Default, family = binomial)

#(b) estimating test error using validation set approach
tData <- function(x){
  #i. training data
  IBtrain = sample(dim(Default)[1], dim(Default)[1]/x)
  #ii. logistic regression model using income, balance, and training data to predict default
  IB = glm(default ~ income + balance, data = Default, family = binomial, subset = IBtrain)
  #iii. prediction on test data
  IBtest = rep("No", dim(Default)[1] * (x-1)/x)
  IBprob = predict(IB, Default[-IBtrain, ], type = "response")
  IBtest[IBprob > 0.5] = "Yes"
  #iv. error rate
  return(mean(IBtest != Default[-IBtrain, ]$default))
}
tData(4)

#(c)
tData(8)
tData(10)
tData(25)

#(d) logistic regression model using income, balance, dummy student variable, and training data to predict default
IBtrain = sample(dim(Default)[1], dim(Default)[1]/4)
IB = glm(default ~ income + balance + student, data = Default, family = binomial, subset = IBtrain)
IBtest = rep("No", dim(Default)[1] * 3/4)
IBprob = predict(IB, Default[-IBtrain, ], type = "response")
IBtest[IBprob > 0.5] = "Yes"
mean(IBtest != Default[-IBtrain, ]$default)

