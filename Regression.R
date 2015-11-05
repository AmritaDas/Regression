rm(list = ls())

dataset <- read.csv("C:/Users/amrita/Desktop/FDS papers/Advertising.csv", header=TRUE)
dataset$X <- NULL
View(dataset)

#q4a

#Fit TV with Sales, plot TV with Sales, and add regression line from the model
TVmodel <- lm(dataset$Sales ~ dataset$TV)
summary(TVmodel)
plot(dataset$TV, dataset$Sales, col="red", xlab="TV", ylab="Sales")
abline(TVmodel, col="blue")
op <- par(mfrow=c(2,2))
plot(TVmodel)                #plot of TVmodel

#Fit Radio with Sales, plot Radio with Sales, and add regression line from the model
Radiomodel <- lm(dataset$Sales ~ dataset$Radio)
summary(Radiomodel)
op <- par(mfrow=c(1,1))
plot(dataset$Radio, dataset$Sales, col="red", xlab="Radio", ylab="Sales")
abline(Radiomodel, col="blue")
op <- par(mfrow=c(2,2))
plot(Radiomodel)            #plot of Radiomodel

#Fit Newspaper with Sales, plot Newspaper with Sales, and add regression line from the model
Newsmodel <- lm(dataset$Sales ~ dataset$Newspaper)
summary(Newsmodel)
op <- par(mfrow=c(1,1))
plot(dataset$Newspaper, dataset$Sales, col="red", xlab="News", ylab="Sales")
abline(Newsmodel, col="blue")
op <- par(mfrow=c(2,2))
plot(Newsmodel)             #plot of Newsmodel


#q4b

#Multiple Linear Regression
MLRmodel <- lm(dataset$Sales ~ dataset$TV + dataset$Radio + dataset$Newspaper)
summary(MLRmodel)
op <- par(mfrow=c(2,2))
plot(MLRmodel)              #plot of MLRmodel

#To get intercept and slope
coef(TVmodel)
coef(Radiomodel)
coef(Newsmodel)
coef(MLRmodel)


#q4c

Cdata <- read.table("C:/Users/amrita/Desktop/FDS papers/hw3-q4c.txt", header=TRUE)

View(Cdata)

#Logistic regression
#family is binomial for logit
LRmodel <- glm(Cdata$Y ~ Cdata$X1 + Cdata$X2 + Cdata$X3, family="binomial")
summary(LRmodel)
op <- par(mfrow=c(2,2))
plot(LRmodel)               #plot of LRmodel

fitted <- predict(LRmodel,type='response')  #get fitted values
fitted <- ifelse(fitted > 0.5,1,0)        #categorize fitted values into 0 and 1, divide is 0.5
accuracy <- mean(fitted == Cdata$Y)       #check accuracy
accuracy


#q5

fittednew <- predict(LRmodel,type='response')   #get fitted values
fittednew <- ifelse(fittednew > 0.3,1,0)        #categorize fitted values into 0 and 1, divide is 0.3
accuracynew <- mean(fittednew == Cdata$Y)       #check accuracy
accuracynew

#Logistic Regression with only two variables, X1 and X2
LRmodel12 <- glm(Cdata$Y ~ Cdata$X1 + Cdata$X2, family="binomial")

f12 <- predict(LRmodel12,type='response')   #get fitted values
f12 <- ifelse(f12 > 0.5,1,0)                #categorize fitted values into 0 and 1, divide is 0.5
accuracy12 <- mean(f12 == Cdata$Y)          #check accuracy
accuracy12

f12new <- predict(LRmodel12,type='response')  #get fitted values
f12new <- ifelse(f12new > 0.4,1,0)            #categorize fitted values into 0 and 1, divide is 0.4
accuracy12new <- mean(f12new == Cdata$Y)      #check accuracy
accuracy12new
