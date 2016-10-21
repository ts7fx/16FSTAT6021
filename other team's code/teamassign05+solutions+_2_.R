# STAT6021 - Team Assignment 5
# 10/5/2016
# Team 8
# Jordan Baker (jmb4ax), C.J Wheeler (cjw5fn), Muyang Sun (ms2vc), Leo Cao (lc2ub)

## Please submit one set of answers per team.                                  ##
## Your answers should be submitted as a .csv file per the instructions below. ##
## You should also submit your annotated R code per the instructions below.    ##
#################################################################################


# For this team assignment you will use the file "teamassign05train.csv" to develop
# a linear model using whatever methods you consider appropriate. You will then use
# the model that you have developed to predict the values of the response variable
# corresponding to the explanatory variable values given in the file
# "teamassign05test.csv". 
#
# Once you have predicted the values of the response variable for the testing set,
# you should save them to a vector called predvect and write them into a .csv file 
# using the following code:

setwd("C:/Users/Jordan/Documents/School/University of Virginia/Fall 2016/STAT6021 - Linear Models for Data Science/Team Assignments/Team Assignment 5")

library(readr)
library(car)
library(MASS)

training <- read_csv("teamassign05train.csv")
test <- read_csv("teamassign05test.csv")

# plot all variables
plot(training$x1, training$y)
plot(training$x2, training$y)
plot(training$x3, training$y)
plot(training$x4, training$y)
plot(training$x5, training$y)
plot(training$x6, training$y)
plot(training$x7, training$y)

# define the models with no variables (null) and all variables (full)
model.null <- lm(y~1, data=training)
model.full <- lm(y~., data=training)

# model selection
# forward, backward, and stepwise
step(model.null, scope=list(lower=model.null, upper=model.full), direction="forward")
step(model.null, scope=list(lower=model.null, upper=model.full), direction="backward")
step(model.null, scope=list(lower=model.null, upper=model.full), direction="both")

# turn certain variables into factors
training$x1 <- as.factor(training$x1)
training$x2 <- as.factor(training$x2)
training$x4 <- as.factor(training$x4)
training$x5 <- as.factor(training$x5)
training$x6 <- as.factor(training$x6)

# run the model selection steps on the model with factors
step(model.null, scope=list(lower=model.null, upper=model.full), direction="forward")
step(model.full, scope=list(lower=model.null, upper=model.full), direction="backward")
step(model.null, scope=list(lower=model.null, upper=model.full), direction="both")

# create the model using x1 and x6
# check training MSE
model1 <- lm(y~x1+x6, data=training)
summary(model1)
mean(model1$residuals^2) # 3.034873

# ensure no transformations are needed
boxcox(model1)

# turn x1 and x6 from the test set into factors
test$x1 <- as.factor(test$x1)
test$x6 <- as.factor(test$x6)

# create output file
predvect <- predict(model1, test)
write.table(predvect, file="teamassign05preds.csv", row.names=F, col.names=F, sep=",")






