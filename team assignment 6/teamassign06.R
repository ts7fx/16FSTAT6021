### team assignment 6 ###
### Team 2     
#   Hampton Leonard, hll4ce
#   Tyler Worthington, tjw4ry
#   Andrew Pomykalski, ajp5sb
#   Tianye Song, ts7fx

#### setting things up
setwd("~/Documents/git/16FSTAT6021")
library(readr)
library(lars)
train<-read_csv('team assignment 6/teamassign06train.csv')
test<-read_csv('team assignment 6/teamassign06test.csv')
# check for missing values:
sum(is.na(train))
sum(is.na(test)) 
# no missing values for both data set. 

#### data preprocessing:
# 1. identify categorical variables
# 2. convert such variables to factors

#### model fitting and selection:
# 1. forward, backward, and stepwise
# 2. shrinkage methods

#### testing using CV
# 1. test best model from forward, backward, stepwies
# 2. test shrinkage method models

#### write to output file
write.table(predvect, file="teamassign06preds.csv", row.names=F, col.names=F, sep=",")




########workspace############

temp<-train
cols <- colnames(temp)[-c(3,31,30)]
temp[,cols]<- lapply(train[,cols], factor)
summary(temp)

sth <- multinom(Grade~school, data=train)


summary(sth)

lm.full <- lm(Grade~.,data = train)
lm.null <- lm(Grade~1, data = train)
step(lm.null, scope=list(lower=lm.null, upper=lm.full), direction="both")
step(lm.null, scope=list(lower=lm.null, upper=lm.full), direction="forward")
step(lm.full, scope=list(lower=lm.null, upper=lm.full), direction="backward")

lm.re <- lm(Grade ~ failures + higher + school + sex + schoolsup + goout + internet + studytime + Fedu + famrel, data = train)
train$pred <- predict(lm.re, newdata = train)
plot(residuals(lm.re))


train$testy <- log(train$Grade)

## trying lasso
lasso.predictors = as.matrix(train[,-31])
train$Grade <- as.numeric(train$Grade)
lasso.response = as.numeric(train$Grade)
lasso.fit = lars(lasso.predictors, lasso.response, type = "lasso")
coef(lasso.fit)
lasso.fit$lambda
##






# Once you have predicted the values of the response variable for the testing set,
# you should save them to a vector called predvect and write them into a .csv file 
# using the following code:


