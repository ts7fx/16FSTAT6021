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
train.orig<-read_csv('team assignment 6/teamassign06train.csv')

#### data preprocessing:
# 1. identify categorical variables
# 2. convert such variables to factors
# looks like Age, Absence and Grade cannot be used as categorical variables. Thus convert everything other than these three to factors.
cols <- colnames(train)[-c(31,30)]
train[,cols]<- lapply(train[,cols], factor)
summary(train) #conversion successful

#### model fitting and selection:
# 1. forward, backward, and stepwise
lm.null <- lm(Grade~1, data = train)
lm.full <- lm(Grade~., data = train)

## Forward selection
step(lm.null, scope=list(lower=lm.null, upper=lm.full), direction="forward")
# lm(formula = Grade ~ failures + higher + school + sex + schoolsup + 
#      goout + internet + studytime + Fedu + famrel, data = train)
## Backward selection
step(lm.full, scope=list(lower=lm.null, upper=lm.full), direction="backward")
# lm(formula = Grade ~ school + sex + Fedu + studytime + failures + 
#      schoolsup + higher + internet + famrel + goout, data = train)
## Stepwise selection
step(lm.null, scope=list(lower=lm.null, upper=lm.full), direction="both")
# lm(formula = Grade ~ failures + higher + school + sex + schoolsup + 
#      goout + internet + studytime + Fedu + famrel, data = train)

# all the same. 
lm.init <- lm(Grade ~ failures + higher + school + sex + schoolsup + goout + internet + studytime + Fedu + famrel, data = train)
lm.init2 <- lm(Grade ~ failures + higher + sex + Medu + schoolsup + absences + reason + internet, data = nozero)
# transformation
lm.init3 <- lm(sqrt(Grade) ~ failures + higher + sex + Medu + schoolsup + absences + reason + internet, data = nozero)

summary(lm.init)
nozero$pred2<-(predict(lm.init3))^2


train$pred2<-predict(lm.init2, newdata = train)
train$pred <- NULL

temp <- train[,c(30,31,32)]
test$pred<-predict(lm.init,newdata=test)

nozero<-train[!train$Grade ==0,]
nozero$pred = NULL

# 2. shrinkage methods

#### testing using CV
# 1. test best model from forward, backward, stepwies
# 2. test shrinkage method models

#### write to output file
write.table(predvect, file="teamassign06preds.csv", row.names=F, col.names=F, sep=",")




########workspace############
plot(train.orig$goout, train.orig$Grade)
plot(train.orig$age, train.orig$Grade)
plot(train.orig$absences, train.orig$Grade)
plot(train.orig$Fedu, train.orig$Grade)

plot(train.orig$Medu, train.orig$Grade)
plot(train.orig$freetime, train.orig$Grade)

summary(influence.measures(lm.init))

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

plot(lm.init3$fitted.values,lm.init3$residuals)
qqnorm(resid(lm.init3))
qqplot(resid(lm.init3), lm.init3$fitted.values)
qqline(resid(lm.init3))


# testing transformations
library(MASS)
boxcox(lm.init2)
lm.init$fitted.values
?boxcox


# Once you have predicted the values of the response variable for the testing set,
# you should save them to a vector called predvect and write them into a .csv file 
# using the following code:
lm.null <- lm(Grade~1, data = nozero)
lm.full <- lm(Grade~., data = nozero)

## Forward selection
step(lm.null, scope=list(lower=lm.null, upper=lm.full), direction="forward")
# lm(formula = Grade ~ failures + higher + sex + Medu + schoolsup + absences + reason + internet, data = nozero)
## Backward selection
step(lm.full, scope=list(lower=lm.null, upper=lm.full), direction="backward")
# lm(formula = Grade ~ sex + Medu + reason + failures + schoolsup + higher + internet + absences, data = nozero)
## Stepwise selection
step(lm.null, scope=list(lower=lm.null, upper=lm.full), direction="both")
# lm(formula = Grade ~ failures + higher + sex + Medu + schoolsup + absences + reason + internet, data = nozero)

# all the same. 
lm.init <- lm(Grade ~ failures + higher + sex + Medu + schoolsup + absences + reason + internet, data = nozero)
summary(lm.init)

nozero$pred = predict(lm.init)


# Cross-fold Validation
library("DAAG")
cv.lm(data=nozero, form.lm=lm.init2, m=5, plotit=F)
cv.lm(data=nozero, form.lm=lm.init3, m=5, plotit=F)

cv.lm(data=train, form.lm=lm.init2, m=5, plotit=F)
cv.lm(data=train, form.lm=lm.init3, m=5, plotit=F)

cv.lm(data=train, form.lm=lm.init, m=5, plotit=F)
lm.init <- lm(Grade ~ failures + higher + school + sex + schoolsup + Fedu+ goout + internet + studytime +  famrel, data = train)
