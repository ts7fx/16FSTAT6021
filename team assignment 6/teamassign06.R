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
library(MASS)
library(DAAG)
library(glmnet)
train<-read_csv('team assignment 6/teamassign06train.csv')
test<-read_csv('team assignment 6/teamassign06test.csv')
# check for missing values:
sum(is.na(train))
sum(is.na(test)) 
# no missing values for both data set. 
#### data preprocessing:
# 1. identify categorical variables
# 2. convert such variables to factors
# looks like Age, Absence and Grade cannot be used as categorical variables. Thus convert everything other than these three to factors.
plot(train$age, train$Grade)
# looks like age is categorical as well.
cols <- colnames(train)[-c(31,30)] # thus, only exclude "absences" & "grades" from the set of factors
train[,cols]<- lapply(train[,cols], factor)
# create a nozero dataset for log transformation of Grade
nozero<-train[!train$Grade == 0,]
# create a dataset in which every grade is added 0.01 for log transformation
train.incre <- train
train.incre$Grade <- train.incre$Grade+0.01

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
lm.1 <- lm(Grade ~ failures + higher + school + sex + schoolsup + goout + internet + studytime + Fedu + famrel, data = train)
## look for influence points: 
summary(influence.measures(lm.1)) # cutoff value is 0.642
##
# 2. consider transformation
# first test to see if any transformation is needed
boxcox(lm.1) # <---- won't run because Grade has value of 0 (non-positive). 
# use nozero dataset
# re-run forward, backward, and stepwise subset selection
lm.full <- lm(Grade~.,data = nozero)
lm.null <- lm(Grade~1, data = nozero)
step(lm.null, scope=list(lower=lm.null, upper=lm.full), direction="both")
step(lm.null, scope=list(lower=lm.null, upper=lm.full), direction="forward")
step(lm.full, scope=list(lower=lm.null, upper=lm.full), direction="backward")
# all three yield same results.
lm.2 <- lm(Grade ~ failures + higher + sex + Medu + schoolsup + absences + reason + internet, data = nozero)
# look at influence points
summary(influence.measures(lm.2)) # cutoff value is 0.642
# apparently removing zero values and refitting the data drastically reduced number of influence points.

boxcox(lm.2) # graph suggests us to use yhat = sqrt(y) transformation
# transformation
lm.2.trans <- lm(sqrt(Grade) ~ failures + higher + sex + Medu + schoolsup + absences + reason + internet, data = nozero)
summary(influence.measures(lm.2.trans)) # cutoff value is 0.642

# using incremented dataset and try log transformation to see difference in CV results
# 1. forward, backward, and stepwise
train.incre$pred<-NULL
summary(lm.incre)
lm.null <- lm(Grade~1, data = train.incre)
lm.full <- lm(Grade~., data = train.incre)
## Forward selection
step(lm.null, scope=list(lower=lm.null, upper=lm.full), direction="forward")
# lm(formula = Grade ~ failures + higher + school + sex + schoolsup + 
#      goout + internet + studytime + Fedu + famrel, data = train.incre)
## Backward selection
step(lm.full, scope=list(lower=lm.null, upper=lm.full), direction="backward")
# lm(formula = Grade ~ school + sex + Fedu + studytime + failures + 
#      schoolsup + higher + internet + famrel + goout, data = train.incre)
## Stepwise selection
step(lm.null, scope=list(lower=lm.null, upper=lm.full), direction="both")
# lm(formula = Grade ~ failures + higher + school + sex + schoolsup + 
#      goout + internet + studytime + Fedu + famrel, data = train.incre)
# all the same. 
lm.incre<- lm(Grade ~ failures + higher + school + sex + schoolsup + goout + internet + studytime + Fedu + famrel, data = train.incre)
boxcox(lm.incre) # no tranformation necessary

# 2. regularization won't make sense because partial selection of categorical variable value will not make sense. 

#### testing using CV
# 1. test best model from forward, backward, stepwies
# 2. test shrinkage method models
# Cross-fold Validation
cv.lm(data=nozero, form.lm=lm.2, m=5, plotit=F)
cv.lm(data=nozero, form.lm=lm.2.trans, m=5, plotit=F)

cv.lm(data=train.incre, form.lm=lm.2, m=5, plotit=F)
cv.lm(data=train, form.lm=lm.2.trans, m=5, plotit=F)

cv.lm(data=train, form.lm=lm.1, m=5, plotit=F) # won't run because there's only one instance of Fedu==0 in the dataset. This is because in order to successfully run CV, you need at least two occurances of the same level of a factor.

# try to duplicate Fedu==0 observation 
train.temp <- rbind(train, train[train$Fedu==0,])
lm.1.modified <- lm(Grade ~ failures + higher + school + sex + schoolsup + goout + internet + studytime + Fedu + famrel, data = train.temp)
# again, try running CV
cv.lm(data=train.temp, form.lm=lm.1.modified, m=5, plotit=F)
cv.lm(data=train.incre, form.lm=lm.incre, m=5, plotit=F)
cv.lm(data=nozero, form.lm=lm.incre, m=5, plotit=F)

#### write to output file
write.table(predvect, file="teamassign06preds.csv", row.names=F, col.names=F, sep=",")




########workspace############
plot(train.orig$goout, train.orig$Grade)
plot(train$age, train$Grade)
plot(train.orig$absences, train.orig$Grade)
plot(train.orig$Fedu, train.orig$Grade)

plot(train.orig$Medu, train.orig$Grade)
plot(train.orig$freetime, train.orig$Grade)
plot(nozero$Grade, lm.2$fitted.values)

lm.incre<- lm(Grade ~ failures + higher + school + sex + schoolsup + goout + internet + studytime + famrel, data = train.incre)

summary(temp)








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


# resid for lm.incre
plot(lm.incre$fitted.values,lm.incre$residuals)
qqnorm(resid(lm.incre))
qqline(resid(lm.incre))

# resid for lm.incre
plot(lm.2$fitted.values,lm.2$residuals)
qqnorm(resid(lm.2))
qqline(resid(lm.2))


train.incre$pred<-predict(lm.incre, newdata=train.incre)


# Once you have predicted the values of the response variable for the testing set,
# you should save them to a vector called predvect and write them into a .csv file 
# using the following code:



