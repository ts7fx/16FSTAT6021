### team assignment 6 ###
### Team 2     
#   Hampton Leonard, hll4ce
#   Tyler Worthington, tjw4ry
#   Andrew Pomykalski, ajp5sb
#   Tianye Song, ts7fx

#### setting things up
setwd("~/Documents/git/16FSTAT6021")
library(readr)
library(MASS)
library(DAAG)
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
# looks like age is categorical after all.
cols <- colnames(train)[-c(31,30)] # thus, only exclude "absences" & "grades" from the set of factors
train[,cols]<- lapply(train[,cols], factor)
# create a nozero dataset for log transformation of Grade
nozero<-train[!train$Grade == 0,]
# create a dataset in which every grade is added 0.01 for log transformation
train.incre <- train
train.incre$Grade <- train.incre$Grade+0.01

#### model fitting and selection:
# two models: 
#   1. model fitted using nozero data set
#   2. model fitted using train.incre

# 1. nozero mode:
lm.null <- lm(Grade~1, data = nozero)
lm.full <- lm(Grade~., data = nozero)
step(lm.null, scope=list(lower=lm.null, upper=lm.full), direction="forward")
# lm(formula = Grade ~ failures + higher + sex + Medu + schoolsup + absences + reason + internet, data = nozero)
## Backward selection
step(lm.full, scope=list(lower=lm.null, upper=lm.full), direction="backward")
# lm(formula = Grade ~ sex + Medu + reason + failures + schoolsup + higher + internet + absences, data = nozero)
## Stepwise selection
step(lm.null, scope=list(lower=lm.null, upper=lm.full), direction="both")
# lm(formula = Grade ~ failures + higher + sex + Medu + schoolsup + absences + reason + internet, data = nozero)
# all the same. 
lm.1 <- lm(Grade ~ failures + higher + sex + Medu + schoolsup + absences + reason + internet, data = nozero)
# 2. fit model using all data, but incremented by 0.01
lm.full.2 <- lm(Grade~.,data = train.incre)
lm.null.2 <- lm(Grade~1, data = train.incre)
step(lm.null.2, scope=list(lower=lm.null.2, upper=lm.full.2), direction="both")
# lm(formula = Grade ~ failures + higher + school + sex + schoolsup + 
#      goout + internet + studytime + Fedu + famrel, data = train.incre)
step(lm.null.2, scope=list(lower=lm.null.2, upper=lm.full.2), direction="forward")
# lm(formula = Grade ~ failures + higher + school + sex + schoolsup + 
#      goout + internet + studytime + Fedu + famrel, data = train.incre)
step(lm.full.2, scope=list(lower=lm.null.2, upper=lm.full.2), direction="backward")
# lm(formula = Grade ~ school + sex + Fedu + studytime + failures + 
#      schoolsup + higher + internet + famrel + goout, data = train.incre)
# again, same.
lm.2 <- lm(Grade ~ school + sex + Fedu + studytime + failures + schoolsup + higher + internet + famrel + goout, data = train.incre)
# Regularization won't make sense because partial selection of categorical variable value will not make sense. 

#### testing using CV
# CV two models on nozero dataset
cv.lm(data=nozero, form.lm=lm.1, m=5, plotit=F) # overall ms 5.14
cv.lm(data=nozero[!nozero$Fedu==0,], form.lm=lm.2, m=5, plotit=F) # overall ms 5.7

# CV two models on complete dataset
# take out observation with Fedu == 0, compromise one observation for CV purpose.
cv.lm(data=train.incre[!train.incre$Fedu==0,], form.lm=lm.2, m=5, plotit=F) # overall ms 8.39
# if CV model 1 one all data
cv.lm(data=train.incre, form.lm=lm.1, m=5, plotit=F) #overall ms 8.79

#### CV results (Unit: mse)
#            M1    M2
# nozero   5.14   5.7
# complete 8.79   8.39

# each model performed better on dataset based on which the model is generated.
# we believe we should not sacrifice the not-so-large amount of data at hand.
# with similar performance on both models, we chose the model (lm.2) which is generated based on the complete dataset. 
cols <- colnames(test)[-30] # thus, only exclude "absences" & "grades" from the set of factors
test[,cols]<- lapply(test[,cols], factor)
predvect<-predict(lm.2, newdata = test)

#### write to output file
write.table(predvect, file="teamassign06preds.csv", row.names=F, col.names=F, sep=",")
