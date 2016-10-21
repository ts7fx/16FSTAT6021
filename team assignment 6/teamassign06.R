# team assignment 6

#   Team 2     
#   Hampton Leonard, hll4ce
#   Tyler Worthington, tjw4ry
#   Andrew Pomykalski, ajp5sb
#   Tianye Song, ts7fx

# List of variables:
# 1 school - student's school (binary: 'GP' or 'MS')
# 2 sex - student's sex (binary: 'F' - female or 'M' - male)
# 3 age - student's age (numeric: from 15 to 22)
# 4 address - student's home address type (binary: 'U' - urban or 'R' - rural)
# 5 famsize - family size (binary: 'LE3' - less or equal to 3 or 'GT3' - greater than 3)
# 6 Pstatus - parent's cohabitation status (binary: 'T' - living together or 'A' - apart)
# 7 Medu - mother's education (numeric: 0 - none, 1 - primary education (4th grade), 
#                               2 - 5th to 9th grade, 3 - secondary education,
#                               or 4 - higher education)
# 8 Fedu - father's education (numeric: 0 - none, 1 - primary education (4th grade),
#                               2 - 5th to 9th grade, 3 - secondary education, 
#                               or 4 - higher education)
# 9 Mjob - mother's job (nominal: 'teacher', 'health' care related, civil 'services'
#                        (e.g. administrative or police), 'at_home' or 'other')
# 10 Fjob - father's job (nominal: 'teacher', 'health' care related, civil 'services'
#                         (e.g. administrative or police), 'at_home' or 'other')
# 11 reason - reason to choose this school (nominal: close to 'home', school 'reputation',
#                                           'course' preference or 'other')
# 12 guardian - student's guardian (nominal: 'mother', 'father' or 'other')
# 13 traveltime - home to school travel time (numeric: 1 - <15 min., 2 - 15 to 30 min.,
#                                             3 - 30 min. to 1 hour, or 4 - >1 hour)
# 14 studytime - weekly study time (numeric: 1 - <2 hours, 2 - 2 to 5 hours,
#                                   3 - 5 to 10 hours, or 4 - >10 hours)
# 15 failures - number of past class failures (numeric: n if 1<=n<3, else 4)
# 16 schoolsup - extra educational support (binary: yes or no)
# 17 famsup - family educational support (binary: yes or no)
# 18 paid - extra paid classes within the course subject (binary: yes or no)
# 19 activities - extra-curricular activities (binary: yes or no)
# 20 nursery - attended nursery school (binary: yes or no)
# 21 higher - wants to take higher education (binary: yes or no)
# 22 internet - Internet access at home (binary: yes or no)
# 23 romantic - with a romantic relationship (binary: yes or no)
# 24 famrel - quality of family relationships (numeric: from 1 - very bad to 5 - excellent)
# 25 freetime - free time after school (numeric: from 1 - very low to 5 - very high)
# 26 goout - going out with friends (numeric: from 1 - very low to 5 - very high)
# 27 Dalc - workday alcohol consumption (numeric: from 1 - very low to 5 - very high)
# 28 Walc - weekend alcohol consumption (numeric: from 1 - very low to 5 - very high)
# 29 health - current health status (numeric: from 1 - very bad to 5 - very good)
# 30 absences - number of school absences (numeric: from 0 to 93)
# 31 Grade (numeric: from 0 to 20)
setwd("~/Documents/git/16FSTAT6021")
library(readr)
library(nnet)
library(lars)

train<-read_csv('team assignment 6/teamassign06train.csv')
test<-read_csv('team assignment 6/teamassign06test.csv')
# check for missing values:
sum(is.na(train))
sum(is.na(test)) 
# both give zero, thus no missing values.
plot(train$absences,train$Grade)
# this is a prediction question. what are we predicting? 
# response variable: col 31 Grade (numeric: from 0 to 20)
# what are the performance indeces for prediction problems?

# first check out what are variable classes
apply(train,2,class)
# school         sex         age     address     famsize     Pstatus        Medu        Fedu        Mjob        Fjob 
# "character" "character" "character" "character" "character" "character" "character" "character" "character" "character" 
# reason    guardian  traveltime   studytime    failures   schoolsup      famsup        paid  activities     nursery 
# "character" "character" "character" "character" "character" "character" "character" "character" "character" "character" 
# higher    internet    romantic      famrel    freetime       goout        Dalc        Walc      health    absences 
# "character" "character" "character" "character" "character" "character" "character" "character" "character" "character" 
# Grade 
# "character" 

# a lot of categorical variables. logistic regression?

# first things first. convert categorical variables to factors:
cols <- colnames(train)[-c(3,31,30)]
train[,cols]<- lapply(train[,cols], factor)
summary(train)

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
write.table(predvect, file="teamassign06preds.csv", row.names=F, col.names=F, sep=",")

