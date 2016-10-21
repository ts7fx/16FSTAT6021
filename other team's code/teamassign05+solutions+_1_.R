###########################
#                         #
#   Team Assignment 5     #
#                         #
###########################

## Names:
# Kaley Hanrahan (knh9yd)
# Chris Patrick (cpp4f)
# Kevin Sun (kws8de)
# Qi Tang (qt4ur)

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

#
# Your annotated R code should explain the reasoning behind your choices in 
# model selection and should be neatly organized.
#
# Your grade on this team assignment will be based on how well your model predicts
# the observed values relative to the other teams.

#Installing Libraries
install.packages("leaps")
library("leaps")
install.packages("DAAG")
library("DAAG")

#Importing Data and Setting Seed
seed <- 17
set.seed(seed)

train.full <- read.csv('teamassign05train.csv') #reading in Training Data
train.full.randomized <- train.full[sample(nrow(train.full)),] #randomizing the training dataset

test.full <-read.csv('teamassign05test.csv') #reading in Testing Data
test.full.randomized <- test.full[sample(nrow(test.full)),]

#Exploratory Analysis
#Observation Predictor and Response variables in the training data
plot(train.full.randomized)
#By a terse look at the plots, it seems like the univariate relationship of x1, x2, x4 is more flexible than just
#linear to y. Similarly, it seems that the univariate reationship of x7, x6 seem to be more inflexible (linear) 
#to y. It seems that the relationship with x5 with y is pretty weak.

#Furthermore by looking at variable x6, there seems to be clusters. Exploring x6
unique(train.full$x6)
View(train.full.randomized[train.full.randomized$x6==0.00,])
View(train.full.randomized[train.full.randomized$x6==0.10,])
View(train.full.randomized[train.full.randomized$x6==0.25,])
View(train.full.randomized[train.full.randomized$x6==0.40,])
#after sorting on x1 in each of these views, we see that there are groupings of x1,x2 and x4 combinations

#check the correlation matrix
cor(train.full.randomized)
#surprising to see that x2 is not correlated with x1. Doesn't surprise me that x4 is highly correlated with x1. 
#hypothesize that perhaps we need x2 and either x1 or x4 in our model.

#check to see if we get similar groupings in the testing data set
View(test.full.randomized[test.full.randomized$x6==0.00,])
View(test.full.randomized[test.full.randomized$x6==0.10,])
View(test.full.randomized[test.full.randomized$x6==0.25,])
View(test.full.randomized[test.full.randomized$x6==0.40,])
#exact same grouping values of x1, x2, and x4 based on the value of x6

#because this problem is a prediction problem, no need to check for multicollinearity for model selection.
#if it was an inference problem, then we need to check for multicollinearity.

#Doing forward, backward, and hybrid subsetting on variables just as it is. Using K-fold CV to do subsetting
#for forward and backward due to less bias compared to the subsetting metric used in the leaps library, since
#K-fold cross validation uses testing MSE of the "testing" fold that is left out. 

#Create a function that returns the rows in df.1 that are NOT IN the rows in df.2 
#The function returns the difference between df.1 and df.2: (df.1 MINUS df.2)
#This function is needed in the model selection by using K-fold cross validation
fun.get.difference <- function(df.1,df.2,...){
  df.1p <- do.call("paste", df.1)
  df.2p <- do.call("paste", df.2)
  df.1[! df.1p %in% df.2p, ]
}

#Doing forward selection 

predictor.mse.list <- c() #to save MSEs of each predictor per node test
predictor.list <- c() #to save which predictors per node test
model.mse.list <- c() # to save best model MSEs for each n-predictors we test
model.predictor.list <- c() #to save the best models for each n-predictors we test

#Creating Predictor Columns
column.name.is.y= names(train.full) == "y"
predictor.columns = names(train.full)[!column.name.is.y]

k <- 5 #number of folds
n <- nrow(train.full.randomized) #number of rows
fold.length <- n/k #number of observations per fold  
#but due to simplicity catered to the team assignment, I will go with this method

list.index = 1 #index used to printing out mse values for each predictor

#Round 1: Find single-best predictor 
for(predictor in predictor.columns){
  data.columns <- c("y",predictor)
  temp.mse <- c() #Temporary MSE for each k folds for a single predictor 
  for(index.k in 1:k){ #for each predictor, we need to do cross validation to calculate the MSE
    testing.set <- train.full.randomized[(fold.length*(index.k-1)+1):(fold.length*index.k), ] #Create testing set
    training.set <- fun.get.difference(train.full.randomized, testing.set) #Create training set
    lm.fit = lm(y ~ ., data = training.set[,data.columns])  
    predictions = predict(lm.fit, newdata = testing.set[,data.columns], type="response")
    mse = mean((predictions - testing.set$y)^2)
    temp.mse <-c(temp.mse, mse)
  }
  predictor.mse.list = c(predictor.mse.list, mean(temp.mse))
  predictor.list <- c(predictor.list, predictor)
  print(paste("MSE for ", predictor, ":  ", as.list(predictor.mse.list)[list.index], sep=""))
  list.index = list.index+1
}

model.mse.list <- c(model.mse.list, min(predictor.mse.list)) #recording the best MSE into the list
model.predictor.list <-c(model.predictor.list, predictor.list[which.min(predictor.mse.list)]) #recording the predictors of the best model

num.predictors <- ncol(train.full.randomized)-1

#For each n-variables aside from 1
for(p in 1:(num.predictors-1)){
  previous.predictors <- unlist(strsplit(model.predictor.list[p], " ")) #Get previous predictors in our model
  predictor.columns <- predictor.columns[predictor.columns !=previous.predictors]
  predictor.mse.list <- c() #to save MSEs of each predictor per node test
  predictor.list <- c() #to save which predictors per node test
  
  list.index = 1 #index used to printing out mse values for each predictor
  
  for(predictor in predictor.columns){
    data.columns <- c("y",previous.predictors, predictor)
    temp.mse <- c() #Temporary MSE for each k folds for a single predictor 
    for(index.k in 1:k){ #for each predictor, we need to do cross validation to calculate the MSE
      testing.set <- train.full.randomized[(fold.length*(index.k-1)+1):(fold.length*index.k), ] #Create testing set
      training.set <- fun.get.difference(train.full.randomized, testing.set) #Create training set
      lm.fit = lm(y ~ ., data = training.set[,data.columns])  
      predictions = predict(lm.fit, newdata = testing.set[,data.columns], type="response")
      mse = mean((predictions - testing.set$y)^2)
      temp.mse <-c(temp.mse, mse)
    }
    predictor.mse.list = c(predictor.mse.list, mean(temp.mse))
    predictor.list <- c(predictor.list, predictor)
    print(paste("MSE for ", predictor, ":  ", as.list(predictor.mse.list)[list.index], sep=""))
    list.index = list.index+1
  }
  
  min(predictor.mse.list) #find best MSE
  which.min(predictor.mse.list) #Find index with the best MSE
  model.mse.list <- c(model.mse.list, min(predictor.mse.list))
  model.predictor.list <-c(model.predictor.list, paste(c(previous.predictors, predictor.list[which.min(predictor.mse.list)]), collapse = " ")) #cumulatively 
  #recording the predictors of the best model 
}

model.mse.list
model.predictor.list
model.mse.list[which.min(model.mse.list)] #Lowest CV MSE
model.predictor.list[which.min(model.mse.list)] #Predictors of Lowest CV MSE
# > model.mse.list[which.min(model.mse.list)] #Lowest CV MSE
# [1] 11.6
# > model.predictor.list[which.min(model.mse.list)] #Predictors of Lowest CV MSE
# [1] "x3 x7 x6 x4 x1 x2"

#Using Backward subset 
#Creating Predictor Columns
column.name.is.y= names(train.full) == "y"
predictor.columns = names(train.full)[!column.name.is.y]

predictor.mse.list <- c() #to save MSEs of each predictor per node test
predictor.list <- c() #to save which predictors per node test
model.mse.list <- c() # to save best model MSEs for each n-predictors we test
model.predictor.list <- c() #to save the best models for each n-predictors we test

#Round 1: Drop the single worse predictor 
list.index = 1 #index used to printing out mse values for each predictor
for(predictor in predictor.columns){
  remove<-predictor
  data.columns <- c("y",predictor.columns [!predictor.columns%in%remove])
  temp.mse <- c() #Temporary MSE for each k folds for a single predictor 
  for(index.k in 1:k){ #for each predictor, we need to do cross validation to calculate the MSE
    testing.set <- train.full.randomized[(fold.length*(index.k-1)+1):(fold.length*index.k), ] #Create testing set
    training.set <- fun.get.difference(train.full.randomized, testing.set) #Create training set
    lm.fit = lm(y ~ ., data = training.set[,data.columns])  
    predictions = predict(lm.fit, newdata = testing.set[,data.columns], type="response")
    mse = mean((predictions - testing.set$y)^2)
    temp.mse <-c(temp.mse, mse)
  }
  predictor.mse.list = c(predictor.mse.list, mean(temp.mse))
  predictor.list <- c(predictor.list, paste(predictor.columns [!predictor.columns%in%remove], collapse = " "))
  print(paste("MSE by removing ", predictor, ":  ", as.list(predictor.mse.list)[list.index], sep=""))
  list.index = list.index+1
}

min(predictor.mse.list) #find best MSE
which.min(predictor.mse.list) #Find index with the best MSE
model.mse.list <- c(model.mse.list, min(predictor.mse.list)) #recording the best MSE into the list
model.predictor.list <-c(model.predictor.list, predictor.list[which.min(predictor.mse.list)]) #recording the predictors of the best model

#Round 2 For each n-variables aside from 1, drop the single worst predictor 
num.predictors <- ncol(train.full.randomized)-1

for(p in 1:(num.predictors-2)){
  previous.predictors <- unlist(strsplit(model.predictor.list[p], " ")) #Get previous predictors in our model
  predictor.columns <- previous.predictors
  predictor.mse.list <- c() #to save MSEs of each predictor per node test
  predictor.list <- c() #to save which predictors per node test
  
  list.index = 1 #index used to printing out mse values for each model with 1 predictor removed
  
  for(predictor in predictor.columns){
    remove<-predictor
    data.columns <- c("y",predictor.columns [!predictor.columns%in%remove])
    temp.mse <- c() #Temporary MSE for each k folds for a single predictor 
    if(length(data.columns) != 1){
      for(index.k in 1:k){ #for each predictor, we need to do cross validation to calculate the MSE
        testing.set <- train.full.randomized[(fold.length*(index.k-1)+1):(fold.length*index.k), ] #Create testing set
        training.set <- fun.get.difference(train.full.randomized, testing.set) #Create training set
        lm.fit = lm(y ~ ., data = training.set[,data.columns])  
        predictions = predict(lm.fit, newdata = testing.set[,data.columns], type="response")
        mse = mean((predictions - testing.set$y)^2)
        temp.mse <-c(temp.mse, mse)
      }
      predictor.mse.list = c(predictor.mse.list, mean(temp.mse))
      predictor.list <- c(predictor.list, paste(predictor.columns [!predictor.columns%in%remove], collapse = " "))
      print(paste("MSE by removing ", predictor, ":  ", as.list(predictor.mse.list)[list.index], sep=""))
      list.index = list.index+1
    }
  }
  
  min(predictor.mse.list) #find best MSE
  which.min(predictor.mse.list) #Find index with the best MSE
  model.mse.list <- c(model.mse.list, min(predictor.mse.list))
  model.predictor.list <-c(model.predictor.list, predictor.list[which.min(predictor.mse.list)]) #cumulatively 
  #recording the predictors of the best model 
}

model.mse.list
model.predictor.list
model.mse.list[which.min(model.mse.list)] #Lowest CV MSE
model.predictor.list[which.min(model.mse.list)] #Predictors of Lowest CV MSE
# > model.mse.list[which.min(model.mse.list)] #Lowest CV MSE
# [1] 11.6
# > model.predictor.list[which.min(model.mse.list)] #Predictors of Lowest CV MSE
# [1] "x1 x2 x3 x4 x6 x7"
#Same as the forward selection

#Using hybrid subsetting. Used the leaps package version because writing the K-fold cross validation method was too hard
s.null <- lm(y~1, data=train.full.randomized)
s.full <- lm(y~., data=train.full.randomized)
step(s.null, scope=list(lower=s.null, upper=s.full), direction="both")
#lm(formula = y ~ x3 + x7 + x6 + x4 + x1 + x2, data = train.full.randomized)
#Got the same model as the K-fold CV forward/backward version
lm.step.inflex <-lm(formula = y ~ x3 + x7 + x6 + x4 + x1 + x2, data = train.full.randomized)
#Use 5 fold CV to see how well this model do
cv.lm(data=train.full.randomized, form.lm=lm.step.inflex, m=5, plotit=F) #MSE = 11.7

#plotting residuals vs fitted for stepwise/hybrid model
#studentized residuals
par(mfrow=c(1,1))
step.inflex.resid.stu <- rstandard(lm.step.inflex)
step.inflex.pred <-predict(lm.step.inflex, newdata=train.full.randomized2)
plot(step.inflex.resid.stu ~ step.inflex.pred)
#Heteroskedasticity appears (inconsistant variance). Model gets inaccurate for predicted y values in the range of 25 to 40.

#perhaps a more flexible model is better

#Looking at the distribution of the predictor variables in the testing dataset compared to that in the training dataset to 
#see if there is any risk in providing a more flexible model.

#x1
par(mfrow=c(1,2))
plot(sort(train.full$x1))
plot(sort(test.full$x1))
#x2
par(mfrow=c(1,2))
plot(sort(train.full$x2))
plot(sort(test.full$x2))
#x3
par(mfrow=c(1,2))
plot(sort(train.full$x3))
plot(sort(test.full$x3))
#x4
par(mfrow=c(1,2))
plot(sort(train.full$x4))
plot(sort(test.full$x4))
#x5
par(mfrow=c(1,2))
plot(sort(train.full$x5))
plot(sort(test.full$x5))
#x6
par(mfrow=c(1,2))
plot(sort(train.full$x6))
plot(sort(test.full$x6))
#x7
par(mfrow=c(1,2))
plot(sort(train.full$x7))
plot(sort(test.full$x7))

#Distribution and values of the predictor variables in the testing and training data set is similar, so the risk should not be too high.
#furthermore, we looked at the groupings of x1,x2 and x4 before and found exact same groupings. So providing a more flexible method is valid

#Making a new flexible model
train.full.randomized2 <- train.full.randomized

#Making functional form transformations for all predictor values
#x1 transformations
train.full.randomized2$invX1 <- 1/train.full.randomized$x1
train.full.randomized2$logX1 <- log(train.full.randomized$x1)
train.full.randomized2$sqrX1 <- train.full.randomized$x1^2
train.full.randomized2$rootX1 <- train.full.randomized$x1^0.5

#x2 transformations
train.full.randomized2$invX2 <- 1/train.full.randomized$x2
train.full.randomized2$logX2 <- log(train.full.randomized$x2)
train.full.randomized2$sqrX2 <- train.full.randomized$x2^2
train.full.randomized2$rootX2 <- train.full.randomized$x2^0.5

#x3 transformations
train.full.randomized2$invX3 <- 1/train.full.randomized$x3
train.full.randomized2$logX3 <- log(train.full.randomized$x3)
train.full.randomized2$sqrX3 <- train.full.randomized$x3^2
train.full.randomized2$rootX3 <- train.full.randomized$x3^0.5

#x4 transformations
train.full.randomized2$invX4 <- 1/train.full.randomized$x4
train.full.randomized2$logX4 <- log(train.full.randomized$x4)
train.full.randomized2$sqrX4 <- train.full.randomized$x4^2
train.full.randomized2$rootX4 <- train.full.randomized$x4^0.5

#x5 transformations
train.full.randomized2$invX5 <- 1/train.full.randomized$x5
train.full.randomized2$logX5 <- log(train.full.randomized$x5)
train.full.randomized2$sqrX5 <- train.full.randomized$x5^2
train.full.randomized2$rootX5 <- train.full.randomized$x5^0.5

#x6 transformations
train.full.randomized2$sqrX6 <- train.full.randomized$x6^2
train.full.randomized2$rootX6 <- train.full.randomized$x6^0.5

#x7 transformations
train.full.randomized2$invX7 <- 1/train.full.randomized$x7
train.full.randomized2$logX7 <- log(train.full.randomized$x7)
train.full.randomized2$sqrX7 <- train.full.randomized$x7^2
train.full.randomized2$rootX7 <- train.full.randomized$x7^0.5

#Doing forward subset selection
predictor.mse.list <- c() #to save MSEs of each predictor per node test
predictor.list <- c() #to save which predictors per node test
model.mse.list <- c() # to save best model MSEs for each n-predictors we test
model.predictor.list <- c() #to save the best models for each n-predictors we test

#Creating Predictor Columns
column.name.is.y= names(train.full.randomized2) == "y"
predictor.columns = names(train.full.randomized2)[!column.name.is.y]

k <- 5 #number of folds
n <- nrow(train.full.randomized2) #number of rows
fold.length <- n/k #number of observations per fold 
#but due to simplicity catered to the team assignment, I will go with this method

list.index = 1 #index used to printing out mse values for each predictor
#Round 1: Find single-best predictor 
for(predictor in predictor.columns){
  data.columns <- c("y",predictor)
  temp.mse <- c() #Temporary MSE for each k folds for a single predictor 
  for(index.k in 1:k){ #for each predictor, we need to do cross validation to calculate the MSE
    testing.set <- train.full.randomized2[(fold.length*(index.k-1)+1):(fold.length*index.k), ] #Create testing set
    training.set <- fun.get.difference(train.full.randomized2, testing.set) #Create training set
    lm.fit = lm(y ~ ., data = training.set[,data.columns])  
    predictions = predict(lm.fit, newdata = testing.set[,data.columns], type="response")
    mse = mean((predictions - testing.set$y)^2)
    temp.mse <-c(temp.mse, mse)
  }
  predictor.mse.list = c(predictor.mse.list, mean(temp.mse))
  predictor.list <- c(predictor.list, predictor)
  print(paste("MSE for ", predictor, ":  ", as.list(predictor.mse.list)[list.index], sep=""))
  list.index = list.index+1
}

model.mse.list <- c(model.mse.list, min(predictor.mse.list)) #recording the best MSE into the list
model.predictor.list <-c(model.predictor.list, predictor.list[which.min(predictor.mse.list)]) #recording the predictors of the best model

num.predictors <- ncol(train.full.randomized2)-1

#For each n-variables aside from 1
for(p in 1:(num.predictors-1)){
  previous.predictors <- unlist(strsplit(model.predictor.list[p], " ")) #Get previous predictors in our model
  predictor.columns <- predictor.columns[predictor.columns !=previous.predictors]
  predictor.mse.list <- c() #to save MSEs of each predictor per node test
  predictor.list <- c() #to save which predictors per node test
  
  list.index = 1 #index used to printing out mse values for each predictor
  
  for(predictor in predictor.columns){
    data.columns <- c("y",previous.predictors, predictor)
    temp.mse <- c() #Temporary MSE for each k folds for a single predictor 
    for(index.k in 1:k){ #for each predictor, we need to do cross validation to calculate the MSE
      testing.set <- train.full.randomized2[(fold.length*(index.k-1)+1):(fold.length*index.k), ] #Create testing set
      training.set <- fun.get.difference(train.full.randomized2, testing.set) #Create training set
      lm.fit = lm(y ~ ., data = training.set[,data.columns])  
      predictions = predict(lm.fit, newdata = testing.set[,data.columns], type="response")
      mse = mean((predictions - testing.set$y)^2)
      temp.mse <-c(temp.mse, mse)
    }
    predictor.mse.list = c(predictor.mse.list, mean(temp.mse))
    predictor.list <- c(predictor.list, predictor)
    print(paste("MSE for ", predictor, ":  ", as.list(predictor.mse.list)[list.index], sep=""))
    list.index = list.index+1
  }
  
  min(predictor.mse.list) #find best MSE
  which.min(predictor.mse.list) #Find index with the best MSE
  model.mse.list <- c(model.mse.list, min(predictor.mse.list))
  model.predictor.list <-c(model.predictor.list, paste(c(previous.predictors, predictor.list[which.min(predictor.mse.list)]), collapse = " ")) #cumulatively 
  #recording the predictors of the best model 
}

model.mse.list
model.predictor.list
model.mse.list[which.min(model.mse.list)] #Lowest CV MSE
model.predictor.list[which.min(model.mse.list)] #Predictors of Lowest CV MSE
# > model.mse.list[which.min(model.mse.list)] #Lowest CV MSE
# [1] 3.43
# > model.predictor.list[which.min(model.mse.list)] #Predictors of Lowest CV MSE
# [1] "sqrX3 rootX6 sqrX4 invX3 x3 rootX3 sqrX7 invX7 invX2 invX4 invX1 sqrX1 logX3 x1 logX4 sqrX2 x2 logX2"

#MSE is lower: (3.43 compared to 11.6) but number of predictors increased by a lot (18 predictors vs 6)

#Backward subset selection using K-fold CV validation
column.name.is.y= names(train.full.randomized2) == "y"
predictor.columns = names(train.full.randomized2)[!column.name.is.y]

predictor.mse.list <- c() #to save MSEs of each predictor per node test
predictor.list <- c() #to save which predictors per node test
model.mse.list <- c() # to save best model MSEs for each n-predictors we test
model.predictor.list <- c() #to save the best models for each n-predictors we test

#Round 1: Drop the single worse predictor 
list.index = 1 #index used to printing out mse values for each predictor
for(predictor in predictor.columns){
  remove<-predictor
  data.columns <- c("y",predictor.columns [!predictor.columns%in%remove])
  temp.mse <- c() #Temporary MSE for each k folds for a single predictor 
  for(index.k in 1:k){ #for each predictor, we need to do cross validation to calculate the MSE
    testing.set <- train.full.randomized2[(fold.length*(index.k-1)+1):(fold.length*index.k), ] #Create testing set
    training.set <- fun.get.difference(train.full.randomized2, testing.set) #Create training set
    lm.fit = lm(y ~ ., data = training.set[,data.columns])  
    predictions = predict(lm.fit, newdata = testing.set[,data.columns], type="response")
    mse = mean((predictions - testing.set$y)^2)
    temp.mse <-c(temp.mse, mse)
  }
  predictor.mse.list = c(predictor.mse.list, mean(temp.mse))
  predictor.list <- c(predictor.list, paste(predictor.columns [!predictor.columns%in%remove], collapse = " "))
  print(paste("MSE by removing ", predictor, ":  ", as.list(predictor.mse.list)[list.index], sep=""))
  list.index = list.index+1
}

model.mse.list <- c(model.mse.list, min(predictor.mse.list)) #recording the best MSE into the list
model.predictor.list <-c(model.predictor.list, predictor.list[which.min(predictor.mse.list)]) #recording the predictors of the best model

#Round 2 For each n-variables aside from 1, drop the single worst predictor 
num.predictors <- ncol(train.full.randomized2)-1

for(p in 1:(num.predictors-2)){
  previous.predictors <- unlist(strsplit(model.predictor.list[p], " ")) #Get previous predictors in our model
  predictor.columns <- previous.predictors
  predictor.mse.list <- c() #to save MSEs of each predictor per node test
  predictor.list <- c() #to save which predictors per node test
  
  list.index = 1 #index used to printing out mse values for each model with 1 predictor removed
  
  for(predictor in predictor.columns){
    remove<-predictor
    data.columns <- c("y",predictor.columns [!predictor.columns%in%remove])
    temp.mse <- c() #Temporary MSE for each k folds for a single predictor 
    if(length(data.columns) != 1){
      for(index.k in 1:k){ #for each predictor, we need to do cross validation to calculate the MSE
        testing.set <- train.full.randomized2[(fold.length*(index.k-1)+1):(fold.length*index.k), ] #Create testing set
        training.set <- fun.get.difference(train.full.randomized2, testing.set) #Create training set
        lm.fit = lm(y ~ ., data = training.set[,data.columns])  
        predictions = predict(lm.fit, newdata = testing.set[,data.columns], type="response")
        mse = mean((predictions - testing.set$y)^2)
        temp.mse <-c(temp.mse, mse)
      }
      predictor.mse.list = c(predictor.mse.list, mean(temp.mse))
      predictor.list <- c(predictor.list, paste(predictor.columns [!predictor.columns%in%remove], collapse = " "))
      print(paste("MSE by removing ", predictor, ":  ", as.list(predictor.mse.list)[list.index], sep=""))
      list.index = list.index+1
    }
  }
  
  min(predictor.mse.list) #find best MSE
  which.min(predictor.mse.list) #Find index with the best MSE
  model.mse.list <- c(model.mse.list, min(predictor.mse.list))
  model.predictor.list <-c(model.predictor.list, predictor.list[which.min(predictor.mse.list)]) #cumulatively 
  #recording the predictors of the best model 
}

model.mse.list
model.predictor.list
model.mse.list[which.min(model.mse.list)] #Lowest CV MSE
model.predictor.list[which.min(model.mse.list)] #Predictors of Lowest CV MSE
# > model.mse.list[which.min(model.mse.list)] #Lowest CV MSE
# [1] 3.39
# > model.predictor.list[which.min(model.mse.list)] #Predictors of Lowest CV MSE
# [1] x1 x2 x4 logX1 sqrX1 rootX1 sqrX2 rootX2 invX4 logX4 sqrX4 rootX6"

#Pretty good model. Only 12 predictors and a pretty low MSE of 3.39

#Hybrid Subsetting 
s.null <- lm(y~1, data=train.full.randomized2)
s.full <- lm(y~., data=train.full.randomized2)
step(s.null, scope=list(lower=s.null, upper=s.full), direction="both")
# lm(formula = y ~ sqrX3 + rootX6 + sqrX4 + invX3 + x3 + rootX3 + 
#      sqrX7 + x7 + invX4 + invX1 + sqrX1 + logX3, data = train.full.randomized2)

lm.step.flex <- lm(formula = y ~ sqrX3 + rootX6 + sqrX4 + invX3 + x3 + rootX3 + 
                     sqrX7 + x7 + invX4 + invX1 + sqrX1 + logX3, data = train.full.randomized2)

#Checking cross validation for hybrid subsetting with k = 5
cv.lm(data=train.full.randomized2, form.lm=lm.step.flex, m=5, plotit=F) 
#mse = 7.23, which does worse than the forward/backward subset models by using k-fold cross validation. However
#the number of predictors is only 12

#reconfirming the CV for the forward/backward subset models 
#forward
lm.forward.flex <- lm(y~sqrX3 + rootX6 + sqrX4 + invX3 + x3 + rootX3 + sqrX7 + invX7 + invX2 + invX4 + 
                        invX1 + sqrX1 + logX3 + x1 + logX4 + sqrX2 + x2 + logX2,
                      data = train.full.randomized2)
cv.lm(data=train.full.randomized2, form.lm=lm.forward.flex, m=5, plotit=F) 
#mse = 3.41, which does better than the hybrid, but may be overfitting due to 18 variables

#backward
lm.backward.flex <- lm(y~x1 + x2 + x4 + logX1 + sqrX1 + rootX1 + sqrX2 + 
                         rootX2 + invX4 + logX4 + sqrX4 + rootX6, data = train.full.randomized2)
cv.lm(data=train.full.randomized2, form.lm=lm.backward.flex, m=5, plotit=F) 
#mse = 3.28, which does better than the hybrid, and has only 12 variables. This is a good candidate so far.

#looking at residual plots vs fitted for these models
#studentized residuals for hybrid/stepwise flexible model 
par(mfrow=c(1,1))
step.flex.resid.stu <- rstandard(lm.step.flex)
step.flex.pred <-predict(lm.step.flex, newdata=train.full.randomized2)
plot(step.flex.resid.stu ~ step.flex.pred)
#Heteroskedasticity still appears due to the fanning out plot

#studentized residuals for forward flexible model 
par(mfrow=c(1,1))
forward.flex.resid.stu <- rstandard(lm.forward.flex)
forward.flex.pred <-predict(lm.forward.flex, newdata=train.full.randomized2)
plot(forward.flex.resid.stu ~ forward.flex.pred)
#Major Heteroskedasticity appears due to the fanning out plot

#studentized residuals for backward flexible model 
par(mfrow=c(1,1))
backward.flex.resid.stu <- rstandard(lm.backward.flex)
backward.flex.pred <-predict(lm.backward.flex, newdata=train.full.randomized2)
plot(backward.flex.resid.stu ~ backward.flex.pred)
#Major Heteroskedasticity appears due to the fanning out plot

#Due to the fanning out residual plots and the major heteroskedasticity, it makes sense to try to use log(y) as the response variable

#Using Log(y) as the response variable 
train.full.randomized3 <- train.full.randomized2

train.full.randomized3$y <- log(train.full.randomized3$y) #the y in train.full.randomized3 is now log(y)

#Forward subsetting
predictor.mse.list <- c() #to save MSEs of each predictor per node test
predictor.list <- c() #to save which predictors per node test
model.mse.list <- c() # to save best model MSEs for each n-predictors we test
model.predictor.list <- c() #to save the best models for each n-predictors we test

column.name.is.y= (names(train.full.randomized3) == 'y')
predictor.columns = names(train.full.randomized3)[!column.name.is.y]

k <- 5 #number of folds
n <- nrow(train.full.randomized3) #number of rows
fold.length <- n/k #number of observations per fold 
#but due to simplicity catered to the team assignment, I will go with this method

list.index = 1 #index used to printing out mse values for each predictor
#Round 1: Find single-best predictor 
for(predictor in predictor.columns){
  data.columns <- c("y",predictor)
  temp.mse <- c() #Temporary MSE for each k folds for a single predictor 
  for(index.k in 1:k){ #for each predictor, we need to do cross validation to calculate the MSE
    testing.set <- train.full.randomized3[(fold.length*(index.k-1)+1):(fold.length*index.k), ] #Create testing set
    training.set <- fun.get.difference(train.full.randomized3, testing.set) #Create training set
    lm.fit = lm(y ~ ., data = training.set[,data.columns])  
    predictions = predict(lm.fit, newdata = testing.set[,data.columns], type="response")
    mse = mean((predictions - testing.set$y)^2)
    temp.mse <-c(temp.mse, mse)
  }
  predictor.mse.list = c(predictor.mse.list, mean(temp.mse))
  predictor.list <- c(predictor.list, predictor)
  print(paste("MSE for ", predictor, ":  ", as.list(predictor.mse.list)[list.index], sep=""))
  list.index = list.index+1
}

model.mse.list <- c(model.mse.list, min(predictor.mse.list)) #recording the best MSE into the list
model.predictor.list <-c(model.predictor.list, predictor.list[which.min(predictor.mse.list)]) #recording the predictors of the best model

num.predictors <- ncol(train.full.randomized3)-1

#For each n-variables aside from 1
for(p in 1:(num.predictors-1)){
  previous.predictors <- unlist(strsplit(model.predictor.list[p], " ")) #Get previous predictors in our model
  predictor.columns <- predictor.columns[predictor.columns !=previous.predictors]
  predictor.mse.list <- c() #to save MSEs of each predictor per node test
  predictor.list <- c() #to save which predictors per node test
  
  list.index = 1 #index used to printing out mse values for each predictor
  
  for(predictor in predictor.columns){
    data.columns <- c("y",previous.predictors, predictor)
    temp.mse <- c() #Temporary MSE for each k folds for a single predictor 
    for(index.k in 1:k){ #for each predictor, we need to do cross validation to calculate the MSE
      testing.set <- train.full.randomized3[(fold.length*(index.k-1)+1):(fold.length*index.k), ] #Create testing set
      training.set <- fun.get.difference(train.full.randomized3, testing.set) #Create training set
      lm.fit = lm(y ~ ., data = training.set[,data.columns])  
      predictions = predict(lm.fit, newdata = testing.set[,data.columns], type="response")
      mse = mean((predictions - testing.set$y)^2)
      temp.mse <-c(temp.mse, mse)
    }
    predictor.mse.list = c(predictor.mse.list, mean(temp.mse))
    predictor.list <- c(predictor.list, predictor)
    print(paste("MSE for ", predictor, ":  ", as.list(predictor.mse.list)[list.index], sep=""))
    list.index = list.index+1
  }
  
  min(predictor.mse.list) #find best MSE
  which.min(predictor.mse.list) #Find index with the best MSE
  model.mse.list <- c(model.mse.list, min(predictor.mse.list))
  model.predictor.list <-c(model.predictor.list, paste(c(previous.predictors, predictor.list[which.min(predictor.mse.list)]), collapse = " ")) #cumulatively 
  #recording the predictors of the best model 
}

model.mse.list
model.predictor.list
model.mse.list[which.min(model.mse.list)] #Lowest CV MSE
model.predictor.list[which.min(model.mse.list)] #Predictors of Lowest CV MSE
# > model.mse.list[which.min(model.mse.list)] #Lowest CV MSE
# [1] 0.003
# > model.predictor.list[which.min(model.mse.list)] #Predictors of Lowest CV MSE
# [1] "sqrX3 rootX6 sqrX4 invX3 x3 rootX3 sqrX7 invX2 invX4 invX1 sqrX1 logX3 x1 sqrX2 x2 rootX1 rootX2 logX4"

#18 variables. Can't compare model's mse since log(y) is much smaller than y.

#Backward subset selection using log(y) as the response variable
column.name.is.y= names(train.full.randomized3) == "y"
predictor.columns = names(train.full.randomized3)[!column.name.is.y]

predictor.mse.list <- c() #to save MSEs of each predictor per node test
predictor.list <- c() #to save which predictors per node test
model.mse.list <- c() # to save best model MSEs for each n-predictors we test
model.predictor.list <- c() #to save the best models for each n-predictors we test

#Round 1: Drop the single worse predictor 
list.index = 1 #index used to printing out mse values for each predictor
for(predictor in predictor.columns){
  remove<-predictor
  data.columns <- c("y",predictor.columns [!predictor.columns%in%remove])
  temp.mse <- c() #Temporary MSE for each k folds for a single predictor 
  for(index.k in 1:k){ #for each predictor, we need to do cross validation to calculate the MSE
    testing.set <- train.full.randomized3[(fold.length*(index.k-1)+1):(fold.length*index.k), ] #Create testing set
    training.set <- fun.get.difference(train.full.randomized3, testing.set) #Create training set
    lm.fit = lm(y ~ ., data = training.set[,data.columns])  
    predictions = predict(lm.fit, newdata = testing.set[,data.columns], type="response")
    mse = mean((predictions - testing.set$y)^2)
    temp.mse <-c(temp.mse, mse)
  }
  predictor.mse.list = c(predictor.mse.list, mean(temp.mse))
  predictor.list <- c(predictor.list, paste(predictor.columns [!predictor.columns%in%remove], collapse = " "))
  print(paste("MSE by removing ", predictor, ":  ", as.list(predictor.mse.list)[list.index], sep=""))
  list.index = list.index+1
}

model.mse.list <- c(model.mse.list, min(predictor.mse.list)) #recording the best MSE into the list
model.predictor.list <-c(model.predictor.list, predictor.list[which.min(predictor.mse.list)]) #recording the predictors of the best model

#Round 2 For each n-variables aside from 1, drop the single worst predictor 
num.predictors <- ncol(train.full.randomized3)-1

for(p in 1:(num.predictors-2)){
  previous.predictors <- unlist(strsplit(model.predictor.list[p], " ")) #Get previous predictors in our model
  predictor.columns <- previous.predictors
  predictor.mse.list <- c() #to save MSEs of each predictor per node test
  predictor.list <- c() #to save which predictors per node test
  
  list.index = 1 #index used to printing out mse values for each model with 1 predictor removed
  
  for(predictor in predictor.columns){
    remove<-predictor
    data.columns <- c("y",predictor.columns [!predictor.columns%in%remove])
    temp.mse <- c() #Temporary MSE for each k folds for a single predictor 
    if(length(data.columns) != 1){
      for(index.k in 1:k){ #for each predictor, we need to do cross validation to calculate the MSE
        testing.set <- train.full.randomized3[(fold.length*(index.k-1)+1):(fold.length*index.k), ] #Create testing set
        training.set <- fun.get.difference(train.full.randomized3, testing.set) #Create training set
        lm.fit = lm(y ~ ., data = training.set[,data.columns])  
        predictions = predict(lm.fit, newdata = testing.set[,data.columns], type="response")
        mse = mean((predictions - testing.set$y)^2)
        temp.mse <-c(temp.mse, mse)
      }
      predictor.mse.list = c(predictor.mse.list, mean(temp.mse))
      predictor.list <- c(predictor.list, paste(predictor.columns [!predictor.columns%in%remove], collapse = " "))
      print(paste("MSE by removing ", predictor, ":  ", as.list(predictor.mse.list)[list.index], sep=""))
      list.index = list.index+1
    }
  }
  
  min(predictor.mse.list) #find best MSE
  which.min(predictor.mse.list) #Find index with the best MSE
  model.mse.list <- c(model.mse.list, min(predictor.mse.list))
  model.predictor.list <-c(model.predictor.list, predictor.list[which.min(predictor.mse.list)]) #cumulatively 
  #recording the predictors of the best model 
}

model.mse.list
model.predictor.list
model.mse.list[which.min(model.mse.list)] #Lowest CV MSE
model.predictor.list[which.min(model.mse.list)] #Predictors of Lowest CV MSE
# > model.mse.list[which.min(model.mse.list)] #Lowest CV MSE
# [1] 0.00289
# > model.predictor.list[which.min(model.mse.list)] #Predictors of Lowest CV MSE
# [1] "x1 x2 x4 invX1 logX1 sqrX1 invX2 rootX2 invX4 logX4 sqrX4 rootX6"
#Lower MSE than the forward selection, and has only 12 variables, which is good. 

#Stepwise/Hybrid Subsetting
s.null <- lm(y~1, data=train.full.randomized3)
s.full <- lm(y~., data=train.full.randomized3)
step(s.null, scope=list(lower=s.null, upper=s.full), direction="both")
lm.step.flex.log <-lm(formula = y ~ sqrX3 + rootX6 + sqrX4 + invX3 + x3 + invX2 + 
                          invX4 + invX1 + sqrX1 + logX3 + sqrX2 + x2 + rootX1 + rootX2 + 
                          x4, data = train.full.randomized3)
#cross validation for stepwise/hybrid subsetting
cv.lm(data=train.full.randomized3, form.lm=lm.step.flex.log, m=5, plotit=F)
#mse = 0.00296, worse than backward, but better than forward. Has 15 variables. 

#reconfirming CV MSE values for forward and backward models.
#forward
lm.forward.flex.log<-lm(y~sqrX3 + rootX6 + sqrX4 + invX3 + x3 + rootX3 + sqrX7 + invX2 + 
                          invX4 + invX1 + sqrX1 + logX3 + x1 + sqrX2 + x2 + rootX1 + rootX2 + logX4,
                        data = train.full.randomized3)
cv.lm(data=train.full.randomized3, form.lm=lm.forward.flex.log, m=5, plotit=F) 
#mse = 0.00305, calculations may be different than the MSE that I got due to the randomizations of the k folds

#backward
lm.backward.flex.log<-lm(y~x1 + x2 + x4 + invX1 + logX1 + sqrX1 + invX2 + rootX2 + invX4 + 
                         logX4 + sqrX4 + rootX6, data = train.full.randomized3)
cv.lm(data=train.full.randomized3, form.lm=lm.backward.flex.log, m=5, plotit=F) 
#mse = 0.00291, calculations may be different than the MSE that I got due to the randomizations of the k folds

#Seems like out of all these, the backward model does the best due to the lowest MSE score AND it has the least number of variables (12)

#Checking the rstudent residuals vs fitted for log(y) to see if transforming y to log(y) got rid of heteroskedasticity
par(mfrow=c(1,1))
backward.flex.log.resid.stu <- rstandard(lm.backward.flex.log)
backward.flex.log.pred <-predict(lm.backward.flex.log, newdata=train.full.randomized3)
plot(backward.flex.log.resid.stu ~ backward.flex.log.pred)
#Still has heteroskedasticity. Let's compare it to the graph before when we did not transform y
#studentized residuals for backward flexible model with regular y and not log(y)
par(mfrow=c(1,1))
backward.flex.resid.stu <- rstandard(lm.backward.flex)
backward.flex.pred <-predict(lm.backward.flex, newdata=train.full.randomized2)
plot(backward.flex.resid.stu ~ backward.flex.pred)

#Although the transformation of y to log(y) did not remove all of heteroskedasticity. It improved it greatly


#transforming log(y) back to y so we can calculate a proper MSE and compare it to the models done before
train.full.randomized4 <-train.full.randomized3
train.full.randomized4$y <- exp(train.full.randomized3$y)
train.full.randomized4$predict <- exp(predict(lm.backward.flex.log, newdata=train.full.randomized3))
log.mse = mean((train.full.randomized4$predict-train.full.randomized4$y)^2)
log.mse
#MSE for this log version is 2.71 

#The MSE of the best candidate model that we had before (the flexible backward selection model on the regular y was 3.28)
#We conclude that the final best model is 
# lm.backward.flex.log<-lm(y~x1 + x2 + x4 + invX1 + logX1 + sqrX1 + invX2 + rootX2 + invX4 + 
#                            logX4 + sqrX4 + rootX6, data = train.full.randomized3)
#NOTE: Where y in the formula is actually log(y)
#Also, based on our univariate analysis and the correlation matrix of the predictor variables,
#final model confirms our assumption that x5 doesn't add value to the model. It's reassuring to see that x2 does exist in the model
#along with at least one of x1 or x4 due to our exploration that x1, x2, and x4 seem to be grouped together. Thus maybe those variables combined indicate a cluster.
#Also good to see that variables x1, x2, and x4 are flexible, but x6 is relatively inflexible (due to one term), based on our univariate analysis.

#Writing predicted values to the vector predvect based on the original testing dataset
test.full2 <- test.full
#transforming all x values for test.full2
#x1 transformations
test.full2$invX1 <- 1/test.full2$x1
test.full2$logX1 <- log(test.full2$x1)
test.full2$sqrX1 <- test.full2$x1^2
test.full2$rootX1 <- test.full2$x1^0.5

#x2 transformations
test.full2$invX2 <- 1/test.full2$x2
test.full2$logX2 <- log(test.full2$x2)
test.full2$sqrX2 <- test.full2$x2^2
test.full2$rootX2 <- test.full2$x2^0.5

#x3 transformations
test.full2$invX3 <- 1/test.full2$x3
test.full2$logX3 <- log(test.full2$x3)
test.full2$sqrX3 <- test.full2$x3^2
test.full2$rootX3 <- test.full2$x3^0.5

#x4 transformations
test.full2$invX4 <- 1/test.full2$x4
test.full2$logX4 <- log(test.full2$x4)
test.full2$sqrX4 <- test.full2$x4^2
test.full2$rootX4 <- test.full2$x4^0.5

#x5 transformations
test.full2$invX5 <- 1/test.full2$x5
test.full2$logX5 <- log(test.full2$x5)
test.full2$sqrX5 <- test.full2$x5^2
test.full2$rootX5 <- test.full2$x5^0.5

#x6 transformations
test.full2$sqrX6 <- test.full2$x6^2
test.full2$rootX6 <- test.full2$x6^0.5

#x7 transformations
test.full2$invX7 <- 1/test.full2$x7
test.full2$logX7 <- log(test.full2$x7)
test.full2$sqrX7 <- test.full2$x7^2
test.full2$rootX7 <- test.full2$x7^0.5

#predicting log(y) on testing dataset
test.full2$logy <- predict(lm.backward.flex.log, newdata=test.full2)
test.full2$y <- exp(test.full2$logy)

#Gut checking to see if the distribution of the predicted y's of the testing set 
#has about the same distribution as the y's of the training set, given we already checked the distributions of the
#x predictor variables are relatively similar to each other in the training/testing data sets.
summary(test.full2$y)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 11.6    15.6    22.1    24.5    31.5    44.0 
summary(train.full$y)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 10.9    15.6    21.8    24.2    31.6    48.0 

#gut check passes, no obvious errors. Saving the predicted values to a vector called predvect

predvect <- test.full2$y
write.table(predvect, file="teamassign05preds.csv", row.names=F, col.names=F, sep=",")
