# Tianye Song, ts7fx, hw8, rfile

####################
#                  #
#   Homework 8     #
#                  #
####################



## Your answers may be submitted as an annotated R file. ##
###########################################################

##############
## Question ##
##############

# For this problem you will use the files "homework08data01.csv", "homework08data02.csv", and "homework08data03.csv" to 
# demonstrate through simulation the issues that can result from using the "traditional" methods to treat missing data. Each
# of the three data sets contains real estate information for 1000 records that detail home selling price (y), 
# home size (x1), and a quality rating (x2). There are 400 missing values in each data set. The missing mechanism is 
# MCAR, MAR, and MNAR, respectively. The true relationship is y = 29.3 + 5.6*x1 + 3.8*x2 + epsilon and the standard 
# deviation of epsilon is 21.
# 
# Follow the procedures below for each of the three data sets.
#
# For each "traditional" method listed below, do the following:
#   (1) Repeat the following process 1000 times. 
#       (a) Take a simple random sample of size 500 from the data set.
#       (b) Treat the missing data according to the particular method.
#       (c) Estimate and record the three regression parameters. 
#       (d) Use the estimated regression parameters to calculate MSE.
#       (e) Determine the 95% confidence interval for each of the regression parameters and 
#           record whether or not it contains the true parameter value.
#   (2) Determine and report the mean and variance of the generated coefficients and the MSE. 
#   (3) Determine and report the coverage of the confidence intervals of the parameters.
#
# The "traditional" methods to use:
#   (1) Listwise deletion
#   (2) Pairwise deletion
#   (3) Arithmetic mean imputation
#   (4) Regression imputation
#   (5) Stochastic regression imputation
#   (6) Hot-Deck imputation
#   (7) Similar resonse pattern imputation
#   (8) Indicator method imputation
#
# Summarize your findings in a table and discuss your observations.
library(mice)
library(hot.deck)
library(VIM)
# load the data:
setwd("~/Documents/git/16FSTAT6021/hw/hw8")
data1<-read.csv('Homework08data01.csv',header = FALSE)
data2<-read.csv('Homework08data02.csv',header = FALSE)
data3<-read.csv('Homework08data03.csv',header = FALSE)
colnames(data1)<-c('price','size','rating')
colnames(data2)<-c('price','size','rating')
colnames(data3)<-c('price','size','rating')

# LISTWISE:
# dataset 1:
b1vec<-vector()
b2vec<-vector()
msevec<-vector()
sizeboolvec<-vector()
ratingboolvec<-vector()
for (i in 1:1000){
  # re-sample every time
  data.sample.temp <- data1[sample.int(1000, size = 500),]
  # listwise, so takeout all missing values
  data.sample.temp <- na.omit(data.sample.temp)
  # re-fit a linear model every time
  lm.sample.temp <- lm(price~., data = data.sample.temp)
  # store betas in ith position in vectors intiated earlier
  b1vec[i] = coef(lm.sample.temp)[2]
  b2vec[i] = coef(lm.sample.temp)[3]
  # predict:
  y.pred.temp <- predict(lm.sample.temp)
  # calculate MSE, store in ith position in vector intiated above
  msevec[i] <- sum((data.sample.temp$price - y.pred.temp)^2) / nrow(data.sample.temp) 
  
  # generate 95% CI of model
  conf <- confint(lm.sample.temp, level=0.95)
  sizeboolvec[i]<- conf[2,1]<5.6 & 5.6<conf[2,2]
  ratingboolvec[i] <- conf[3,1]<3.8 & 3.8<conf[3,2]
  
}
mean.B1 <- mean(b1vec)
mean.B2 <- mean(b2vec)
var.B1 <- var(b1vec) 
var.B2 <- var(b2vec) 
mean.MSE <- mean(msevec) 
var.MSE <- var(msevec)
size.ratio <- sum(sizeboolvec)/1000
rating.ratio <- sum(ratingboolvec)/1000

#data2:
b1vec<-vector()
b2vec<-vector()
msevec<-vector()
sizeboolvec<-vector()
ratingboolvec<-vector()
for (i in 1:1000){
  # re-sample every time
  data.sample.temp <- data2[sample.int(1000, size = 500),]
  # listwise, so takeout all missing values
  data.sample.temp <- na.omit(data.sample.temp)
  # re-fit a linear model every time
  lm.sample.temp <- lm(price~., data = data.sample.temp)
  # store betas in ith position in vectors intiated earlier
  b1vec[i] = coef(lm.sample.temp)[2]
  b2vec[i] = coef(lm.sample.temp)[3]
  # predict:
  y.pred.temp <- predict(lm.sample.temp)
  # calculate MSE, store in ith position in vector intiated above
  msevec[i] <- sum((data.sample.temp$price - y.pred.temp)^2) / nrow(data.sample.temp) 
  
  # generate 95% CI of model
  conf <- confint(lm.sample.temp, level=0.95)
  sizeboolvec[i]<- conf[2,1]<5.6 & 5.6<conf[2,2]
  ratingboolvec[i] <- conf[3,1]<3.8 & 3.8<conf[3,2]
  
}
mean.B1 <- mean(b1vec)
mean.B2 <- mean(b2vec)
var.B1 <- var(b1vec) 
var.B2 <- var(b2vec) 
mean.MSE <- mean(msevec) 
var.MSE <- var(msevec)
size.ratio <- sum(sizeboolvec)/1000
rating.ratio <- sum(ratingboolvec)/1000

#data3:
b1vec<-vector()
b2vec<-vector()
msevec<-vector()
sizeboolvec<-vector()
ratingboolvec<-vector()
for (i in 1:1000){
  # re-sample every time
  data.sample.temp <- data3[sample.int(1000, size = 500),]
  # listwise, so takeout all missing values
  data.sample.temp <- na.omit(data.sample.temp)
  # re-fit a linear model every time
  lm.sample.temp <- lm(price~., data = data.sample.temp)
  # store betas in ith position in vectors intiated earlier
  b1vec[i] = coef(lm.sample.temp)[2]
  b2vec[i] = coef(lm.sample.temp)[3]
  # predict:
  y.pred.temp <- predict(lm.sample.temp)
  # calculate MSE, store in ith position in vector intiated above
  msevec[i] <- sum((data.sample.temp$price - y.pred.temp)^2) / nrow(data.sample.temp) 
  
  # generate 95% CI of model
  conf <- confint(lm.sample.temp, level=0.95)
  sizeboolvec[i]<- conf[2,1]<5.6 & 5.6<conf[2,2]
  ratingboolvec[i] <- conf[3,1]<3.8 & 3.8<conf[3,2]
  
}
mean.B1 <- mean(b1vec)
mean.B2 <- mean(b2vec)
var.B1 <- var(b1vec) 
var.B2 <- var(b2vec) 
mean.MSE <- mean(msevec) 
var.MSE <- var(msevec)
size.ratio <- sum(sizeboolvec)/1000
rating.ratio <- sum(ratingboolvec)/1000



#PAIRWISE:
#data1
b1vec<-vector()
b2vec<-vector()
msevec<-vector()
sizeboolvec<-vector()
ratingboolvec<-vector()
for (i in 1:1000){
  # re-sample every time
  data.sample.temp <- data1[sample.int(1000, size = 500),]
  # pairwise treatment
  covMat <- cov(data.sample.temp, use="pairwise.complete.obs")
  rxy <- covMat[1,c(2,3)]
  R <- covMat[c(2,3),c(2,3)]
  betas <- rxy %*% solve(R)
  beta1 <- betas[1]
  beta2 <- betas[2]
  beta0 <- mean(na.omit(data.sample.temp)[, 1]) - betas[1]*mean(na.omit(data.sample.temp)[, 2]) - betas[2]*mean(na.omit(data.sample.temp)[, 3])
  b1vec[i] <- beta1
  b2vec[i] <- beta2
  betas <- c(beta0, beta1, beta2)
  # predict:
  data.sample.temp <- na.omit(data.sample.temp)
  y.pred.temp <- beta0 + data.sample.temp$size * beta1 + data.sample.temp$rating * beta2
  # calculate MSE, store in ith position in vector intiated above
  msevec[i] <- sum((data.sample.temp$price - y.pred.temp)^2) / nrow(data.sample.temp) 
  
  # generate 95% CI of model
  X <- model.matrix(price ~ ., data.sample.temp)
  var.betaHat <- msevec[i] * solve(t(X) %*% X)
  se.betas <- sqrt(diag(var.betaHat))
  confint.beta1 <- betas[2] + c(-1, 1) * qt(0.975, df = nrow(data.sample.temp) - 3) * se.betas[2]
  sizeboolvec[i] <- confint.beta1[1] <= 5.6 & 5.6 <= confint.beta1[2]
  confint.beta2 <- betas[3] + c(-1, 1) * qt(0.975, df = nrow(data.sample.temp) - 3) * se.betas[3]
  ratingboolvec[i] <- confint.beta2[1] <= 3.8 & 3.8 <= confint.beta2[2]
  
}
mean.B1 <- mean(b1vec)
mean.B2 <- mean(b2vec)
var.B1 <- var(b1vec) 
var.B2 <- var(b2vec) 
mean.MSE <- mean(msevec) 
var.MSE <- var(msevec)
size.ratio <- sum(sizeboolvec)/1000
rating.ratio <- sum(ratingboolvec)/1000

#data2

b1vec<-vector()
b2vec<-vector()
msevec<-vector()
sizeboolvec<-vector()
ratingboolvec<-vector()
for (i in 1:1000){
  # re-sample every time
  data.sample.temp <- data2[sample.int(1000, size = 500),]
  # pairwise treatment
  covMat <- cov(data.sample.temp, use="pairwise.complete.obs")
  rxy <- covMat[1,c(2,3)]
  R <- covMat[c(2,3),c(2,3)]
  betas <- rxy %*% solve(R)
  beta1 <- betas[1]
  beta2 <- betas[2]
  beta0 <- mean(na.omit(data.sample.temp)[, 1]) - betas[1]*mean(na.omit(data.sample.temp)[, 2]) - betas[2]*mean(na.omit(data.sample.temp)[, 3])
  b1vec[i] <- beta1
  b2vec[i] <- beta2
  betas <- c(beta0, beta1, beta2)
  # predict:
  data.sample.temp <- na.omit(data.sample.temp)
  y.pred.temp <- beta0 + data.sample.temp$size * beta1 + data.sample.temp$rating * beta2
  # calculate MSE, store in ith position in vector intiated above
  msevec[i] <- sum((data.sample.temp$price - y.pred.temp)^2) / nrow(data.sample.temp) 
  
  # generate 95% CI of model
  X <- model.matrix(price ~ ., data.sample.temp)
  var.betaHat <- msevec[i] * solve(t(X) %*% X)
  se.betas <- sqrt(diag(var.betaHat))
  confint.beta1 <- betas[2] + c(-1, 1) * qt(0.975, df = nrow(data.sample.temp) - 3) * se.betas[2]
  sizeboolvec[i] <- confint.beta1[1] <= 5.6 & 5.6 <= confint.beta1[2]
  confint.beta2 <- betas[3] + c(-1, 1) * qt(0.975, df = nrow(data.sample.temp) - 3) * se.betas[3]
  ratingboolvec[i] <- confint.beta2[1] <= 3.8 & 3.8 <= confint.beta2[2]
  
}
mean.B1 <- mean(b1vec)
mean.B2 <- mean(b2vec)
var.B1 <- var(b1vec) 
var.B2 <- var(b2vec) 
mean.MSE <- mean(msevec) 
var.MSE <- var(msevec)
size.ratio <- sum(sizeboolvec)/1000
rating.ratio <- sum(ratingboolvec)/1000


#data3
b1vec<-vector()
b2vec<-vector()
msevec<-vector()
sizeboolvec<-vector()
ratingboolvec<-vector()
for (i in 1:1000){
  # re-sample every time
  data.sample.temp <- data3[sample.int(1000, size = 500),]
  # pairwise treatment
  covMat <- cov(data.sample.temp, use="pairwise.complete.obs")
  rxy <- covMat[1,c(2,3)]
  R <- covMat[c(2,3),c(2,3)]
  betas <- rxy %*% solve(R)
  beta1 <- betas[1]
  beta2 <- betas[2]
  beta0 <- mean(na.omit(data.sample.temp)[, 1]) - betas[1]*mean(na.omit(data.sample.temp)[, 2]) - betas[2]*mean(na.omit(data.sample.temp)[, 3])
  b1vec[i] <- beta1
  b2vec[i] <- beta2
  betas <- c(beta0, beta1, beta2)
  # predict:
  data.sample.temp <- na.omit(data.sample.temp)
  y.pred.temp <- beta0 + data.sample.temp$size * beta1 + data.sample.temp$rating * beta2
  # calculate MSE, store in ith position in vector intiated above
  msevec[i] <- sum((data.sample.temp$price - y.pred.temp)^2) / nrow(data.sample.temp) 
  
  # generate 95% CI of model
  X <- model.matrix(price ~ ., data.sample.temp)
  var.betaHat <- msevec[i] * solve(t(X) %*% X)
  se.betas <- sqrt(diag(var.betaHat))
  confint.beta1 <- betas[2] + c(-1, 1) * qt(0.975, df = nrow(data.sample.temp) - 3) * se.betas[2]
  sizeboolvec[i] <- confint.beta1[1] <= 5.6 & 5.6 <= confint.beta1[2]
  confint.beta2 <- betas[3] + c(-1, 1) * qt(0.975, df = nrow(data.sample.temp) - 3) * se.betas[3]
  ratingboolvec[i] <- confint.beta2[1] <= 3.8 & 3.8 <= confint.beta2[2]
  
}
mean.B1 <- mean(b1vec)
mean.B2 <- mean(b2vec)
var.B1 <- var(b1vec) 
var.B2 <- var(b2vec) 
mean.MSE <- mean(msevec) 
var.MSE <- var(msevec)
size.ratio <- sum(sizeboolvec)/1000
rating.ratio <- sum(ratingboolvec)/1000


#Arithmetic mean imputation

#data1:
b1vec<-vector()
b2vec<-vector()
msevec<-vector()
sizeboolvec<-vector()
ratingboolvec<-vector()
for (i in 1:1000){
  # re-sample every time
  data.sample.temp <- data1[sample.int(1000, size = 500),]
  ymean <- mean(data.sample.temp$price[!is.na(data.sample.temp$price)])
  x1mean <- mean(data.sample.temp$size[!is.na(data.sample.temp$size)])
  x2mean <- mean(data.sample.temp$rating[!is.na(data.sample.temp$rating)])
  data.sample.temp[is.na(data.sample.temp$price), 1] <- ymean
  data.sample.temp[is.na(data.sample.temp$size), 2] <- x1mean
  data.sample.temp[is.na(data.sample.temp$rating), 3] <- x2mean
  # re-fit a linear model every time
  lm.sample.temp <- lm(price~., data = data.sample.temp)
  # store betas in ith position in vectors intiated earlier
  b1vec[i] = coef(lm.sample.temp)[2]
  b2vec[i] = coef(lm.sample.temp)[3]
  # predict:
  y.pred.temp <- predict(lm.sample.temp)
  # calculate MSE, store in ith position in vector intiated above
  msevec[i] <- sum((data.sample.temp$price - y.pred.temp)^2) / nrow(data.sample.temp) 
  
  # generate 95% CI of model
  conf <- confint(lm.sample.temp, level=0.95)
  sizeboolvec[i]<- conf[2,1]<5.6 & 5.6<conf[2,2]
  ratingboolvec[i] <- conf[3,1]<3.8 & 3.8<conf[3,2]
  
}
mean.B1 <- mean(b1vec)
mean.B2 <- mean(b2vec)
var.B1 <- var(b1vec) 
var.B2 <- var(b2vec) 
mean.MSE <- mean(msevec) 
var.MSE <- var(msevec)
size.ratio <- sum(sizeboolvec)/1000
rating.ratio <- sum(ratingboolvec)/1000


#data2
b1vec<-vector()
b2vec<-vector()
msevec<-vector()
sizeboolvec<-vector()
ratingboolvec<-vector()
for (i in 1:1000){
  # re-sample every time
  data.sample.temp <- data2[sample.int(1000, size = 500),]
  ymean <- mean(data.sample.temp$price[!is.na(data.sample.temp$price)])
  x1mean <- mean(data.sample.temp$size[!is.na(data.sample.temp$size)])
  x2mean <- mean(data.sample.temp$rating[!is.na(data.sample.temp$rating)])
  data.sample.temp[is.na(data.sample.temp$price), 1] <- ymean
  data.sample.temp[is.na(data.sample.temp$size), 2] <- x1mean
  data.sample.temp[is.na(data.sample.temp$rating), 3] <- x2mean
  # re-fit a linear model every time
  lm.sample.temp <- lm(price~., data = data.sample.temp)
  # store betas in ith position in vectors intiated earlier
  b1vec[i] = coef(lm.sample.temp)[2]
  b2vec[i] = coef(lm.sample.temp)[3]
  # predict:
  y.pred.temp <- predict(lm.sample.temp)
  # calculate MSE, store in ith position in vector intiated above
  msevec[i] <- sum((data.sample.temp$price - y.pred.temp)^2) / nrow(data.sample.temp) 
  
  # generate 95% CI of model
  conf <- confint(lm.sample.temp, level=0.95)
  sizeboolvec[i]<- conf[2,1]<5.6 & 5.6<conf[2,2]
  ratingboolvec[i] <- conf[3,1]<3.8 & 3.8<conf[3,2]
  
}
mean.B1 <- mean(b1vec)
mean.B2 <- mean(b2vec)
var.B1 <- var(b1vec) 
var.B2 <- var(b2vec) 
mean.MSE <- mean(msevec) 
var.MSE <- var(msevec)
size.ratio <- sum(sizeboolvec)/1000
rating.ratio <- sum(ratingboolvec)/1000


#data3
b1vec<-vector()
b2vec<-vector()
msevec<-vector()
sizeboolvec<-vector()
ratingboolvec<-vector()
for (i in 1:1000){
  # re-sample every time
  data.sample.temp <- data3[sample.int(1000, size = 500),]
  ymean <- mean(data.sample.temp$price[!is.na(data.sample.temp$price)])
  x1mean <- mean(data.sample.temp$size[!is.na(data.sample.temp$size)])
  x2mean <- mean(data.sample.temp$rating[!is.na(data.sample.temp$rating)])
  data.sample.temp[is.na(data.sample.temp$price), 1] <- ymean
  data.sample.temp[is.na(data.sample.temp$size), 2] <- x1mean
  data.sample.temp[is.na(data.sample.temp$rating), 3] <- x2mean
  # re-fit a linear model every time
  lm.sample.temp <- lm(price~., data = data.sample.temp)
  # store betas in ith position in vectors intiated earlier
  b1vec[i] = coef(lm.sample.temp)[2]
  b2vec[i] = coef(lm.sample.temp)[3]
  # predict:
  y.pred.temp <- predict(lm.sample.temp)
  # calculate MSE, store in ith position in vector intiated above
  msevec[i] <- sum((data.sample.temp$price - y.pred.temp)^2) / nrow(data.sample.temp) 
  
  # generate 95% CI of model
  conf <- confint(lm.sample.temp, level=0.95)
  sizeboolvec[i]<- conf[2,1]<5.6 & 5.6<conf[2,2]
  ratingboolvec[i] <- conf[3,1]<3.8 & 3.8<conf[3,2]
  
}
mean.B1 <- mean(b1vec)
mean.B2 <- mean(b2vec)
var.B1 <- var(b1vec) 
var.B2 <- var(b2vec) 
mean.MSE <- mean(msevec) 
var.MSE <- var(msevec)
size.ratio <- sum(sizeboolvec)/1000
rating.ratio <- sum(ratingboolvec)/1000




# regression imputation
# data1
b1vec<-vector()
b2vec<-vector()
msevec<-vector()
sizeboolvec<-vector()
ratingboolvec<-vector()
for (i in 1:1000){
  # re-sample every time
  data.sample.temp <- data1[sample.int(1000, size = 500),]
  imp <- mice(data.sample.temp, method = "norm.predict", m = 1)
  data.sample.temp <- complete(imp)
  # re-fit a linear model every time
  lm.sample.temp <- lm(price~., data = data.sample.temp)
  # store betas in ith position in vectors intiated earlier
  b1vec[i] = coef(lm.sample.temp)[2]
  b2vec[i] = coef(lm.sample.temp)[3]
  # predict:
  y.pred.temp <- predict(lm.sample.temp)
  # calculate MSE, store in ith position in vector intiated above
  msevec[i] <- sum((data.sample.temp$price - y.pred.temp)^2) / nrow(data.sample.temp) 
  
  # generate 95% CI of model
  conf <- confint(lm.sample.temp, level=0.95)
  sizeboolvec[i]<- conf[2,1]<5.6 & 5.6<conf[2,2]
  ratingboolvec[i] <- conf[3,1]<3.8 & 3.8<conf[3,2]
  
}
mean.B1 <- mean(b1vec)
mean.B2 <- mean(b2vec)
var.B1 <- var(b1vec) 
var.B2 <- var(b2vec) 
mean.MSE <- mean(msevec) 
var.MSE <- var(msevec)
size.ratio <- sum(sizeboolvec)/1000
rating.ratio <- sum(ratingboolvec)/1000


#data2
b1vec<-vector()
b2vec<-vector()
msevec<-vector()
sizeboolvec<-vector()
ratingboolvec<-vector()
for (i in 1:1000){
  # re-sample every time
  data.sample.temp <- data2[sample.int(1000, size = 500),]
  imp <- mice(data.sample.temp, method = "norm.predict", m = 1)
  data.sample.temp <- complete(imp)
  # re-fit a linear model every time
  lm.sample.temp <- lm(price~., data = data.sample.temp)
  # store betas in ith position in vectors intiated earlier
  b1vec[i] = coef(lm.sample.temp)[2]
  b2vec[i] = coef(lm.sample.temp)[3]
  # predict:
  y.pred.temp <- predict(lm.sample.temp)
  # calculate MSE, store in ith position in vector intiated above
  msevec[i] <- sum((data.sample.temp$price - y.pred.temp)^2) / nrow(data.sample.temp) 
  
  # generate 95% CI of model
  conf <- confint(lm.sample.temp, level=0.95)
  sizeboolvec[i]<- conf[2,1]<5.6 & 5.6<conf[2,2]
  ratingboolvec[i] <- conf[3,1]<3.8 & 3.8<conf[3,2]
  
}
mean.B1 <- mean(b1vec)
mean.B2 <- mean(b2vec)
var.B1 <- var(b1vec) 
var.B2 <- var(b2vec) 
mean.MSE <- mean(msevec) 
var.MSE <- var(msevec)
size.ratio <- sum(sizeboolvec)/1000
rating.ratio <- sum(ratingboolvec)/1000



#data3
b1vec<-vector()
b2vec<-vector()
msevec<-vector()
sizeboolvec<-vector()
ratingboolvec<-vector()
for (i in 1:1000){
  # re-sample every time
  data.sample.temp <- data3[sample.int(1000, size = 500),]
  imp <- mice(data.sample.temp, method = "norm.predict", m = 1)
  data.sample.temp <- complete(imp)
  # re-fit a linear model every time
  lm.sample.temp <- lm(price~., data = data.sample.temp)
  # store betas in ith position in vectors intiated earlier
  b1vec[i] = coef(lm.sample.temp)[2]
  b2vec[i] = coef(lm.sample.temp)[3]
  # predict:
  y.pred.temp <- predict(lm.sample.temp)
  # calculate MSE, store in ith position in vector intiated above
  msevec[i] <- sum((data.sample.temp$price - y.pred.temp)^2) / nrow(data.sample.temp) 
  
  # generate 95% CI of model
  conf <- confint(lm.sample.temp, level=0.95)
  sizeboolvec[i]<- conf[2,1]<5.6 & 5.6<conf[2,2]
  ratingboolvec[i] <- conf[3,1]<3.8 & 3.8<conf[3,2]
  
}
mean.B1 <- mean(b1vec)
mean.B2 <- mean(b2vec)
var.B1 <- var(b1vec) 
var.B2 <- var(b2vec) 
mean.MSE <- mean(msevec) 
var.MSE <- var(msevec)
size.ratio <- sum(sizeboolvec)/1000
rating.ratio <- sum(ratingboolvec)/1000


#stochastic regression imputation
#data1:
b1vec<-vector()
b2vec<-vector()
msevec<-vector()
sizeboolvec<-vector()
ratingboolvec<-vector()
for (i in 1:1000){
  # re-sample every time
  data.sample.temp <- data1[sample.int(1000, size = 500),]
  imp <- mice(data.sample.temp, method = "norm.nob", m = 1)
  data.sample.temp <- complete(imp)
  # re-fit a linear model every time
  lm.sample.temp <- lm(price~., data = data.sample.temp)
  # store betas in ith position in vectors intiated earlier
  b1vec[i] = coef(lm.sample.temp)[2]
  b2vec[i] = coef(lm.sample.temp)[3]
  # predict:
  y.pred.temp <- predict(lm.sample.temp)
  # calculate MSE, store in ith position in vector intiated above
  msevec[i] <- sum((data.sample.temp$price - y.pred.temp)^2) / nrow(data.sample.temp) 
  
  # generate 95% CI of model
  conf <- confint(lm.sample.temp, level=0.95)
  sizeboolvec[i]<- conf[2,1]<5.6 & 5.6<conf[2,2]
  ratingboolvec[i] <- conf[3,1]<3.8 & 3.8<conf[3,2]
  
}
mean.B1 <- mean(b1vec)
mean.B2 <- mean(b2vec)
var.B1 <- var(b1vec) 
var.B2 <- var(b2vec) 
mean.MSE <- mean(msevec) 
var.MSE <- var(msevec)
size.ratio <- sum(sizeboolvec)/1000
rating.ratio <- sum(ratingboolvec)/1000


#data2:
b1vec<-vector()
b2vec<-vector()
msevec<-vector()
sizeboolvec<-vector()
ratingboolvec<-vector()
for (i in 1:1000){
  # re-sample every time
  data.sample.temp <- data2[sample.int(1000, size = 500),]
  imp <- mice(data.sample.temp, method = "norm.nob", m = 1)
  data.sample.temp <- complete(imp)
  # re-fit a linear model every time
  lm.sample.temp <- lm(price~., data = data.sample.temp)
  # store betas in ith position in vectors intiated earlier
  b1vec[i] = coef(lm.sample.temp)[2]
  b2vec[i] = coef(lm.sample.temp)[3]
  # predict:
  y.pred.temp <- predict(lm.sample.temp)
  # calculate MSE, store in ith position in vector intiated above
  msevec[i] <- sum((data.sample.temp$price - y.pred.temp)^2) / nrow(data.sample.temp) 
  
  # generate 95% CI of model
  conf <- confint(lm.sample.temp, level=0.95)
  sizeboolvec[i]<- conf[2,1]<5.6 & 5.6<conf[2,2]
  ratingboolvec[i] <- conf[3,1]<3.8 & 3.8<conf[3,2]
  
}
mean.B1 <- mean(b1vec)
mean.B2 <- mean(b2vec)
var.B1 <- var(b1vec) 
var.B2 <- var(b2vec) 
mean.MSE <- mean(msevec) 
var.MSE <- var(msevec)
size.ratio <- sum(sizeboolvec)/1000
rating.ratio <- sum(ratingboolvec)/1000

#data3:
b1vec<-vector()
b2vec<-vector()
msevec<-vector()
sizeboolvec<-vector()
ratingboolvec<-vector()
for (i in 1:1000){
  # re-sample every time
  data.sample.temp <- data3[sample.int(1000, size = 500),]
  imp <- mice(data.sample.temp, method = "norm.nob", m = 1)
  data.sample.temp <- complete(imp)
  # re-fit a linear model every time
  lm.sample.temp <- lm(price~., data = data.sample.temp)
  # store betas in ith position in vectors intiated earlier
  b1vec[i] = coef(lm.sample.temp)[2]
  b2vec[i] = coef(lm.sample.temp)[3]
  # predict:
  y.pred.temp <- predict(lm.sample.temp)
  # calculate MSE, store in ith position in vector intiated above
  msevec[i] <- sum((data.sample.temp$price - y.pred.temp)^2) / nrow(data.sample.temp) 
  
  # generate 95% CI of model
  conf <- confint(lm.sample.temp, level=0.95)
  sizeboolvec[i]<- conf[2,1]<5.6 & 5.6<conf[2,2]
  ratingboolvec[i] <- conf[3,1]<3.8 & 3.8<conf[3,2]
  
}
mean.B1 <- mean(b1vec)
mean.B2 <- mean(b2vec)
var.B1 <- var(b1vec) 
var.B2 <- var(b2vec) 
mean.MSE <- mean(msevec) 
var.MSE <- var(msevec)
size.ratio <- sum(sizeboolvec)/1000
rating.ratio <- sum(ratingboolvec)/1000


#hot deck imputation
#data1
b1vec<-vector()
b2vec<-vector()
msevec<-vector()
sizeboolvec<-vector()
ratingboolvec<-vector()
for (i in 1:1000){
  # re-sample every time
  data.sample.temp <- data1[sample.int(1000, size = 500),]
  temp<-hot.deck(data.sample.temp,m=1)
  data.sample.temp <- as.data.frame(temp[1])
  colnames(data.sample.temp) <- c('price','size','rating')
  # re-fit a linear model every time
  lm.sample.temp <- lm(price~., data = data.sample.temp)
  # store betas in ith position in vectors intiated earlier
  b1vec[i] = coef(lm.sample.temp)[2]
  b2vec[i] = coef(lm.sample.temp)[3]
  # predict:
  y.pred.temp <- predict(lm.sample.temp)
  # calculate MSE, store in ith position in vector intiated above
  msevec[i] <- sum((data.sample.temp$price - y.pred.temp)^2) / nrow(data.sample.temp) 
  
  # generate 95% CI of model
  conf <- confint(lm.sample.temp, level=0.95)
  sizeboolvec[i]<- conf[2,1]<5.6 & 5.6<conf[2,2]
  ratingboolvec[i] <- conf[3,1]<3.8 & 3.8<conf[3,2]
  
}
mean.B1 <- mean(b1vec)
mean.B2 <- mean(b2vec)
var.B1 <- var(b1vec) 
var.B2 <- var(b2vec) 
mean.MSE <- mean(msevec) 
var.MSE <- var(msevec)
size.ratio <- sum(sizeboolvec)/1000
rating.ratio <- sum(ratingboolvec)/1000
#data2
b1vec<-vector()
b2vec<-vector()
msevec<-vector()
sizeboolvec<-vector()
ratingboolvec<-vector()
for (i in 1:1000){
  # re-sample every time
  data.sample.temp <- data2[sample.int(1000, size = 500),]
  temp<-hot.deck(data.sample.temp,m=1)
  data.sample.temp <- as.data.frame(temp[1])
  colnames(data.sample.temp) <- c('price','size','rating')
  # re-fit a linear model every time
  lm.sample.temp <- lm(price~., data = data.sample.temp)
  # store betas in ith position in vectors intiated earlier
  b1vec[i] = coef(lm.sample.temp)[2]
  b2vec[i] = coef(lm.sample.temp)[3]
  # predict:
  y.pred.temp <- predict(lm.sample.temp)
  # calculate MSE, store in ith position in vector intiated above
  msevec[i] <- sum((data.sample.temp$price - y.pred.temp)^2) / nrow(data.sample.temp) 
  
  # generate 95% CI of model
  conf <- confint(lm.sample.temp, level=0.95)
  sizeboolvec[i]<- conf[2,1]<5.6 & 5.6<conf[2,2]
  ratingboolvec[i] <- conf[3,1]<3.8 & 3.8<conf[3,2]
  
}
mean.B1 <- mean(b1vec)
mean.B2 <- mean(b2vec)
var.B1 <- var(b1vec) 
var.B2 <- var(b2vec) 
mean.MSE <- mean(msevec) 
var.MSE <- var(msevec)
size.ratio <- sum(sizeboolvec)/1000
rating.ratio <- sum(ratingboolvec)/1000
#data3
b1vec<-vector()
b2vec<-vector()
msevec<-vector()
sizeboolvec<-vector()
ratingboolvec<-vector()
for (i in 1:1000){
  # re-sample every time
  data.sample.temp <- data3[sample.int(1000, size = 500),]
  temp<-hot.deck(data.sample.temp,m=1)
  data.sample.temp <- as.data.frame(temp[1])
  colnames(data.sample.temp) <- c('price','size','rating')
  # re-fit a linear model every time
  lm.sample.temp <- lm(price~., data = data.sample.temp)
  # store betas in ith position in vectors intiated earlier
  b1vec[i] = coef(lm.sample.temp)[2]
  b2vec[i] = coef(lm.sample.temp)[3]
  # predict:
  y.pred.temp <- predict(lm.sample.temp)
  # calculate MSE, store in ith position in vector intiated above
  msevec[i] <- sum((data.sample.temp$price - y.pred.temp)^2) / nrow(data.sample.temp) 
  
  # generate 95% CI of model
  conf <- confint(lm.sample.temp, level=0.95)
  sizeboolvec[i]<- conf[2,1]<5.6 & 5.6<conf[2,2]
  ratingboolvec[i] <- conf[3,1]<3.8 & 3.8<conf[3,2]
  
}
mean.B1 <- mean(b1vec)
mean.B2 <- mean(b2vec)
var.B1 <- var(b1vec) 
var.B2 <- var(b2vec) 
mean.MSE <- mean(msevec) 
var.MSE <- var(msevec)
size.ratio <- sum(sizeboolvec)/1000
rating.ratio <- sum(ratingboolvec)/1000




# indicator method imputation
# data1
b1vec<-vector()
b2vec<-vector()
msevec<-vector()
sizeboolvec<-vector()
ratingboolvec<-vector()
for (i in 1:1000){
  # re-sample every time
  data.sample.temp <- data1[sample.int(1000, size = 500),]
  imp <- mice(data.sample.temp, method = "ri", m = 1)
  data.sample.temp <- complete(imp)
  # re-fit a linear model every time
  lm.sample.temp <- lm(price~., data = data.sample.temp)
  # store betas in ith position in vectors intiated earlier
  b1vec[i] = coef(lm.sample.temp)[2]
  b2vec[i] = coef(lm.sample.temp)[3]
  # predict:
  y.pred.temp <- predict(lm.sample.temp)
  # calculate MSE, store in ith position in vector intiated above
  msevec[i] <- sum((data.sample.temp$price - y.pred.temp)^2) / nrow(data.sample.temp) 
  
  # generate 95% CI of model
  conf <- confint(lm.sample.temp, level=0.95)
  sizeboolvec[i]<- conf[2,1]<5.6 & 5.6<conf[2,2]
  ratingboolvec[i] <- conf[3,1]<3.8 & 3.8<conf[3,2]
  
}
mean.B1 <- mean(b1vec)
mean.B2 <- mean(b2vec)
var.B1 <- var(b1vec) 
var.B2 <- var(b2vec) 
mean.MSE <- mean(msevec) 
var.MSE <- var(msevec)
size.ratio <- sum(sizeboolvec)/1000
rating.ratio <- sum(ratingboolvec)/1000

# data2
b1vec<-vector()
b2vec<-vector()
msevec<-vector()
sizeboolvec<-vector()
ratingboolvec<-vector()
for (i in 1:1000){
  # re-sample every time
  data.sample.temp <- data2[sample.int(1000, size = 500),]
  imp <- mice(data.sample.temp, method = "ri", m = 1)
  data.sample.temp <- complete(imp)
  # re-fit a linear model every time
  lm.sample.temp <- lm(price~., data = data.sample.temp)
  # store betas in ith position in vectors intiated earlier
  b1vec[i] = coef(lm.sample.temp)[2]
  b2vec[i] = coef(lm.sample.temp)[3]
  # predict:
  y.pred.temp <- predict(lm.sample.temp)
  # calculate MSE, store in ith position in vector intiated above
  msevec[i] <- sum((data.sample.temp$price - y.pred.temp)^2) / nrow(data.sample.temp) 
  
  # generate 95% CI of model
  conf <- confint(lm.sample.temp, level=0.95)
  sizeboolvec[i]<- conf[2,1]<5.6 & 5.6<conf[2,2]
  ratingboolvec[i] <- conf[3,1]<3.8 & 3.8<conf[3,2]
  
}
mean.B1 <- mean(b1vec)
mean.B2 <- mean(b2vec)
var.B1 <- var(b1vec) 
var.B2 <- var(b2vec) 
mean.MSE <- mean(msevec) 
var.MSE <- var(msevec)
size.ratio <- sum(sizeboolvec)/1000
rating.ratio <- sum(ratingboolvec)/1000

# data3
b1vec<-vector()
b2vec<-vector()
msevec<-vector()
sizeboolvec<-vector()
ratingboolvec<-vector()
for (i in 1:1000){
  # re-sample every time
  data.sample.temp <- data3[sample.int(1000, size = 500),]
  imp <- mice(data.sample.temp, method = "ri", m = 1)
  data.sample.temp <- complete(imp)
  # re-fit a linear model every time
  lm.sample.temp <- lm(price~., data = data.sample.temp)
  # store betas in ith position in vectors intiated earlier
  b1vec[i] = coef(lm.sample.temp)[2]
  b2vec[i] = coef(lm.sample.temp)[3]
  # predict:
  y.pred.temp <- predict(lm.sample.temp)
  # calculate MSE, store in ith position in vector intiated above
  msevec[i] <- sum((data.sample.temp$price - y.pred.temp)^2) / nrow(data.sample.temp) 
  
  # generate 95% CI of model
  conf <- confint(lm.sample.temp, level=0.95)
  sizeboolvec[i]<- conf[2,1]<5.6 & 5.6<conf[2,2]
  ratingboolvec[i] <- conf[3,1]<3.8 & 3.8<conf[3,2]
  
}
mean.B1 <- mean(b1vec)
mean.B2 <- mean(b2vec)
var.B1 <- var(b1vec) 
var.B2 <- var(b2vec) 
mean.MSE <- mean(msevec) 
var.MSE <- var(msevec)
size.ratio <- sum(sizeboolvec)/1000
rating.ratio <- sum(ratingboolvec)/1000


#Similar response pattern imputation
#data1
b1vec<-vector()
b2vec<-vector()
msevec<-vector()
sizeboolvec<-vector()
ratingboolvec<-vector()
for (i in 1:1000){
  # re-sample every time
  data.sample.temp <- data1[sample.int(1000, size = 500),]
  data.sample.temp <- kNN(data.sample.temp)
  # re-fit a linear model every time
  lm.sample.temp <- lm(price~size+rating, data = data.sample.temp)
  # store betas in ith position in vectors intiated earlier
  b1vec[i] = coef(lm.sample.temp)[2]
  b2vec[i] = coef(lm.sample.temp)[3]
  # predict:
  y.pred.temp <- predict(lm.sample.temp)
  # calculate MSE, store in ith position in vector intiated above
  msevec[i] <- sum((data.sample.temp$price - y.pred.temp)^2) / nrow(data.sample.temp) 
  
  # generate 95% CI of model
  conf <- confint(lm.sample.temp, level=0.95)
  sizeboolvec[i]<- conf[2,1]<5.6 & 5.6<conf[2,2]
  ratingboolvec[i] <- conf[3,1]<3.8 & 3.8<conf[3,2]
  
}
mean.B1 <- mean(b1vec)
mean.B2 <- mean(b2vec)
var.B1 <- var(b1vec) 
var.B2 <- var(b2vec) 
mean.MSE <- mean(msevec) 
var.MSE <- var(msevec)
size.ratio <- sum(sizeboolvec)/1000
rating.ratio <- sum(ratingboolvec)/1000


#data2
b1vec<-vector()
b2vec<-vector()
msevec<-vector()
sizeboolvec<-vector()
ratingboolvec<-vector()
for (i in 1:1000){
  # re-sample every time
  data.sample.temp <- data2[sample.int(1000, size = 500),]
  data.sample.temp <- kNN(data.sample.temp)
  # re-fit a linear model every time
  lm.sample.temp <- lm(price~size+rating, data = data.sample.temp)
  # store betas in ith position in vectors intiated earlier
  b1vec[i] = coef(lm.sample.temp)[2]
  b2vec[i] = coef(lm.sample.temp)[3]
  # predict:
  y.pred.temp <- predict(lm.sample.temp)
  # calculate MSE, store in ith position in vector intiated above
  msevec[i] <- sum((data.sample.temp$price - y.pred.temp)^2) / nrow(data.sample.temp) 
  
  # generate 95% CI of model
  conf <- confint(lm.sample.temp, level=0.95)
  sizeboolvec[i]<- conf[2,1]<5.6 & 5.6<conf[2,2]
  ratingboolvec[i] <- conf[3,1]<3.8 & 3.8<conf[3,2]
  
}
mean.B1 <- mean(b1vec)
mean.B2 <- mean(b2vec)
var.B1 <- var(b1vec) 
var.B2 <- var(b2vec) 
mean.MSE <- mean(msevec) 
var.MSE <- var(msevec)
size.ratio <- sum(sizeboolvec)/1000
rating.ratio <- sum(ratingboolvec)/1000

#data3
b1vec<-vector()
b2vec<-vector()
msevec<-vector()
sizeboolvec<-vector()
ratingboolvec<-vector()
for (i in 1:1000){
  # re-sample every time
  data.sample.temp <- data3[sample.int(1000, size = 500),]
  data.sample.temp <- kNN(data.sample.temp)
  # re-fit a linear model every time
  lm.sample.temp <- lm(price~size+rating, data = data.sample.temp)
  # store betas in ith position in vectors intiated earlier
  b1vec[i] = coef(lm.sample.temp)[2]
  b2vec[i] = coef(lm.sample.temp)[3]
  # predict:
  y.pred.temp <- predict(lm.sample.temp)
  # calculate MSE, store in ith position in vector intiated above
  msevec[i] <- sum((data.sample.temp$price - y.pred.temp)^2) / nrow(data.sample.temp) 
  
  # generate 95% CI of model
  conf <- confint(lm.sample.temp, level=0.95)
  sizeboolvec[i]<- conf[2,1]<5.6 & 5.6<conf[2,2]
  ratingboolvec[i] <- conf[3,1]<3.8 & 3.8<conf[3,2]
  
}
mean.B1 <- mean(b1vec)
mean.B2 <- mean(b2vec)
var.B1 <- var(b1vec) 
var.B2 <- var(b2vec) 
mean.MSE <- mean(msevec) 
var.MSE <- var(msevec)
size.ratio <- sum(sizeboolvec)/1000
rating.ratio <- sum(ratingboolvec)/1000