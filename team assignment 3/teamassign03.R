
###########################
#                         #
#   Team Assignment 3     #
#                         #
###########################

## Please submit one set of answers per team.                  ##
## Your answers may be submitted as an annotated R file.       ##
## Please submit your plots in a PDF as a separate attachment. ##
#################################################################

library(gdata)
library(MASS)
#################
## Question 1: ##
#################

# For this problem you will use the files "teamassign03data01.csv" and "teamassign03data02.csv" to 
# demonstrate through simulation the effects of multicollinearity on the variance of the regression 
# coefficients and how they influence the accuracy of predictions.
data01<- read.csv("teamassign03data01.csv")
data02<- read.csv("teamassign03data02.csv")
#
#   (a) Repeat the following  1000 times:
#       (1) Select a random sample of 100 observations from data01.
data01.sample <- data01[sample(c(1:nrow(data01)),100), ]
#       (2) Fit a linear model to the 100 observations using all four variables. Save the values
#           of the estimated coefficients in separate vectors.
lm.data01.sample <- lm(y~., data = data01.sample)
summary(lm.data01.sample)
# Store Betas in separate vectors
B1 <- coef(lm.data01.sample)[2]
B2 <- coef(lm.data01.sample)[3]
B3 <- coef(lm.data01.sample)[4]
B4 <- coef(lm.data01.sample)[5]
#       (3) Use your linear model to predict the y-values given in data02 then compute the MSE
#           using these residuals. Save this value in a vector.
# pedicted y-values using data02 
y.pred <- predict(lm.data01.sample, newdata = data02)
# plug in formula for calculating MSE
MSE <- sum((data02$y - y.pred)^2) / 195 

#       (4) Compute the standard deviation for the vectors containing the coefficients and compute
#           the mean of the vector containing the MSEs. Record these values.

#now let's repeat this process for 1000 times:

#but before we do this, let's do some vector initiation
b1vec<-vector()
b2vec<-vector()
b3vec<-vector()
b4vec<-vector()
msevec<-vector()
for (i in 1:1000){
  # re-sample every time
  data01.sample.temp <- data01[sample(c(1:nrow(data01)),100), ]
  # re-fit a linear model every time
  lm.sample.temp <- lm(y~., data = data01.sample.temp)
  # store betas in ith position in vectors intiated earlier
  b1vec[i] = coef(lm.sample.temp)[2]
  b2vec[i] = coef(lm.sample.temp)[3]
  b3vec[i] = coef(lm.sample.temp)[4]
  b4vec[i] = coef(lm.sample.temp)[5]
  # predict:
  y.pred.temp <- predict(lm.sample.temp, newdata = data02)
  # calculate MSE, store in ith position in vector intiated above
  msevec[i] <- sum((data02$y - y.pred.temp)^2) / 195 

}

sd.B1 <- sd(b1vec) #3.507392
sd.B2 <- sd(b2vec) #3.519516
sd.B3 <- sd(b3vec) #3.509577
sd.B4 <- sd(b4vec) #0.1357072
mean.MSE <- mean(msevec) #410.9689

#   (b) Choose a suitable variable to remove from the model. Repeat (1)-(4) given in part (a)
#       1000 times using this model.

# utilizing the code written in part A, 
# We ran a random sample of 100 observations. We fitted a linear model and took a look at the summary statistics. 
# The summary statistics are as follows:

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   3.6861     6.8539   0.538    0.592    
# x1           -1.8380     2.9590  -0.621    0.536    
# x2           -0.9823     2.9663  -0.331    0.741    
# x3            3.9275     2.9605   1.327    0.188    
# x4            2.8630     0.1066  26.854   <2e-16 ***

# since x2 has the largest p-value, we took it out of our model.

# we re-ran the model, without x2, for another 1000 times.
b1vec.b<-vector()
b3vec.b<-vector()
b4vec.b<-vector()
msevec.b<-vector()
for (i in 1:1000){
  # re-sample every time
  data01.sample.temp <- data01[sample(c(1:nrow(data01)),100), ]
  # re-fit a linear model every time without x2
  lm.sample.temp <- lm(y~.-x2, data = data01.sample.temp)
  # store betas in ith position in vectors intiated earlier
  b1vec.b[i] = coef(lm.sample.temp)[2]
  b3vec.b[i] = coef(lm.sample.temp)[3]
  b4vec.b[i] = coef(lm.sample.temp)[4]
  # predict:
  y.pred.temp <- predict(lm.sample.temp, newdata = data02)
  # calculate MSE, store in ith position in vector intiated above
  msevec.b[i] <- sum((data02$y - y.pred.temp)^2) / 195 
}

sd.B1.b <- sd(b1vec.b) #0.1937136
sd.B3.b <- sd(b3vec.b) #0.1372232
sd.B4.b <- sd(b4vec.b) #0.1345685
mean.MSE.b <- mean(msevec.b) #408.7249

#   (c) How do the results from parts (a)(4) and (b)(4) compare? Explain what you observe.

# after we removed x2 from our linear model, the standard deviations for beta1 and beta3 decreased drastically. 
# on the other hand, the mean MSE and standard deviation for beta 4 did not vary much. 

# we conclude that because x1 & x3 became significant after the removal of x2, the standard deviation for coefficient for 
# x1 & x3 had to decrease to obtain a small p-value.




#################
## Question 2: ##
#################

# For this problem you will use the file "data-table-B2.XLS".
#
#   (a) Fit the model using all explanatory variables. Iteratively remove insignificant variables
#       one-by-one until the all remaining variables are significant. Which variables remain in your model?
data.Q2<- read.xls("data-table-B2.XLS")
summary(lm(y~. ,data = data.Q2)) # remove x5
summary(lm(y~. -x5,data = data.Q2)) # remove x1
summary(lm(y~. -x5-x1,data = data.Q2)) # remove x2
summary(lm(y~. -x5-x1-x2,data = data.Q2)) # everything is significant
lm.final <- lm(y~. -x5-x1-x2,data = data.Q2)

#   (b) Compute each of the five types of residuals discussed in the textbook:
#       Residuals; Standardized residuals; Studentized residuals; PRESS residuals; R-student residuals.
#       You may use R functions.
## Find the residuals
resid(lm.final)
## Find the standardized residuals
stdres(lm.final)
## Find the studentized residuals
rstandard(lm.final)
## Find the PRESS residuals
PRESS <- residuals(lm.final)/(1 - lm.influence(lm.final)$hat)
## Find the R-student residuals
rstudent(lm.final)

#   (c) Use the results from part(b) to decide if there appear to be any outliers and/or high 
#       influence points.
par(mfrow=c(3,2))

plot(lm.final$fitted.values,resid(lm.final),main="residuals")
plot(lm.final$fitted.values,stdres(lm.final),main="standardized residuals")
plot(lm.final$fitted.values,rstandard(lm.final),main="studentized residuals")
plot(lm.final$fitted.values,PRESS,main="PRESS residuals")
plot(lm.final$fitted.values,rstudent(lm.final),main="R-student residuals")
# we plotted graphs and located one resdual point that appeared to be an outlier. 
# All of the graphs demonstrated similar pattern
# we then identified that residual by finding maximum value of absolute value of all residual points.
max(abs(resid(lm.final)))


# we then plotted boxplots for all residuals and in all of the box plots, the maximum point stood out.
# this further strengthened our argument that the outlier is the residual point with the maximum value.
boxplot(resid(lm.final),main="residuals")
boxplot(stdres(lm.final),main="standardized residuals")
boxplot(rstandard(lm.final),main="studentized residuals")
boxplot(PRESS,main="PRESS residuals")
boxplot(rstudent(lm.final), main="R-student residuals")


#   (d) Produce a normal probability plot of the R-student residuals and evaluate the plot for 
#       signs of departures from normality.
par(mfrow=c(1,1))

qqnorm(rstudent(lm.final))
qqline(rstudent(lm.final))

# it appears to be light tailed. 

#   (e) Produce plots of the R-student residuals vs. 
#       (1) the predicted values, and
#       (2) each explanatory variable.
#       What assumptions may not be satisfied? Explain.
par(mfrow=c(3,2))
plot(lm.final$fitted.values,rstudent(lm.final))
abline(a=0,b=0)
plot(data.Q2$x1,rstudent(lm.final))
abline(a=0,b=0)
plot(data.Q2$x2,rstudent(lm.final))
abline(a=0,b=0)
plot(data.Q2$x3,rstudent(lm.final))
abline(a=0,b=0)
plot(data.Q2$x4,rstudent(lm.final))
abline(a=0,b=0)
plot(data.Q2$x5,rstudent(lm.final))
abline(a=0,b=0)

# we observe three things:
# 1. a clear patter in the predicted values vs r-student residual plot;
# 2. a fanned-in pattern in the x1 vs r-student residual plot;
# 3. a fanned-out pattern in the x4 vs r-student residual plot.

# such patterns imply a non-constant variance. 
# whether such patterns can go further and imply a non-linear relationship should be investigated more carefully.





