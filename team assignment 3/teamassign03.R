
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
sd.B1 <- sd(B1) 
sd.B2 <- sd(B2) 
sd.B3 <- sd(B3) 
sd.B4 <- sd(B4)
mean.MSE <- mean(MSE)


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
  #### not sure about this, whether the sd & mean of Betas & MSE should be computed inside/outside of this forloop.
  #### check with instructor
  sd.B1 <- sd(b1vec) 
  sd.B2 <- sd(b2vec) 
  sd.B3 <- sd(b3vec) 
  sd.B4 <- sd(b4vec)
  mean.MSE <- mean(msevec)
}


#   (b) Choose a suitable variable to remove from the model. Repeat (1)-(4) given in part (a)
#       1000 times using this model.


#   (c) How do the results from parts (a)(4) and (b)(4) compare? Explain what you observe.



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

##### ask instructor if it is necessary to use a for loop here.
# fullmodel<-lm(y~. ,data = data.Q2)
# data.Q2.a <- data.Q2
# name = 'x5'
# data.Q2.a$name=NULL
# for(i in 1:5){
#   # find the max p-value, if that p-value is larger than 5%, then remove the corresponding variable from the linear model.
#   if(max(summary(fullmodel)$coefficients[2:6,4])>0.05 ){
#     # getting the name of the response variable that has the max p-value
#     name <- names(which.max(summary(fullmodel)$coefficients[2:6,4]))
#     
#     
#   }
# }
# step(fullmodel, direction = "backward", trace=1 )


#   (b) Compute each of the five types of residuals discussed in the textbook:
#       Residuals; Standardized residuals; Studentized residuals; PRESS residuals; R-student residuals.
#       You may use R functions.

###resid(lm.final, type="pearson")/sqrt(1 - hatvalues(lm.final))
pr <- residuals(lm.final)/(1 - lm.influence(lm.final)$hat)
PRESS <- sum(pr^2)
PRESS
## Find the residuals
resid(lm.final)
## Find the standardized residuals
stdres(lm.final)
rstandard(lm.final)
## Find the studentized residuals
studres(lm.final)

## Find the PRESS residuals

##### ask the instructor about this
## Find the R-student residuals
rstudent(lm.final)


##### ??????
#   (c) Use the results from part(a) to decide if there appear to be any outliers and/or high 
#       influence points.





#   (d) Produce a normal probability plot of the R-student residuals and evaluate the plot for 
#       signs of departures from normality.
qqnorm(rstudent(lm.final))
qqline(rstudent(lm.final))

# light tailed. 

#   (e) Produce plots of the R-student residuals vs. 
#       (1) the predicted values, and
#       (2) each explanatory variable.
#       What assumptions may not be satisfied? Explain.

lm.final$fitted.values
rstudent(lm.final)
plot(lm.final$fitted.values,rstudent(lm.final))
abline(a=0,b=0)





