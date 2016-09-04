
###########################
#                         #
#   Team Assignment 1     #
#                         #
###########################

## Please submit one set of answers per team.            ##
## Your answers may be submitted as an annotated R file. ##
###########################################################

## Start with given x-values
x <- read.table("teamassign01data.txt")[,1]
x

## Generate corresponding y-values according to the model y ~ 25 + 4x + e, where e~N(0,var=12^2)
y <- 25 + 4*x + rnorm(100, mean=0, sd = 12)

## Plot the relationship
plot(x,y, pch=20, cex=0.3)


#################
## Question 1: ##
#################

 
df = data.frame(x,y)

bp.lm <- lm(y~x, data=df)
class(coef(bp.lm))
summary(bp.lm)

y_pred <- coef(bp.lm)[1] + coef(bp.lm)[2] * 18

sum(bp.lm$residuals^2)/98 

anova(bp.lm)

# Using the (x,y) from above, generate a linear model. 
#
#   (a) Report the coefficients hat(beta_0) and hat(beta_1).
#   (b) Report the predicted value of y for x=18.
#   (c) Report MS_Res.


#################
## Question 2: ##
#################

# Generate the linear model requested in Question 1 1000 times. Create a new vector of y-values 
# for each repetition.
ytemp=list()
for (i in 1:1000){
  # for each iteration, create a new vector of y-values
  ytemp[i] <-  data.frame(25 + 4*x + rnorm(100, mean=0, sd = 12))
  # for each iteration, use x & y for that iteration to generate a linear model
}
#
#   (a) Determine and report the mean and variance of the generated coefficients.
#   (b) Based on theoretical considerations, what should the mean and variance of the  
#       generated coefficients be? Explain your answer.
#   (c) Find a 95% confidence interval centered at each coefficient. Determine and report  
#       the percentage of intervals that contain the true value of the coefficient. 
#       What should the percentage be?
#   (d) Carry out the hypothesis test H0: beta_1 = 4 vs H1: beta_1 not= 4 at a 5% significance level. 
#       Determine and report the proportion of times that the null hypothesis is rejected, 
#       implying that beta_1 not= 4.
#   (e) For each set of coefficients, find a 95% confidence interval for the mean
#       response associated with x = 18. Determine and report the percentage of your
#       intervals that contain the true value. What should the percentage be?
#   (f) For each estimated mean response from part (d), find a corresponding
#       95% prediction interval for the response y. Generate one random response y based 
#       on the true model. Determine and report the percentage of intervals that contain the response.
#       What should the percentage be?
#   (g) Find and report a 95% confidence interval for sigma^2 by finding the 2.5th and 97.5th 
#       percentiles of the generated values of MS_Res to give the lower and upper confidence limits.

