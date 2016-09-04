
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
y
## Plot the relationship
plot(x,y, pch=20, cex=0.3)


#################
## Question 1: ##
#################

# Using the (x,y) from above, generate a linear model. 

df = data.frame(x,y)
bp.lm <- lm(y~x, data=df)

#   (a) Report the coefficients hat(beta_0) and hat(beta_1).

b0hat = coef(bp.lm)[1]
b1hat = coef(bp.lm)[2]

#   (b) Report the predicted value of y for x=18.

y_pred <- b0hat + b1hat * 18

#   (c) Report MS_Res.

anova(bp.lm)$`Mean Sq`[2]


#################
## Question 2: ##
#################

# Generate the linear model requested in Question 1 1000 times. 
# Create a new vector of y-values for each repetition.

# initiate two vectors to keep track of b0hat and b1hat values for each linear model
b0vec<-vector()
b1vec<-vector()
lm.list <- list()
for (i in 1:1000){
  # for each iteration, create a new vector of y-values & generate a new linear model
  ytemp <- 25 + 4*x + rnorm(100, mean=0, sd = 12)
  dftemp <- data.frame(x,ytemp)
  lm.temp <- lm(ytemp~x, data=dftemp)
  # store coefficients of each linear model into corresponding location in vectors initiated outside of this for loop
  b0vec[i] = coef(lm.temp)[1]
  b1vec[i] = coef(lm.temp)[2]
  lm.list[[i]] = lm.temp
}
#
#   (a) Determine and report the mean and variance of the generated coefficients.
mean(b0vec)
mean(b1vec)
var(b0vec)
var(b1vec)

#   (b) Based on theoretical considerations, what should the mean and variance of the  
#       generated coefficients be? Explain your answer.

# b0hat and b1hat are least square estimators of the parameters of the true regression model.
# They are also unbiased estimators of the model parameters B0 and B1
# Thus, theoretically, the mean of the generated coefficients should be the true values of the coefficients
# of the population regression model, which means:
#     E{B1hat} = B1 & E{B0hat} = B0

# As for Var{B1hat} & Var{B0hat}, according to the Gauss-Markov Theorem, the least squares estimators have
# minimum variance when compared with all other unbiased estimators. Thus, the variances of B1hat and B0hat
# are minimum, hence they are the best linear unbiased estimators.


#   (c) Find a 95% confidence interval centered at each coefficient. Determine and report  
#       the percentage of intervals that contain the true value of the coefficient. 
#       What should the percentage be?
CIs<-lapply(lm.list, confint)
b0tf <- vector()
b1tf <- vector()

# for each CI, 
for (i in 1:1000){
  if (CIs[[i]][1,1]<=25 & 25<=CIs[[i]][1,2])
    b0tf[i] <- TRUE
  else
    b0tf[i] <- FALSE
  if (CIs[[i]][2,1]<=4 & 4<=CIs[[i]][2,2])
    b1tf[i] <- TRUE
  else
    b1tf[i] <- FALSE
}

sum(b0tf)/1000
sum(b1tf)/1000




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

