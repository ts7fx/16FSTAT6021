
###########################
#                         #
#   Team Assignment 4     #
#                         #
###########################

## Please submit one set of answers per team.                    ##
## Your answers may be submitted as an annotated R file.         ##
## Please submit your plots in one PDF as a separate attachment. ##
###################################################################
library(gdata)
library(MASS)

#################
## Question 1: ##
#################

# For each part of this problem, you are to create (or find) a data set with about 30 observations 
# that is suitable for simple linear regression and satisfies the given specifications. If it is 
# not possible, explain why it is not possible. For each part, include a plot that shows the 
# interesting points.
#
#   (a) The data set has a point that is clearly visible for all four types of residuals 
#       discussed -- standardized, studentized, PRESS, R-student.
df <- data.frame(c(1:29),c(20:48))
colnames(df) <- c('x','y')

attach(df)
plot(df$x,df$y)

df$y[15] <- 120
plot(df$x,df$y)

lm.df<-lm(df$y~df$x)
summary(lm.df)
summary(lm.df)$r.squared
## Find the standardized residuals
plot(stdres(lm.df))
abline(a=0,b=0)
## Find the studentized residuals
plot(rstandard(lm.df))
abline(a=0,b=0)
## Find the PRESS residuals
plot(residuals(lm.df)/(1 - lm.influence(lm.df)$hat))
## Find the R-student residuals
plot(rstudent(lm.df))
#   (b) The data set has a point that stands out when viewing studentized residuals but not 
#       when viewing standardized residuals.
x<-rnorm(30, mean=0, sd = 12)
y<-25 + 4*x + rnorm(30, mean=0, sd = 12)
df1<-data.frame(y,x)
plot(df1$x,df1$y)

df1$x[30] <- 45
df1$y[30] <- 125
plot(df1$x,df1$y)

lm.df1<-lm(df1$y~df1$x)
summary(lm.df1)
summary(lm.df1)$r.squared

## Find the standardized residuals
stdres(lm.df1)
plot(stdres(lm.df1))
abline(a=0,b=0)
## Find the studentized residuals
rstandard(lm.df1)
plot(rstandard(lm.df1))
abline(a=0,b=0)
#   (c) The data set has a point that stands out when viewing PRESS residuals but not when 
#       viewing standardized residuals.
set.seed(10)
x<-rnorm(30, mean=0, sd = 12)
y<-25 + 4*x + rnorm(30, mean=0, sd = 12)
df2<-data.frame(y,x)
plot(df2$x,df2$y)

df2$x[30] <- 34
df2$y[30] <- 115
plot(df2$x,df2$y)

lm.df2<-lm(df2$y~df2$x)
summary(lm.df2)
summary(lm.df2)$r.squared

#Standardized Residual
stdres(lm.df2)
plot(stdres(lm.df2))
abline(a=0,b=0)
boxplot(stdres(lm.df2))

#press residuals
residuals(lm.df2)/(1 - lm.influence(lm.df2)$hat)
plot(residuals(lm.df2)/(1 - lm.influence(lm.df2)$hat))
abline(a=0,b=0)
boxplot(residuals(lm.df2)/(1 - lm.influence(lm.df2)$hat))


#   (d) The data set has a point that stands out when viewing R-student residuals but not when 
#       viewing standardized residuals.
#   (e) The data set has a point that stands out when viewing PRESS residuals but not when 
#       viewing studentized residuals.
set.seed(10)
x<-rnorm(30, mean=0, sd = 12)
y<-25 + 4*x + rnorm(30, mean=0, sd = 12)
df2<-data.frame(y,x)
plot(df2$x,df2$y)

df2$x[30] <- 34
df2$y[30] <- 115
plot(df2$x,df2$y)

lm.df2<-lm(df2$y~df2$x)
summary(lm.df2)
summary(lm.df2)$r.squared

#Standardized Residual
rstandard(lm.df2)
plot(rstandard(lm.df2))
abline(a=0,b=0)
boxplot(rstandard(lm.df2))

#press residuals
residuals(lm.df2)/(1 - lm.influence(lm.df2)$hat)
plot(residuals(lm.df2)/(1 - lm.influence(lm.df2)$hat))
abline(a=0,b=0)
boxplot(residuals(lm.df2)/(1 - lm.influence(lm.df2)$hat))
#   (f) The data set has a point that stands out when viewing R-student residuals but not when 
#       viewing PRESS residuals.



#################
## Question 2: ##
#################

# For each part of this problem, you are to create (or find) a data set with about 30 observations 
# that is suitable for simple linear regression and satisfies the given specifications on the variance
# of the residuals. For each part, include a plot of your residuals that shows the required characteristic.
#
#   (a) The residuals have constant variance.
#   (b) The residuals have variance proportional to E(y).
#   (c) The residuals have variance proportional to E(y)^2.
#   (d) The residuals have variance proportional to 1/E(y).
#   (e) The residuals have variance proportional to C-E(y) for some constant C.
#   (f) The residuals have variance proportional to E(y)(C-E(y)) for some constant C.



