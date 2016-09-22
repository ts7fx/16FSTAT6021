## Tianye Song, ts7fx
## STAT 6021, HW4
## 9/21/2016

#4.2, 4.4, 4.8, 4.13, 4.25, 4.29, 5.2, 5.5, 5.7, 5.9, 5.10
library(car)
library(gdata)
#4.2
# pull code from Q3.1:
setwd("~/Documents/16FSTAT6021/hw/hw3")

data.3.1 <- read.xls("data-table-B1.xls")
lm.3.1 <- lm(y~x2+x7+x8, data=data.3.1)
par(mfrow=c(3,3))

#a)
# investigation will be based on the above linear model 
# plotting normality plot:
qqnorm(rstudent(lm.3.1))
qqline(rstudent(lm.3.1))
# the points lie approximately along the qqline. Some of the points are slightly off the line. There is no big problem, but only slight deviations.

#b) 
# plotting fitted.values vs residuals plot
plot(lm.3.1$fitted.values,resid(lm.3.1), main = "y vs. residuals")
# the points are randomly scattered. Thus the plot looks good.
#c)
# plotting residuals versus each of the regressor variables
plot(data.3.1$x2,resid(lm.3.1), main = "x2 vs. residuals")
plot(data.3.1$x7,resid(lm.3.1), main = "x7 vs. residuals")
plot(data.3.1$x8,resid(lm.3.1), main = "x8 vs. residuals")
# the plot for x8 looks ok. The plot for both x7 and x2 look funnel. Thus the plots for x7 and x2 exhibit nonconstant variance.
#d)
# partial regression:
avPlots(lm.3.1)
# the plot for x7 is worth investigating because it shows there is not a very strong linear relationship between x7 and the response variable.
#e)
# Find the studentized residuals
rstandard(lm.3.1)
# Find the R-student residuals
rstudent(lm.3.1)
# the 1st observation is a possible outlier.

#4.4
# q3.5 code:
data.3.5 <- read.xls("data-table-B3.xls")
lm.3.5 <- lm(y~x1 + x6, data=data.3.5)

#a)
# plotting normality plot:
qqnorm(rstudent(lm.3.5))
qqline(rstudent(lm.3.5))
# there seems to be a problem with the normality assumption. There seems to be a positive skew.

#b) 
# plotting fitted.values vs residuals plot
plot(lm.3.5$fitted.values,resid(lm.3.5), main = "y vs. residuals")
# points are clustered so there seems to be a nonlinear pattern

#c)
#
avPlots(lm.3.5)
# the plot for x6 does not exhibit a strong linear relationship between x6 and y. Thus, should consider removing x6 from the model.

#d)
# Find the studentized residuals
rstandard(lm.3.5)
# Find the R-student residuals
rstudent(lm.3.5)
# the 12th and 15th observations seem to be possible outliers.

#4.8
# q2.12
usage <- read.xls("data-prob-2-12.XLS")
lm.4 <- lm(usage~temp,data=usage)

#a)

# plotting normality plot:
qqnorm(rstudent(lm.4))
qqline(rstudent(lm.4))
# most of the points are scattered approximately along the line. The last point seems to be a special case. Overall speaking, there seems to be no big problem with the normality assumption.

#b)
# plotting fitted.values vs residuals plot
plot(lm.4$fitted.values,resid(lm.4), main = "y vs. residuals")
# the points exhibit a curved pattern

#c)
plot(lm.4$residuals, xlab = "obervation order",main = "y vs. residuals")
# the plot shows a positive correlation with time.


#4.25
# load model and data
data.3.16 <- read.xls("data-table-B16.xls")
lm.total<-lm(LifeExp~People.per.TV+People.per.Dr,data=data.3.16)
lm.male<-lm(LifeExpMale~People.per.TV+People.per.Dr,data=data.3.16)
lm.female<-lm(LifeExpFemale~People.per.TV+People.per.Dr,data=data.3.16)
#a)
# construct 3 normal probability plots:
qqnorm(rstudent(lm.total),main="total")
qqline(rstudent(lm.total))
qqnorm(rstudent(lm.male),main="male")
qqline(rstudent(lm.male))
qqnorm(rstudent(lm.female),main="female")
qqline(rstudent(lm.female))
# all three of the plots show a bit of non-normality at the tails.
# the female plot exhibits the best normality among the three.
#b)
# construct 3 y vs residual plots:
plot(lm.total$fitted.values,resid(lm.total), main = "total")
plot(lm.male$fitted.values,resid(lm.male), main = "male")
plot(lm.female$fitted.values,resid(lm.female), main = "female")
# all three plots illustrate a nonlinear pattern.

#4.29
# using model from q2.22
conv <- read.xls("data-table-B20.XLS")
colnames(conv) <-c("x1","x2","x3","x4","x5","y")
# remove insignificant regressors one by one.
lm.4.29 <- lm(y~.,data=conv)
summary(lm.4.29)
lm.4.29 <- lm(y~.-x4, data = conv)
summary(lm.4.29)
lm.4.29 <- lm(y~.-x4-x5, data = conv)
summary(lm.4.29)

qqnorm(rstudent(lm.4.29))
qqline(rstudent(lm.4.29))
# the normal probability plot demonstrates the normality assumption is met. All points are approximately scattered around the line.

plot(lm.4.29$fitted.values,resid(lm.4.29), main = "y vs residuals")
# in the fitted value vs residuals plot, there exists a pattern.

# thus, overall, the model fits the data with good summary statistics. However, whether the relationship is linear remains questionable.



#5.2
data.5.2 = data.frame(c(273,283,293,303,313,323,333,343,353,363,373),
                      c(4.6,9.2,17.5,31.8,55.3,92.5,149.4,233.7,355.1,525.8,760))
colnames(data.5.2) <- c("temp","pressure")
#a)
plot(data.5.2)
# a straight line model will not suffice.
#b)
lm.5.2 <- lm(pressure~., data= data.5.2)
summary(lm.5.2)
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -1956.258    363.807  -5.377 0.000446 ***
#   temp            6.686      1.121   5.964 0.000212 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 117.6 on 9 degrees of freedom
# Multiple R-squared:  0.7981,	Adjusted R-squared:  0.7756 
# F-statistic: 35.57 on 1 and 9 DF,  p-value: 0.0002117

# interestingly, although we fitted a straight line to a data set that obviously cannot be well explained by a straight line, the outputs are good. 
# firstly, the F-stat is good. Small pvalue, significance in regression.
# Then, P value for temp is good. Predictor is significant.
# R-squared is 0.7981, again, not bad.

# now let's look at residual plots
qqnorm(rstudent(lm.5.2))
qqline(rstudent(lm.5.2))
# the normality assumption seems to be violated by what appears to be the normal probability plot of a positive skewed distribution.

# now plot the fitted value vs residuals plot
plot(lm.5.2$fitted.values,resid(lm.5.2), main = "y vs residuals")
# there is clearly a patter in this plot, which means the relationship is nonlinear.

# conclusion is that since the residual plot illustrates a nonlinear relationship, a linear model clearly is not an adequate choice in this situation.

#c)
data.5.2$lnp <- log(data.5.2$pressure)
data.5.2$newTemp <- -1/data.5.2$temp
lm.5.2.c <- lm(lnp~newTemp, data= data.5.2)
summary(lm.5.2.c)
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 2.061e+01  6.325e-02   325.8   <2e-16 ***
#   newTemp     5.201e+03  2.014e+01   258.3   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.02067 on 9 degrees of freedom
# Multiple R-squared:  0.9999,	Adjusted R-squared:  0.9999 
# F-statistic: 6.672e+04 on 1 and 9 DF,  p-value: < 2.2e-16

# the r-squared improved dramatically. Now nearly all of the variation in y can be explained by x. 
qqnorm(rstudent(lm.5.2.c))
qqline(rstudent(lm.5.2.c))
plot(lm.5.2.c$fitted.values,resid(lm.5.2.c), main = "y vs residuals")
# However, there is no improvement in the model from residual analysis's standpoint. There still is a nonlinear relationship.
# Instead of a positive skewed distribution, we now get a negative skewed one.
# Overall speaking, although prdictors explain variations in dependent variable better, there is no improvement in identifying whether there is a linear relationship or not.


#5.5 
data.5.5 = data.frame(c(13,16.1,14.5,17.8,22,27.4,16.8,34.2,65.6,49.2,66.2,81.2,87.4,114.5),
                      c(4,5,6,7,8,9,10,11,12,13,14,15,16,17))
colnames(data.5.5) <- c("def","week")
#a)
lm.5.5 <- lm(def~week, data = data.5.5)
summary(lm.5.5)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -31.6982     9.7758  -3.243  0.00705 ** 
#   week          7.2767     0.8692   8.372 2.35e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 13.11 on 12 degrees of freedom
# Multiple R-squared:  0.8538,	Adjusted R-squared:  0.8416 
# F-statistic: 70.09 on 1 and 12 DF,  p-value: 2.354e-06

# according to summary statistics, the linear model is modeling the data pretty-well.

# however, whether the data is best modeled using a linear model remains unknown. Thus, I need residual analysis.
plot(lm.5.5$fitted.values,resid(lm.5.5), main = "y vs residuals")
# there appears to be a nonlinear pattern of the residuals vs predicted value plot. Thus, a linear model may not be the best choice.


#b) try take the natural log of y.

data.5.5$newy<-log(data.5.5$def)
lm.5.5.b <- lm(newy~week, data = data.5.5)
summary(lm.5.5.b)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  1.71622    0.17311   9.914 3.93e-07 ***
#   week         0.17351    0.01539  11.273 9.68e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2322 on 12 degrees of freedom
# Multiple R-squared:  0.9137,	Adjusted R-squared:  0.9065 
# F-statistic: 127.1 on 1 and 12 DF,  p-value: 9.676e-08

# there is an all-around improvement in summary statistics

# now let's check to see if there's a linear relationship.
plot(lm.5.5.b$fitted.values,resid(lm.5.5.b), main = "y vs residuals")
qqnorm(rstudent(lm.5.5.b))
qqline(rstudent(lm.5.5.b))
# the model now exhibits a linear relationship.
# it also is light-tailed in the normality check, which is good.

# overall, a btter model is the natural log of defects vs week.

#5.7 
# recall 4.29
conv <- read.xls("data-table-B20.XLS")
lm.4.29 <- lm(y~.-x4-x5, data = conv)

# try to take the natural log for y
conv$newy <- log(conv$y)
# redo the regressor elimination process
lm.4.29b <- lm(newy~., data = conv)
summary(lm.4.29b)
lm.4.29b <- lm(newy~.-x2, data = conv)
summary(lm.4.29b)
lm.4.29b <- lm(newy~.-x2-x4, data = conv)
summary(lm.4.29b)
lm.4.29b <- lm(newy~.-x2-x4-x5, data = conv)
summary(lm.4.29b)

#Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 14.175245   2.007386   7.062 5.67e-06 ***
#   x1          -0.597041   0.228182  -2.617 0.020314 *  
#   x3          -1.597070   0.254575  -6.273 2.04e-05 ***
#   y            0.021711   0.004627   4.692 0.000346 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3782 on 14 degrees of freedom
# Multiple R-squared:  0.9476,	Adjusted R-squared:  0.9364 
# F-statistic: 84.45 on 3 and 14 DF,  p-value: 3.314e-09

# summary statistics look good.

qqnorm(rstudent(lm.4.29b))
qqline(rstudent(lm.4.29b))
# the normal probability plot demonstrates the normality assumption is met. All points are approximately scattered around the line.
plot(lm.4.29b$fitted.values,resid(lm.4.29b), main = "y vs residuals")
# the scatter plot for y vs residuals is better than before, with all points randomly scattered.

# thus, transformation by taking the natural log seemed to have improved the model.


#5.9 
setwd("~/Documents/16FSTAT6021/hw/hw4")
data.5.9 <- read.xls("data-table-B8.xls")
lm.5.9 <- lm(y~., data = data.5.9)
summary(lm.5.9)
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 1.109e+01  1.669e+00   6.642 1.48e-07 ***
#   x1          3.501e+02  3.968e+01   8.823 3.38e-10 ***
#   x2          1.089e-01  9.983e-03  10.912 1.74e-12 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.782 on 33 degrees of freedom
# Multiple R-squared:  0.8415,	Adjusted R-squared:  0.8319 
# F-statistic:  87.6 on 2 and 33 DF,  p-value: 6.316e-14

# both regressors seem to be significant.

# test for normality
qqnorm(rstudent(lm.5.9))
qqline(rstudent(lm.5.9))
# there is a heavy-tailed pattern. Normality is absent.

# look at fitted value vs residuals plot
plot(lm.5.9$fitted.values,resid(lm.5.9), main = "y vs residuals")
# this plot seems normal.

plot(data.5.9$x1,resid(lm.5.9), main = "x1 vs. residuals")
plot(data.5.9$x2,resid(lm.5.9), main = "x2 vs. residuals")
# the plot for x1 vs residuals looks strange becasue x1 is more like a categorical variable with three levels. 
# the plot for x2 vs residuals displays a funnel pattern, which suggests the true relationship between x2 and y is non linear.

# since there is a funnel pattern with decreasing residuals as x increase,
# there is a chance that the true relationship is y~x^2.
# Thus, it makes sense to try to transform y by taking sqrt of it.
data.5.9$newy <- sqrt(data.5.9$y)
lm.5.9b <- lm(newy~x2, data = data.5.9)

plot(lm.5.9b$fitted.values,resid(lm.5.9b), main = "y vs residuals")
plot(data.5.9$x2,resid(lm.5.9b), main = "x2 vs. residuals")

# the new plots are slightly better. Note The x2 vs residuals plot demonstrate less of a funnel pattern.

#5.10
data.5.10 <- read.xls("data-table-B9.xls")
lm.5.10 <- lm(y~., data = data.5.10)

# generate residual plots
qqnorm(rstudent(lm.5.10))
qqline(rstudent(lm.5.10))
# the normality assumption hold
plot(lm.5.10$fitted.values,resid(lm.5.10), main = "y vs residuals")
# there is a pattern in the y vs resid plot. non linear relationship may be present.
plot(data.5.10$x1,resid(lm.5.10), main = "x1 vs. residuals")
# x1 is more like a categorical variable. Thus non linear relationship should be expected
plot(data.5.10$x2,resid(lm.5.10), main = "x2 vs. residuals")
# x2 is more like a categorical variable. Thus non linear relationship should be expected
plot(data.5.10$x3,resid(lm.5.10), main = "x3 vs. residuals")
# x3 is more like a categorical variable. Thus non linear relationship should be expected
plot(data.5.10$x4,resid(lm.5.10), main = "x4 vs. residuals")
# slight pattern of x4 where x4 = 1. There is a patter with x4 where the points a clustered at a center.

#b) 
# try taking the natural log of y
data.5.10$newy <-log(data.5.10$y)
lm.5.10b <- lm(newy~.-y, data = data.5.10)


# generate residual plots
qqnorm(rstudent(lm.5.10b))
qqline(rstudent(lm.5.10b))
# the normality assumption hold
plot(lm.5.10b$fitted.values,resid(lm.5.10b), main = "y vs residuals")
# there is a pattern in the y vs resid plot. non linear relationship seems to be present.
plot(data.5.10$x1,resid(lm.5.10b), main = "x1 vs. residuals")
# x1 is more like a categorical variable. Thus non linear relationship should be expected
plot(data.5.10$x2,resid(lm.5.10b), main = "x2 vs. residuals")
# x2 is more like a categorical variable. Thus non linear relationship should be expected
plot(data.5.10$x3,resid(lm.5.10b), main = "x3 vs. residuals")
# x3 is more like a categorical variable. Thus non linear relationship should be expected
plot(data.5.10$x4,resid(lm.5.10b), main = "x4 vs. residuals")
# slight improve of plot for x4 vs resid. Less clustered the points are.

