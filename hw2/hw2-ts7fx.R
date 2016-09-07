## Tianye Song, ts7fx
## STAT 6021, HW2
## 9/6/2016
#2.20, 2.21, 2.22, 2.30 
setwd("~/Documents/16FSTAT6021/hw2")
library(gdata)

#2.20
# y: fuel consumption in g/km
# test if y ~ x_5 (x_5 is the "initial boiling point (degrees C)")
# test for linear relationship between y and x_5
# H0: B1 = 0 vs H1: B1 != 0 
boil <- read.xls("data-table-B18.XLS")
lm.boil <- lm(y.~X.x_5.,data=boil)
summary(lm.boil)
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 410.72319   18.92479  21.703 3.54e-12 ***
#   X.x_5.     -0.26376    0.09622  -2.741   0.0159 *  
#   Multiple R-squared:  0.3493
##
# according to the summary, p-value = 0.0159. So for any alpha > 0.0159, we would reject H0:B1=0 and conclude that
# there is indeed a linear relationship between y and x_5. That is, fuel consumption can be explained in a linear regression
# relationship by the initial boiling point of the fuel. 

# more specifically, the coefficient for x_5 is -0.26376. That is, for every 1 degree C incress in the initial boiling point 
# of the fuel, there is a -0.26376g/km decrease in fuel consumption.

# R-squared in this case = 0.3493, which means 34.93% of the variance in fuel consumption can be explained by the initial 
# boiling point of the fuel.

#2.21
# y: overall quality rating of a wine
# test if y ~ x_3 (x_3 is the "Total SO₂(ppm)")
# test for a negative linear relationship between y and x_3
# H0: B1 = 0 vs H1: B1 != 0 
wine <- read.xls("data-table-B19.XLS")
lm.wine <- lm(y~X.x_3.,data=wine)
summary(lm.wine)
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 16.564030   0.620906  26.677   <2e-16 ***
#   X.x_3.    -0.012762   0.005744  -2.222    0.034 *  
#   Multiple R-squared:  0.1413

# according to the summary, the p-value for x_3 is 0.034, which means for all alpha > 0.034, we reject the null hypotheses
# of B1 = 0 and conclude that there exists a linear relationship between overall quality rating of a wine and its total SO2
# content.

# Moreover, the coefficient B1 = -0.012762. That means for every 1 ppm increase in total SO2 content of a wine, its corresponding
# overall quality rating drop by 0.012762 of a unit measure.

# In this case, R-squared is 0.1413, which means 14.13% of the variance in the overall quality rating of a wine can be 
# explained by its total SO2 content.


#2.22
# y: percent conversion
# test if y ~ x_5 (x_5 is the "ratio of inlet oxygen to inlet methanol")
# test for a linear relationship between y and x_5
# H0: B1 = 0 vs H1: B1 != 0 
conv <- read.xls("data-table-B20.XLS")
lm.conv <- lm(X.y~X.x_5.,data=conv)
summary(lm.conv)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)    21.25      20.99   1.013    0.326
# X.x_5.          7.80      16.78   0.465    0.648

# P value is this case is 0.648. Which means for a widely used a=0.05 hypotheses test, we would accept H0 and conclude
# that there is no linear relationship between the percent convertion and the ratio of inlet oxygen to inlet methanol

#2.30
usage <- read.xls("data-prob-2-12.XLS")
lm.usage <- lm(usage~temp,data=usage)
#a. sample correlation coefficient = sqrt(R^2)
r <- sqrt(summary(lm.usage)$r.squared)
#0.9999326

#b. H0: rho = 0; H1: rho != 0
# t0 = (r*sqrt(n-2))/sqrt(1-r^2)
n = nrow(usage)
t0 = (r*sqrt(n-2))/sqrt(1-r^2)

#using a alpha = 0.05 hypothesis test,
t = qt(0.975, n-1)

#t0 = 272.255 and t = 2.2
#we reject the hypotheses that rho = 0

#c. H0: rho = 0.5; H1: rho != 0.5
Z0 = (atanh(r) - atanh(0.5)) * sqrt(n-3)
Z = qnorm(0.975)
# here Z0 = 13.8 and Z = 1.96.
# we reject the H0 and conclude that rho != 0.5

#d. plug in the formula of tanh(arctanh(r) - Z0.995/sqrt(n-3)) <= rho <= tanh(arctanh(r) + Z0.995/sqrt(n-3))
CI <- c(tanh(atanh(r) - qnorm(0.995)/sqrt(n-3)), tanh(atanh(r) + qnorm(0.995)/sqrt(n-3)))
CI
# (0.9996244, 0.9999879)




