## Tianye Song, ts7fx
## STAT 6021, HW1
## 8/31/2016

#2.1 
library(gdata)
# read in data
football <- read.xls("data-table-B1.XLS")
# a. fit a simple linear regression model
lm.1 <- lm(y~x8,data=football)
# b. construct the ANOVA table
anova(lm.1)
# Analysis of Variance Table
# Response: y
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# x8         1 178.09 178.092  31.103 7.381e-06 ***
# Residuals 26 148.87   5.726      

# since F(0.01, 1, 26) = 7.72
# we reject H0.
# Thus, x8 is of value in explaining the variability in y

# c.  95% CI on B1
CI1 <- c(lm.1$coefficients[2] - abs(qt(0.025,26)) * summary(lm.1)$coefficients[,2][2],
         lm.1$coefficients[2] + abs(qt(0.025,26)) * summary(lm.1)$coefficients[,2][2])
#(-0.009614347 -0.004435854) 

# d. R squared is the coef that states the proportion of variation in y that is explained by variation in x.

summary(lm.1)$r.squared # 0.5446843

# which means 54.5% of the variation in y is explained by x8.

# e. 
# y = b0 + b1x; for x = 2000, get y:
y0 <- lm.1$coefficients[1] + lm.1$coefficients[2] * 2000
# t-value: 
t <- abs(qt(0.025, 26))
# MSres, xbar, Sxx
MSres <- anova(lm.1)[,3][2]
xbar <- mean(football$x8)
Sxx <- sum((football$x8 - xbar)^2)
#sqrt(MSres * (1/n + (x0-xbar)^2/Sxx))
temp <- sqrt(MSres * (1/28 + (2000-xbar)^2/Sxx))
CI1e <- c(y0 - t * temp, y0 + t * temp)
#(6.765753, 8.710348)

#2.2 
y2.2 <- lm.1$coefficients[1] + lm.1$coefficients[2] * 1800
# 9.14307
# use t value for 90% intervals
t2.2 <- abs(qt(0.05, 26))
# formula for stuff under square root operataion changes:

temp2.2 <- sqrt(MSres * (1 + 1/28 + (1800-xbar)^2/Sxx))

# 90% PI:

PI <- c(y2.2 - t2.2 * temp2.2,
        y2.2 + t2.2 * temp2.2)

#(4.936392, 13.349749)

#2.4
# a. this step is very similar to 2.1a
miles <- read.xls("data-table-B3.XLS")
lm.2 <- lm(y~x1,data=miles)

# b. again, very similar to 2.1b
anova(lm.2)
# Analysis of Variance Table
# 
# Response: y
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# x1         1 955.72  955.72  101.74 3.743e-11 ***
# Residuals 30 281.82    9.39                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# since F(0.01, 1, 30) = 7.56
# we reject H0.
# Thus, x1 is of value in explaining the variability in y

# c. look at r-squared for the answer
summary(lm.2)$r.squared
# 0.7722712
# thus, 77.2% of total variability in gasoline milage is accounted for by the linear relationship with engine displacement.

# d. similar to 2.1e

# y = b0 + b1x; for x = 275, get y:
y2 <- lm.2$coefficients[1] + lm.2$coefficients[2] * 275
# t-value: 
t2 <- abs(qt(0.025, 30))
# MSres, xbar, Sxx
MSres2 <- anova(lm.2)[,3][2]
xbar2 <- mean(miles$x1)
Sxx2 <- sum((miles$x1 - xbar)^2)
#sqrt(MSres * (1/n + (x0-xbar)^2/Sxx))
temp2 <- sqrt(MSres2 * (1/32 + (275-xbar2)^2/Sxx2))
CI2d <- c(y2 - t2 * temp2, y2 + t2 * temp2)
#(19.59224, 21.80535)

# e. 95% PI for x = 275
y2e <- lm.2$coefficients[1] + lm.2$coefficients[2] * 275

# use t value for 95% intervals
t2e <- abs(qt(0.025, 30))
# formula for stuff under square root operataion:

temp2e <- sqrt(MSres2 * (1 + 1/32 + (275-xbar2)^2/Sxx2))

# 95% PI:

PI2e <- c(y2e - t2e * temp2e,
        y2e + t2e * temp2e)

#(14.34220, 27.05539)

# f. the PI is wider than the CI, because the PI estimates the probability of a future y value lies within a range.
# Thus, the PI not only take into consideration the uncertainty of the fitted model (like CI does), but also 
# the uncertainty of that future observation. 

#2.5
# a. 
lm.3 <- lm(y~x10,data=miles)

# b. 
anova(lm.3)
# Analysis of Variance Table
# 
# Response: y
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# x10        1 921.53  921.53  87.482 2.121e-10 ***
# Residuals 30 316.02   10.53                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# since F(0.01, 1, 30) = 7.56
# we reject H0.
# Thus, x10 is of value in explaining the variability in y

# c. look at r-squared for the answer
summary(lm.3)$r.squared
# 0.7446425
# thus, 74.5% of total variability in gasoline milage is accounted for by the linear relationship with engine displacement.

# H0 is rejected for both x1 and x10,
# r-squared of x1 and x10 are slightly different, 
# not enough evidence to conclude one is better than the other.

#2.12
usage <- read.xls("data-prob-2-12.XLS")
# a.
lm.4 <- lm(usage~temp,data=usage)
# b. 
anova(lm.4)
# Analysis of Variance Table
# 
# Response: usage
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# temp       1 280590  280590   74123 < 2.2e-16 ***
# Residuals 10     38       4                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# F-value is too large. Ho rejected for sure.

# c. H0 : B1 = 10000 vs H1 : B1 ≠ 10000 
t <- (coef(lm.4)[2] - 10000/1000) / sqrt(4/sum((usage$temp - mean(usage$temp))^2))
pt(-22.76601, 10)
# p value is 3.014099e-10. Reject H0. Claim that the increase is not 10000.

# d. 99% PI, temp = 58

y4 <- lm.4$coefficients[1] + lm.4$coefficients[2] * 58

# use t value for 99% intervals
t4 <- abs(qt(0.005, 10))
# formula for stuff under square root operataion:
MSres4 <- anova(lm.4)[,3][2]
xbar4 <- mean(usage$temp)
Sxx4 <- sum((usage$temp - xbar4)^2)
temp4 <- sqrt(MSres4 * (1 + 1/12 + (58-xbar4)^2/Sxx4))

# 95% PI:

PI4 <- c(y4 - t4 * temp4,
          y4 + t4 * temp4)

# (521.2237, 534.2944)
