## Tianye Song, ts7fx
## STAT 6021, HW3
## 9/14/2016
#3.1, 3.3, 3.4, 3.5, 3.6, 3.8, 3.11, 3.16 
setwd("~/Documents/16FSTAT6021/hw/hw3")
library(gdata)

#3.1 
data.3.1 <- read.xls("data-table-B1.xls")

#a)
lm.3.1 <- lm(y~x2+x7+x8, data=data.3.1)

#b)
anova(lm.3.1)
#F-statistic: 29.44 on 3 and 24 DF,  p-value: 3.273e-08
#since the p-value for the f statistic is very small, the regression is significant for variables x2, x7 & x8.

#c)
summary(lm.3.1)
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.808372   7.900859  -0.229 0.820899    
# x2           0.003598   0.000695   5.177 2.66e-05 ***
# x7           0.193960   0.088233   2.198 0.037815 *  
# x8          -0.004816   0.001277  -3.771 0.000938 ***
# for all three coefficients (B2,B7 and B8), we reject the null hypotheses that either one of them is not significant. 
# We accept the alternative hypotheses that each individual explanatory variable is significant by itself when the rest 
# are held constant

#d)
summary(lm.3.1)$r.squared
summary(lm.3.1)$adj.r.squared
# R-squared:  0.7863,	Adjusted R-squared:  0.7596

#e)
lm.reduced.3.1 = lm(y ~ x2+x8, data=data.3.1) # Reduced model
anova(lm.reduced.3.1, lm.3.1) 
# Analysis of Variance Table
# 
# Model 1: y ~ x2 + x8
# Model 2: y ~ x2 + x7 + x8
#   Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
# 1     25 83.938                              
# 2     24 69.870  1    14.068 4.8324 0.03782 *

# since the p-value is less than 0.05, therefore we reject H0 that x7 has no contribution. 
# the F stat is the square of t-stat


#3.3
#a)
confint(lm.3.1, parm = 'x7')
#          2.5 %     97.5 % 
# x7 (0.01185532,0.37606510)

#b) 
tempdf<-data.frame(x2=2300,x7=56,x8=2100)
predict(lm.3.1, tempdf, interval = "confidence")
#        fit      lwr      upr
# 1 7.216424 6.436203 7.996645

#3.4
lm.3.4 <- lm(y~x7+x8, data = data.3.1)
#a)
summary(lm.3.4)
#F-statistic: 15.13 on 2 and 25 DF,  p-value: 4.935e-05, thus we reject H0 and accept there is linear regression correlation.

#b)
summary(lm.3.4)$r.squared
summary(lm.3.4)$adj.r.squared
#r squared: 0.5476628 adj. r squared: 0.5114759
#comparing with from 3.1:[R-squared:  0.7863,	Adjusted R-squared:  0.7596], 
#both r squared and adjusted r squared dropped in value

#c)
#95% CI on B7:
confint(lm.3.4, parm = 'x7')

#         2.5 %    97.5 %
# x7 -0.1971643, 0.293906

#
tempdf.3.4<-data.frame(x7=56,x8=2100)
predict(lm.3.4, tempdf.3.4, interval = "confidence")
# fit      lwr      upr
# 1 6.926243 5.828643 8.023842

#comparing with results generated in 3.1:

#          2.5 %     97.5 % 
# x7 (0.01185532,0.37606510)

# comparing the length of the confidence interval:
0.293906--0.1971643 # 0.4910703
0.37606510-0.01185532 # 0.3642098
# the CI for B7 increased in its length

#        fit      lwr      upr
# 1 7.216424 6.436203 7.996645
8.023842-5.828643 #2.195199
7.996645-6.436203 #1.560442

# the CI for mean of games won also increased in length
# in conclusion, both CIs became wider when we excluded x2 as a predictor.

#d)

# by omitting an important regressor, two things happened in this question's domain
# 1. the adjusted R-squared will decrease, which means less variation in y is explained by the linear model.
# 2. the confidence intervals for both individual coefficients as well as y|x0 become wider.


#3.5

data.3.5 <- read.xls("data-table-B3.xls")
#a
lm.3.5 <- lm(y~x1 + x6, data=data.3.5)
#b
summary(lm.3.5)
#F-statistic: 53.67 on 2 and 29 DF,  p-value: 1.79e-10, there is a significance of regression.

#c
summary(lm.3.4)$r.squared
summary(lm.3.4)$adj.r.squared
#Multiple R-squared:  0.7873,	Adjusted R-squared:  0.7726
#in problem 2.4, r-squared is 0.7722712, Adjusted R-squared:  0.7647

#not much changed. 

#d
#95% CI on B1:
confint(lm.3.5, parm = 'x1')

#           2.5 %      97.5 %
#   x1 -0.06569892 -0.04059641

#e
summary(lm.3.5)

# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 32.884551   1.535408  21.417  < 2e-16 ***
#   x1          -0.053148   0.006137  -8.660 1.55e-09 ***
#   x6           0.959223   0.670277   1.431    0.163  

# by looking at p-values, x1 is significant yet x6 is not.

#f
tempdf.3.5<-data.frame(x1=275,x6=2)
predict(lm.3.5, tempdf.3.5, interval = "confidence")
#        fit      lwr      upr
# 1 20.18739 18.87221 21.50257

#g
predict(lm.3.5, tempdf.3.5, interval = "predict")

#        fit     lwr      upr
# 1 20.18739 13.8867 26.48808

#3.6
#            CI         PI 
#length 2.21311   12.71319 in 2.4
#       vs
#length 2.63036   12.60138 in this question
#the lengths did not vary much. This probably indicates that adding x6 into the set of regressors did not contribute much. 
#just as the r squared and adj.r squared indicate.

#3.8
data.3.8 <- read.xls("data-table-B5.xls")
#a
lm.3.8<-lm(y~x6+x7, data = data.3.8)
#b
summary(lm.3.8)
# Multiple R-squared:  0.6996,	Adjusted R-squared:  0.6746 
# F-statistic: 27.95 on 2 and 24 DF,  p-value: 5.391e-07, there is significance of regression.

#c
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 2.526460   3.610055   0.700   0.4908    
# x6          0.018522   0.002747   6.742 5.66e-07 ***
# x7          2.185753   0.972696   2.247   0.0341 *  

# both x6 & x7 have very small p-values, thus both are significant and contribute to the model.

#d
#95% CI on B6:
confint(lm.3.8, parm = 'x6')
#           2.5 %     97.5 %
#   x6 0.01285196 0.02419204
#95% CI on B7:
confint(lm.3.8, parm = 'x7')
#          2.5 %   97.5 %
#   x7 0.1782076 4.193298

#e
lm.3.8.b<-lm(y~x6, data = data.3.8)
summary(lm.3.8.b)


# Multiple R-squared:  0.6365,	Adjusted R-squared:  0.6219 
# F-statistic: 43.77 on 1 and 25 DF,  p-value: 6.238e-07 the model is still significant in regression
# there is a slight drop in R squared value. This is an acceptable model. 

#f
#95% CI on B6:
confint(lm.3.8.b, parm = 'x6')

#           2.5 %     97.5 %
#   x6 0.01335688 0.02543261

# compare length: 0.01207573 in simple linear regression model vs 0.01134008 in previous model.
# both lengths are nearly equal. 

#g

anova(lm.3.8.b)
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# x6         1 5008.9  5008.9  43.766 6.238e-07 ***
# Residuals 25 2861.2   114.4 

anova(lm.3.8)
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# x6         1 5008.9  5008.9 50.8557 2.267e-07 ***
# x7         1  497.3   497.3  5.0495    0.0341 *  
# Residuals 24 2363.8    98.5 



# MSres is lower when an additional explanatory variable is included in the model.
# However, the decrease in MSres is small when x7 is included in the model. Which means only a small portion of variation
# in y is explained by x7.


#3.11
data.3.11 <- read.xls("data-table-B7.xls")
#a.
lm.3.11 <- lm(y~. ,data=data.3.11)
#b.
summary(lm.3.11)
#F-statistic: 29.86 on 5 and 10 DF,  p-value: 1.055e-05 
#there is a significance of regression

#c.
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  5.208e+01  1.889e+01   2.757 0.020218 *  
# x1           5.556e-02  2.987e-02   1.860 0.092544 .  
# x2           2.821e-01  5.761e-02   4.897 0.000625 ***
# x3           1.250e-01  4.033e-01   0.310 0.762949    
# x4           1.776e-16  2.016e-01   0.000 1.000000    
# x5          -1.606e+01  1.456e+00 -11.035  6.4e-07 ***

# according to tvalue and p-values, we can see that x2 and x5 are significant

#d.
# for the all-inclusive model
# Multiple R-squared:  0.9372,	Adjusted R-squared:  0.9058 

# for model that includes only temperature and particle size,
lm.3.11.d <- lm(y~x2+x5,data = data.3.11)
summary(lm.3.11.d)
# Multiple R-squared:  0.9149,	Adjusted R-squared:  0.9018 

# there is only slight change in Adj. R squared.. 

#e.
confint(lm.3.11, parm = 'x2')
#          2.5 %    97.5 %
#   x2 0.1537804 0.4105053
# length of CI = 0.2567249
confint(lm.3.11.d, parm = 'x2')
#          2.5 %    97.5 %
#   x2 0.1550559 0.4092298
# length of CI = 0.2541739

# they are nearly the same

#3.16
data.3.16 <- read.xls("data-table-B16.xls")

#a.
lm.total<-lm(LifeExp~People.per.TV+People.per.Dr,data=data.3.16)
lm.male<-lm(LifeExpMale~People.per.TV+People.per.Dr,data=data.3.16)
lm.female<-lm(LifeExpFemale~People.per.TV+People.per.Dr,data=data.3.16)

#b.
summary(lm.total)
#F-statistic: 13.46 on 2 and 35 DF,  p-value: 4.623e-05, regression is significant
summary(lm.male)
#F-statistic: 12.53 on 2 and 35 DF,  p-value: 7.863e-05, regression is significant
summary(lm.female)
#F-statistic: 14.07 on 2 and 35 DF,  p-value: 3.279e-05, regression is significant

#c.

summary(lm.total)
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)   70.2362645  1.0925483  64.287   <2e-16 ***
#   People.per.TV -0.0226074  0.0096005  -2.355   0.0243 *  
#   People.per.Dr -0.0004470  0.0002016  -2.217   0.0332 * 

# both regressors are significant

summary(lm.male)
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)   73.0919445  1.2505753  58.447   <2e-16 ***
#   People.per.TV -0.0256825  0.0109891  -2.337   0.0253 *  
#   People.per.Dr -0.0004785  0.0002308  -2.074   0.0455 *

# both regressors are significant

summary(lm.female)
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)   67.4297595  0.9569733  70.461   <2e-16 ***
#   People.per.TV -0.0198637  0.0084091  -2.362   0.0239 *  
#   People.per.Dr -0.0004086  0.0001766  -2.314   0.0267 *

# both regressors are significant


#d.
summary(lm.total)
#Multiple R-squared:  0.4347,	Adjusted R-squared:  0.4024 
summary(lm.male)
#Multiple R-squared:  0.4173,	Adjusted R-squared:  0.384 
summary(lm.female)
#Multiple R-squared:  0.4457,	Adjusted R-squared:  0.414 

#e
confint(lm.total, parm = 'People.per.Dr')
#                       2.5 %        97.5 %
# People.per.Dr -0.0008563196 -3.777668e-05
confint(lm.male, parm = 'People.per.Dr')
#                       2.5 %        97.5 %
# People.per.Dr -0.0009470177 -1.008023e-05
confint(lm.female, parm = 'People.per.Dr')
#                       2.5 %        97.5 %
# People.per.Dr -0.0007670492 -5.007977e-05





