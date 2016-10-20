## Tianye Song, ts7fx
## STAT 6021, HW6
## 10/16/2016

library(gdata)
library(dplyr)
library(leaps)
#11.1
# retrieve model built for problem 3.1
data.3.1 <- read.xls("data-table-B1.xls")
lm.3.1 = lm(y ~ x2+x8, data=data.3.1)

## Find the PRESS stats
PRESS <-sum((residuals(lm.3.1)/(1 - lm.influence(lm.3.1)$hat))^2) 
# 100.8346
anova(lm.3.1)
# Response: y
#           Df  Sum Sq Mean Sq F value    Pr(>F)    
# x2         1  76.193  76.193  22.693 6.873e-05 ***
# x8         1 166.833 166.833  49.689 2.177e-07 ***
# Residuals 25  83.938   3.358   

# SST = 76.193+166.833+83.938 = 326.964

# R-squared-pred = 1 - 100.8346/326.964 = 69.2%

# The predictive power is acceptable.

# now take a random sample of half the size of the original data set
data.3.1.b <- data.3.1[sample(1:nrow(data.3.1), 14,replace=FALSE),]

# fit the same regression model to the data
lm.3.1.b = lm(y ~ x2+x8, data=data.3.1.b)
coef(lm.3.1)
#  (Intercept)           x2           x8 
# 14.712674988  0.003111132 -0.006808275 
coef(lm.3.1.b)
#  (Intercept)           x2           x8 
# 17.005379210  0.001648106 -0.006382876 

# on this sample run, coefficient for x2 varied by (0.003111132-0.001648106/0.003111132)=-53%, which is a pretty sizable decrease.
# the predictive power:
PRESS.half <-sum((residuals(lm.3.1.b)/(1 - lm.influence(lm.3.1.b)$hat))^2) 
#71.53252
anova(lm.3.1.b)
# Response: y
#           Df Sum Sq Mean Sq F value   Pr(>F)   
# x2         1 20.567  20.567  4.6758 0.053484 . 
# x8         1 46.263  46.263 10.5177 0.007831 **
# Residuals 11 48.384   4.399        

# SST = 20.567+46.263+48.384=115.214
# R-squared-pred = 1 - 71.53252/115.214 = 38%

# the predictive power droppped significantly.

# getting rid of undesired data
data.3.1.c <- data.3.1[-c(7,8,9,11,17,26),]
# fit linear model and calculate PRESS stat
lm.3.1.c = lm(y ~ x2+x8, data=data.3.1.c)

PRESS.part <-sum((residuals(lm.3.1.c)/(1 - lm.influence(lm.3.1.c)$hat))^2) 
#84.16202
anova(lm.3.1.c)

# Response: y
#           Df  Sum Sq Mean Sq F value    Pr(>F)    
# x2         1  63.664  63.664  17.813 0.0004631 ***
# x8         1 155.702 155.702  43.564  2.57e-06 ***
# Residuals 19  67.907   3.574 

# SST = 63.664+155.702+67.907 = 287.273
# R-squared-pred = 1 - 84.16202/287.273 = 70.7%

# the predictive power remains at the same level. 


#11.2
# adopt an equal data split scheme:

train<-sample(1:(nrow(data.3.1)/2))
data.train <- data.3.1[train,]
data.test <- data.3.1[-train,]
# evaluate the properties of the two data sets:

summary(data.train)
bestmod <- regsubsets(y~., data=data.train, nbest=5)
summary(bestmod)
summary(bestmod)$rss
summary(bestmod)$adjr2
summary(bestmod)$cp
# number of regressors                 subset selected    rss   adjr2     cp
#                    1                            {x8}   62.7  0.4023  22.49
#                    2                        {x2, x8}   36.4  0.6211  10.87
#                    3                    {x2, x5, x8}   34.5  0.6054  11.87  
#                    4                {x2, x6, x7, x9}   27.6  0.6496  10.28
#                    5            {x2, x3, x6, x7, x9}   22.4  0.6801   9.59
#                    6        {x1, x2, x3, x6, x7, x9}   19.1  0.6882   9.88
#                    7    {x1, x2, x3, x4, x6, x7, x9}   13.7  0.7397   9.07 <-- this model seems to be the most adquate.
#                    8{x1, x2, x3, x4, x5, x6, x7, x8}   10.7  0.7546   9.55

lm.11.2 <- lm(y~. -x8 ,data=data.train)
# select {x1, x2, x3, x4, x6, x7, x9} as variables
preds <- predict(lm.11.2, newdata = data.test)
# compute the MSE for this model
sum((preds - data.test$y)^2) / nrow(data.test)
# 117.8244
# MSE is very large. Indicating this model performs poorly in prediction

#11.3
PRESS.11.3 <-sum((residuals(lm.11.2)/(1 - lm.influence(lm.11.2)$hat))^2) 
#106.4144
sum(anova(lm.11.2)$`Sum Sq`)
#113.7143
#R-squared-pred = 1 - 106.4144/113.7143 = 6.4%
# predictive power is very weak.

#11.11
summary(lm.11.2)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)   
# (Intercept) -1.032e+02  3.751e+01  -2.751  0.04027 * 
# x1          -4.605e-03  2.518e-03  -1.829  0.12699   
# x2           7.199e-03  1.246e-03   5.779  0.00218 **
# x3           1.627e+00  6.594e-01   2.468  0.05669 . 
# x4           1.174e-01  7.796e-02   1.506  0.19246   
# x5           1.151e-01  1.390e-01   0.828  0.44540   
# x6           2.831e-02  9.489e-03   2.983  0.03068 * 
# x7           7.759e-01  3.728e-01   2.081  0.09193 . 
# x9          -1.696e-02  5.009e-03  -3.386  0.01955 * 

# the std. errors are pretty small. 

# if model is developed using all the data instead

lm.11.11 <- lm(y~. -x8 ,data=data.3.1)
summary(lm.11.11)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.234e+01  1.336e+01  -0.924 0.367247    
# x1           2.280e-03  1.974e-03   1.155 0.262571    
# x2           4.139e-03  8.501e-04   4.869 0.000107 ***
# x3          -1.660e-01  2.237e-01  -0.742 0.467181    
# x4           3.015e-02  4.435e-02   0.680 0.504898    
# x5          -3.020e-03  4.991e-02  -0.061 0.952383    
# x6           2.824e-03  3.393e-03   0.832 0.415580    
# x7           2.523e-01  1.525e-01   1.654 0.114582    
# x9          -3.095e-03  1.321e-03  -2.342 0.030212 *  

# std. errors for some of the coefficients dropped, while increased for others. Indicating the std. errors do not vary much across models.

#11.12
bestmod2 <- regsubsets(y~., data=data.test, nbest=5)
summary(bestmod2)
summary(bestmod2)$rss
summary(bestmod2)$adjr2
summary(bestmod2)$cp
# number of regressors                 subset selected    rss adjr2    cp
#                    1                            {x1}   33.4  0.54  10.5
#                    2                        {x2, x8}   19.9  0.70   4.2
#                    3                    {x1, x2, x8}   11.5  0.81   1.1 
#                    4                {x1, x2, x3, x9}    8.3  0.85   1.1 <-- this model seems to be the most adquate
#                    5            {x2, x4, x5, x7, x8}    7.1  0.85   2.4
#                    6        {x2, x4, x5, x7, x8, x9}    6.7  0.84   4.1
#                    7    {x2, x4, x5, x6, x7, x8, x9}    6.6  0.82   6.1 
#                    8{x1, x2, x4, x5, x6, x7, x8, x9}    6.5  0.79   8.0

lm.11.12 <- lm(y~x1+x2+x3+x9, data = data.test)
summary(lm.11.12)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  8.2089155  5.8264285   1.409  0.19246    
# x1           0.0059389  0.0008762   6.778 8.11e-05 ***
# x2           0.0023294  0.0005797   4.018  0.00303 ** 
# x3          -0.4015621  0.1428741  -2.811  0.02036 *  
# x9          -0.0019429  0.0008567  -2.268  0.04953 *  

sum((predict(lm.11.2) - data.train$y)^2)/14 #0.8581318
sum((predict(lm.11.12) - data.test$y)^2)/14 #0.5937648
# MSE for this model is slightly better. but no significant improvement in MSE is noticed. 

sum((predict(lm.11.12, newdata = data.train) - data.train$y)^2)/14 #8.765535
PRESS.11.12 = sum((residuals(lm.11.12)/(1 - lm.influence(lm.11.12)$hat))^2) 
#22.88317
sum(anova(lm.11.12)$`Sum Sq`)
#80.35714

# R-squared-pred = 1 - 22.88317/80.35714 = 71.5%
# by looking at MSE for the estimation data set, the model performs poorly in prediction.
# However, the prediction R squared metric indicate the predictive power for this model is adequate.

#13.1
data.13.1 <- read.xls("data-prob-13-1.XLS")
glm.13.1 <- glm(y~x, family=binomial, data=data.13.1)
summary(glm.13.1)
# Coefficients:
#              Estimate Std. Error z value Pr(>|z|)   
# (Intercept)  6.070884   2.108996   2.879  0.00399 **
# x           -0.017705   0.006076  -2.914  0.00357 **

# pihat = 1/(1+e^(6.07-0.0177x)) is the model.

# compute deviance:
anova(glm.13.1, test="Chisq")
#      Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
# NULL                    24     34.617              
# x     1   14.254        23     20.364 0.0001597 ***
# deviance is 14.254 and p value is small. the model is adequate.

# when x = 0, pi = 1/(1 + exp(-6.07)) = 99.77%
# when x = 1, pi = 1/(1 + exp(-6.07+0.177)) = 99.72%
# so for every one increase in the target speed in the unit of Knots, the probability of hitting the target decreases by 0.05%.

data.13.1$x2<- data.13.1$x^2
glm.13.1b <- glm(y~., family=binomial, data=data.13.1)
anova(glm.13.1b, test="Chisq")
#      Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
# NULL                    24     34.617              
# x     1  14.2537        23     20.364 0.0001597 ***
# x2    1   0.0002        22     20.363 0.9889213 
# the deviance change for x2 is very small. P value is very large. Which means this quadratic term is not required in the model. 

#13.2 
data.13.2 <- read.xls("data-prob-13-2.XLS")
# fit logistic regression model
glm.13.2 <- glm(y~x, family=binomial, data=data.13.2)
summary(glm.13.2)
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)  
# (Intercept) -8.7395139  4.4394326  -1.969   0.0490 *
# x            0.0002009  0.0001006   1.998   0.0458 *

# pihat = 1/(1+e^(-8.74-0.0002x)) is the model.

# compute deviance:
anova(glm.13.2, test="Chisq")
#      Df Deviance Resid. Df Resid. Dev Pr(>Chi)  
# NULL                    19     27.526           
# x     1   5.0906        18     22.435  0.02406 *
# p value is small. the model is adequate

# when x = 0, pi = 1/(1 + exp(-8.74)) = 99.984%
# when x = 1000, pi = 1/(1 + exp(-8.74-0.0002*1000)) = 99.987%
# so for every one thousand increase in the household income, the probability of home ownership increase by about 0.003%. 

data.13.2$x2<- data.13.2$x^2
glm.13.2b <- glm(y~., family=binomial, data=data.13.2)
anova(glm.13.2b, test="Chisq")
#      Df Deviance Resid. Df Resid. Dev Pr(>Chi)  
# NULL                    19     27.526           
# x     1   5.0906        18     22.435  0.02406 *
# x2    1   1.1086        17     21.326  0.29239  
# the deviance change for x2 is small. P value is large. Which means this quadratic term is not required in the model.


