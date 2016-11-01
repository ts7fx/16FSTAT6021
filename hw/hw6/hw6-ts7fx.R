## Tianye Song, ts7fx
## STAT 6021, HW6
## 10/9/2016
setwd("~/Documents/16FSTAT6021/hw/hw6")
library(gdata)
library(car)
library(glmnet)
library(leaps)


# 9.7

data.9.7 <- read.xls("data-table-B3.xls")

# correlation matrix
cor(data.9.7[,2:12])
#             x1         x2 x3          x4         x5          x6         x7         x8         x9        x10        x11
# x1   1.0000000  0.9452080 NA -0.33015370 -0.6315968  0.65906008 -0.7814778  0.8551981  0.8013975  0.9456621  0.8354239
# x2   0.9452080  1.0000000 NA -0.29205832 -0.5170425  0.77190992 -0.6431558  0.7973892  0.7176056  0.8834004  0.7266835
# x3          NA         NA  1          NA         NA          NA         NA         NA         NA         NA         NA
# x4  -0.3301537 -0.2920583 NA  1.00000000  0.3737462 -0.04933889  0.4938104 -0.2581079 -0.3187643 -0.2772185 -0.3683612
# x5  -0.6315968 -0.5170425 NA  0.37374620  1.0000000 -0.20535194  0.8428620 -0.5481227 -0.4343576 -0.5424247 -0.7032485
# x6   0.6590601  0.7719099 NA -0.04933889 -0.2053519  1.00000000 -0.3005751  0.4251881  0.3156727  0.5206424  0.4173378
# x7  -0.7814778 -0.6431558 NA  0.49381043  0.8428620 -0.30057509  1.0000000 -0.6630802 -0.6682373 -0.7178265 -0.8549981
# x8   0.8551981  0.7973892 NA -0.25810785 -0.5481227  0.42518809 -0.6630802  1.0000000  0.8849771  0.9475859  0.6863079
# x9   0.8013975  0.7176056 NA -0.31876434 -0.4343576  0.31567268 -0.6682373  0.8849771  1.0000000  0.9015431  0.6507213
# x10  0.9456621  0.8834004 NA -0.27721850 -0.5424247  0.52064243 -0.7178265  0.9475859  0.9015431  1.0000000  0.7722283
# x11  0.8354239  0.7266835 NA -0.36836123 -0.7032485  0.41733783 -0.8549981  0.6863079  0.6507213  0.7722283  1.0000000

# multiple of the off-diagonal terms are close to 1. The correlation matrix tells us that there is potential multicollinearity.

kappa(lm.9.7)
# condition number k = 114075.5 exceeds 1000, which indicates that there is at least one strong near-linear dependence in the data set. 

# VIF
lm.9.7 <- lm(y~., data=data.9.7)
vif(lm.9.7)
#         x1         x2         x3         x4         x5         x6         x7         x8         x9        x10        x11 
# 119.487804  42.800811 149.234409   2.060036   7.729187   5.324730  11.761341  20.917632   9.397108  85.744344   5.145052 

# multiple terms in the VIF are large. Thus multicollinearity is present. 

# 9.13
data.9.13 <- read.xls("data-table-B18.xls")
lm.9.13 <- lm(y.~., data=data.9.13)
kappa(lm.9.13)
# condition number is 416182.3, which indicates at least one strong near-linear dependence in the data
vif(lm.9.13)
#       x1         x2         x3         x4         x5         x6         x7         x8 
# 1.000000   1.900541 168.467420  43.103776  60.791320 275.472571 185.707184  44.363364 
# multiple terms in the VIF are large. Thus multicollinearity is present. 

# 9.14
data.9.14 <- read.xls("data-table-B19.xls")
lm.9.14 <- lm(y~., data=data.9.14)
summary(lm.9.14)
# Coefficients: (2 not defined because of singularities)
#             Estimate Std. Error t value Pr(>|t|)  
# (Intercept) -12.20843   14.61153  -0.836   0.4120  
# x1           -0.84577    0.58596  -1.443   0.1624  
# x2            7.41839    3.51235   2.112   0.0457 *
# x3            0.01046    0.00857   1.220   0.2347  
# x4           -1.94732    2.22110  -0.877   0.3897  
# x5            4.89518    3.21850   1.521   0.1419  
# x6           -1.43382    1.81263  -0.791   0.4370  
# x7                 NA         NA      NA       NA  
# x8          -11.42517    7.88120  -1.450   0.1606  
# x9           -0.10802    0.22040  -0.490   0.6287  
# x10                NA         NA      NA       NA 

# take out x7 and x10
lm.9.14 <- lm(y~.-x7-x10, data=data.9.14)
kappa(lm.9.14)
# condition number is 9903.572, which indicates at least one strong near-linear dependence in the data
vif(lm.9.14)
#       x1         x2         x3         x4         x5         x6         x8         x9 
# 1.970605   4.092086   4.513202 603.518791 511.870261  33.319560   7.930630  36.170717 
# multiple terms in the VIF are large. Thus multicollinearity is present. 

# 9.15
data.9.15 <- read.xls("data-table-B20.xls")
lm.9.15 <- lm(X.y~., data=data.9.15)
kappa(lm.9.15)
# condition number is 71435.55, which indicates at least one strong near-linear dependence in the data
vif(lm.9.15)
#       x1        x2        x3        x4        x5 
# 1.519064 26.283999 26.447032  2.202201  1.922689 

# vif for x2 and x3 are larger than 10. thus multicollinearity is present.

#9.19
data.9.19 <- na.omit(read.xls("data-table-B3.xls"))
x.9.19 <- model.matrix(y~.,data.9.19)[,-1]
y.9.19 <-data.9.19$y

# fit ridge regression model to the data
ridge.9.19 <- glmnet(x.9.19, y.9.19, alpha=0)
# plot the ridge trace plot
plot(ridge.9.19,xvar="lambda",label=TRUE)
# from the ridge trace plot, roughly we can see the direction of changes in coefficients stop around log(lambda) = 3. 
# thus, take lambda value of exp(3), which is 20.1
ridge.9.19b <- glmnet(x.9.19, y.9.19, alpha=0, lambda = 20.1)
coef(ridge.9.19b)
ridge.y<- predict.glmnet(ridge.9.19b, x.9.19)
# residual sum of squares for ridge regression
sum((y.9.19-ridge.y)^2)
# 352
# residual sum of squares for linear model
lm.9.19<-lm(y~., data = data.9.19)
sum((lm.9.19$residuals)^2)
# 187
# the RSS increased by (352-187)/187 = 0.882, which is an 88.2% increase.
ybar <- mean(y.9.19)
Rsquared <- sum((ridge.y-ybar)^2)/sum((y.9.19-ybar)^2)
print(Rsquared)
#0.373
summary(lm.9.19)$r.squared
#0.835
# r-squared dropped by (0.835-0.373)/0.835=0.553, which is a 55.3% decrease.

#alternative approach:
lambdas<-seq(0,1,0.01)
ridge.9.19tt <- glmnet(x.9.19, y.9.19, alpha=0,lambda = lambdas)
# plot the ridge trace plot
plot(ridge.9.19tt,xvar="lambda",label=TRUE)


#9.20
coef<-as.matrix(lm.9.19$coefficients)
anova(lm.9.19)
# Response: y
#           Df Sum Sq Mean Sq F value  Pr(>F)    
# x1         1    866     866   83.23 3.6e-08 ***
# x2         1      6       6    0.54   0.473    
# x3         1      4       4    0.36   0.559    
# x4         1     16      16    1.50   0.237    
# x5         1      2       2    0.21   0.649    
# x6         1      9       9    0.90   0.354    
# x7         1      7       7    0.66   0.426    
# x8         1      0       0    0.02   0.902    
# x9         1     33      33    3.18   0.091 .  
# x10        1      8       8    0.77   0.393    
# x11        1      0       0    0.04   0.835    
# Residuals 18    187      10   

# MSE is 10, so sigma^2_hat = 10.
k<-12*10/t(coef)%*%coef
print(k)
# 0.345 is the k value chosen by the formula, which differes drastically from what is obtained in the previous question. 

# 10.1
data.10.1 <- read.xls("data-table-B1.xls")

# initiate null model for forward and full model for backward selection
lm.10.1.null <- lm(y~1, data=data.10.1)
lm.10.1.full <- lm(y~., data=data.10.1)
# forward selection
step(lm.10.1.null,scope=list(lower=lm.10.1.null, upper=lm.10.1.full), direction="forward")

# lm(formula = y ~ x8 + x2 + x7 + x9, data = data.10.1)
# 
# Coefficients:
# (Intercept)           x8           x2           x7           x9  
#    -1.82170     -0.00401      0.00382      0.21689     -0.00163 
# forward selection selected regressor subset of {x8, x2, x7, x9}

# backward elimination

step(lm.10.1.full, scope=list(lower=lm.10.1.null, upper=lm.10.1.full), direction="backward")
# lm(formula = y ~ x2 + x7 + x8 + x9, data = data.10.1)
# 
# Coefficients:
#   (Intercept)           x2           x7           x8           x9  
#      -1.82170      0.00382      0.21689     -0.00401     -0.00163 

# backward elimination selected same regressor subset of {x2, x7, x8, x9}

# stepwise selection
step(lm.10.1.null, scope=list(lower=lm.10.1.null, upper=lm.10.1.full), direction="both")

#lm(formula = y ~ x8 + x2 + x7 + x9, data = data.10.1)

# Coefficients:
#   (Intercept)           x8           x2           x7           x9  
#      -1.82170     -0.00401      0.00382      0.21689     -0.00163

# stepwise selection selected the same regressor subset of {x8, x2, x7, x9}

# the three procedures have chosen exactly the same regressor subsets. 

# 10.2
# initiate a partial model with y~x1+x2+x4+x7+x8+x9
lm.10.2 <- lm(y~x1+x2+x4+x7+x8+x9, data=data.10.1)
# implement exhaustive subset selection strategy
bestmod <- regsubsets(y~x1+x2+x4+x7+x8+x9, data=data.10.1, nbest=5)
summary(bestmod)
summary(bestmod)$rss
summary(bestmod)$adjr2
summary(bestmod)$cp

# number of regressors          subset selected    rss   adjr2     cp
#                    1                     {x8}  148.9  0.5272  26.47
#                    2                 {x2, x8}   83.9  0.7227   6.46
#                    3              {x2, x7 x8}   69.9  0.7596   3.69  <---- this model seems to be the most adquate. 
#                    4         {x2, x7, x8. x9}   65.0  0.7666   4.04
#                    5     {x1, x2, x7, x8, x9}   63.1  0.7630   5.41
#                    6 {x1, x2, x4, x7, x8, x9}   61.9  0.7564   7.00

# recommend subset {x2, x7 x8}


# 10.14
data.10.14 <- read.xls("data-table-B11.xls")
# modify region information by adding two indicator variables
# region  r1 r2
#      1   0  0
#      2   0  1
#      3   1  0
data.10.14$r1 = as.numeric(data.10.14$Region == 3)
data.10.14$r2 = as.numeric(data.10.14$Region == 2)
# apply the exhaustive all subsets approach
bestmod.10.14 <- regsubsets(Quality~.-Region, data=data.10.14, nbest=5)
# look at CP values for subset selection
summary(bestmod.10.14)
summary(bestmod.10.14)$cp
# number of regressors                                  subset selected    cp
#                    1                                         {Flavor} 35.42
#                    2                                     {Flavor, r1}  9.39
#                    3                                 {Flavor, r2, r1}  2.47  <-- 2nd best model in terms of cp
#                    4                       {Flavor, Oakiness, r2, r1}  2.24  <-- best model in terms of cp
#                    5                {Aroma, Flavor, Oakiness, r2, r1}  4.10  
#                    6          {Aroma, Body, Flavor, Oakiness, r2, r1}  6.00
#                    7 {Clarity, Aroma, Body, Flavor, Oakiness, r2, r1}  8.00

# first choice: Quality ~ Flavor+r2+r1
lm.10.14.a <- lm(Quality ~ Flavor+r2+r1, data= data.10.14)
# second choice: Quality ~ Flavor+Oakiness+r2+r1
lm.10.14.b <- lm(Quality ~ Flavor+Oakiness+r2+r1, data= data.10.14)

summary(lm.10.14.b)
plot(lm.10.14.a$residuals)
plot(lm.10.14.b$residuals)
# the residual plots look similar. Thus there is no basis for selecting based on residual plots.

# looking at PRESS stats
PRESSa <- residuals(lm.10.14.a)/(1 - lm.influence(lm.10.14.a)$hat)
PRESSb <- residuals(lm.10.14.b)/(1 - lm.influence(lm.10.14.b)$hat)
plot(PRESSa)
plot(PRESSb)
# PRESS statistics look similar as well, with the second choice model being more spreaded out. However, no significant differences are observed. Thus cannot establish any basis for model selection. 

# 10.15
# initiate both null and full model
lm.null.10.15 <- lm(Quality~1, data = data.10.14)
lm.full.10.15 <- lm(Quality~.-Region, data = data.10.14)
# implement stepwise subset selection approach
step(lm.null.10.15, scope=list(lower=lm.null.10.15, upper=lm.full.10.15), direction="both")
# final regressor subset selected:
# lm(formula = Quality ~ Flavor + r2 + r1 + Oakiness, data = data.10.14)
# the model selected by stepwise selection is identical to the second best model selected above in question 10.14. 
# this may illustrate that other than the lowest CP value, there are other indeces that we should take into consideration when selecting regressor subsets.

# 10.16
# apply exhaustive all-subset approach, only exclude region information
bestmod.10.16 <- regsubsets(Quality~.-Region-r1-r2, data=data.10.14, nbest=5)
# look at CP values for subset selection
summary(bestmod.10.16)
summary(bestmod.10.16)$cp
# number of regressors                                  subset selected    cp
#                    1                                         {Flavor}  9.04
#                    2                               {Flavor, Oakiness}  6.81
#                    3                        {Aroma, Flavor, Oakiness}  3.93  <-- best model in terms of cp
#                    4               {Clarity, Aroma, Flavor, Oakiness}  4.67  <-- 2nd best model in terms of cp
#                    5         {Clarity, Aroma, Body, Flavor, Oakiness}  6.00  

# comparing the best models in terms of CP without Region information included with the models obtained from 10.14, the models obtained in this problem performed slightly worse. However, the absolute value of decrease in CP is small, despite the percentage drop being significant. I wouldn't say there is a substantial drop in model performance. The drop is moderate, or even slight. 
# compute CI for mean quality for both models
# fit best model obtained in previous step
lm.10.16<- lm(Quality~Aroma+Flavor+Oakiness, data = data.10.14)

# compute CIs for all observations
CI1 <- predict(lm.10.14.a,data.10.14,interval="confidence") 
CI2 <- predict(lm.10.16,data.10.14,interval="confidence") 

# compute mean for all CIs obtained from above
CI1.mean<- c(mean(CI1[,2]),mean(CI1[,3]))
CI2.mean<- c(mean(CI2[,2]),mean(CI2[,3]))

# CI1.mean is (11.9, 13.0) where as CI2.mean is (11.7, 13.2)
# the length for CI1.mean is smaller, which means model1 obtained in 10.14 is superior and more precise in estimating mean y (Quality). 


