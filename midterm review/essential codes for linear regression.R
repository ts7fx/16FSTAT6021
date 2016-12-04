# ESSENTIAL CODES FOR LINEAR REGRESSION ANALYSIS
# this file contains all the essential r codes that the author deemed as crucial for success in the midterm exam of the linear regression analysis course;
# hw1 - hw7 are the domain from which the codes were extracted and summarized.
setwd("~/Documents/git/16FSTAT6021/midterm review/data")
library(gdata)
library(readr)
library(car)
library(MASS)
# 1.  simple linear regression
# a.  Significance of regression
      football <- read.xls("data-table-B1.XLS")
      lm_football <- lm(y~x8,data=football)
      anova(lm_football)
      summary(lm_football)
      # Analysis of Variance Table
      # Response: y
      #           Df Sum Sq Mean Sq F value    Pr(>F)    
      # x8         1 178.09 178.092  31.103 7.381e-06 ***      // 7.381e-06 *** 
      # Residuals 26 148.87   5.726  
      # Summary statistics
      # F-statistic:  31.1 on 1 and 26 DF,  p-value: 7.381e-06 // 7.381e-06 same as above. 
      # comparing with alpha value, this regression is significant. 
# b.  Finding confidence intervals & prediction intervals
      confint(lm_football, level=0.95)
      #                    2.5 %       97.5 %
      # (Intercept) 16.246064040 27.330437725
      # x8          -0.009614347 -0.004435854
      football2 <- data.frame(x8 = 2000)
      predict(lm_football, football2, interval="confidence")
      #       fit      lwr      upr
      # 1 7.73805 6.765753 8.710348
      football3 <- data.frame(x8 = 1800)
      predict(lm_football, football3, interval="prediction", level = 0.9)
      #       fit      lwr      upr
      # 1 9.14307 4.936392 13.34975
      # Prediction intervals must account for both the uncertainty in knowing the value of the population mean,         plus data scatter. So a prediction interval is always wider than a confidence interval. 
# c.  # performing simple linear regression analysis
      # 1. build a model
      # 2. look at the anova table, check for significance of regression
      # 3. look at p-values of coefficients
      # 4. look at R-squared
# 2.  multiple linear regression
# a. significance of regression
      # anova table and p-value of F-stat
# b. partial F-test
      lm_football_full = lm(y~x2+x7+x8, data = football)
      lm_football_reduced = lm(y ~ x2+x8, data=football) # Reduced model
      anova(lm_football_reduced, lm_football_full)  
      # H0: b7 = 0; H1: b7 != 0
      # Analysis of Variance Table
      # Model 1: y ~ x2 + x8
      # Model 2: y ~ x2 + x7 + x8
      #   Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
      # 1     25 83.938                              
      # 2     24 69.870  1    14.068 4.8324 0.03782 *  // P-value for therefore we reject H0, b7 != 0. 
      #                                                // Thus, x7 contributes to the model.
# c. CI and PI
      confint(lm_football_full, parm = 'x7')
      #         2.5 %    97.5 %
      # x7 0.01185532 0.3760651
      tempdf<-data.frame(x2=2300,x7=56,x8=2100)
      predict(lm_football_full, tempdf, interval = "confidence")
      #        fit      lwr      upr
      # 1 7.216424 6.436203 7.996645
# d. MSEs for full & partial model
      # sometimes, although a parameter is significant in the regression (p-value for coef is small), it can be         the case that that parameter contributes only slightly to the model (Mean Sq in anova table is small).          Meaning that this parameter only explains a tiny portion of the variation in the response variable.
# 3.  residual plots
      # hw4
# 4.  transformations
      football_boxcox <- football
      football_boxcox[28,1] = 0.01
      lm_football_boxcox = lm(y~x2+x7+x8, data = football_boxcox)
      boxcox(lm_football_boxcox)
      # try a ynew = y^2 transformation
      football_boxcox$ynew = (football_boxcox$y)^2
      lm_football_boxcox_ = lm(ynew~x2+x7+x8, data = football_boxcox)
      boxcox(lm_football_boxcox_)
      # plot shows that a ynewnew = sqrt(ynew) transformation is required. 
      
# 5.  looking at leverage and influence points
      # cook's d, hw5
# 7.  indicator variables
      # hw5, chapter 8 questions
# 8.  multicollinearity
      # hw6, correlation matrix, VIF, kappa(condition number), ridge, 
# 9.  variable selection and model building techniques
      # hw6, ridge, forward backward, etc
# 10. validation of regression models
      # hw7, data split, training set & test set, 
# 11. glm
      # hw7 chapter 13 questions, logistic regression
      # interpretation: book p428
      

