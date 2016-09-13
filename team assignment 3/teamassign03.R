
###########################
#                         #
#   Team Assignment 3     #
#                         #
###########################

## Please submit one set of answers per team.                  ##
## Your answers may be submitted as an annotated R file.       ##
## Please submit your plots in a PDF as a separate attachment. ##
#################################################################


#################
## Question 1: ##
#################

# For this problem you will use the files "teamassign03data01.csv" and "teamassign03data02.csv" to 
# demonstrate through simulation the effects of multicollinearity on the variance of the regression 
# coefficients and how they influence the accuracy of predictions.
#
#   (a) Repeat the following 1000 times:
#       (1) Select a random sample of 100 observations from data01.
#       (2) Fit a linear model to the 100 observations using all four variables. Save the values
#           of the estimated coefficients in separate vectors.
#       (3) Use your linear model to predict the y-values given in data02 then compute the MSE
#           using these residuals. Save this value in a vector.
#       (4) Compute the standard deviation for the vectors containing the coefficients and compute
#           the mean of the vector containing the MSEs. Record these values.
#   (b) Choose a suitable variable to remove from the model. Repeat (1)-(4) given in part (a)
#       1000 times using this model.
#   (c) How do the results from parts (a)(4) and (b)(4) compare? Explain what you observe.



#################
## Question 2: ##
#################

# For this problem you will use the file "data-table-B2.XLS".
#
#   (a) Fit the model using all explanatory variables. Iteratively remove insignificant variables
#       one-by-one until the all remaining variables are significant. Which variables remain in your model?
#   (b) Compute each of the five types of residuals discussed in the textbook:
#       Residuals; Standardized residuals; Studentized residuals; PRESS residuals; R-student residuals.
#       You may use R functions.
#   (c) Use the results from part(a) to decide if there appear to be any outliers and/or high 
#       influence points.
#   (d) Produce a normal probability plot of the R-student residuals and evaluate the plot for 
#       signs of departures from normality.
#   (e) Produce plots of the R-student residuals vs. 
#       (1) the predicted values, and
#       (2) each explanatory variable.
#       What assumptions may not be satisfied? Explain.





