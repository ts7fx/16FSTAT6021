
###########################
#                         #
#   Team Assignment 7     #
#                         #
###########################

## Please submit one set of answers per team.                                  ##
## Your answers should be submitted as a .csv file per the instructions below. ##
## You should also submit your annotated R code per the instructions below.    ##
#################################################################################


# For this team assignment you will use the file "teamassign07train.csv" to develop
# a linear model using whatever methods you consider appropriate. You will then use
# the model that you have developed to predict the values of the response variable
# corresponding to the explanatory variable values given in the file
# "teamassign07test.csv". 
#
# These data are from credit card applications with the variable names and values
# changed for confidentiality. Information regarding the variables is given below.
#
# Once you have predicted the values of the response variable for the testing set,
# you should save them to a vector called predvect and write them into a .csv file 
# using the following code:
write.table(predvect, file="teamassign07preds.csv", row.names=F, col.names=F, sep=",")
#
# Your annotated R code should explain the reasoning behind your choices in 
# model selection and should be neatly organized.
#
# Your grade on this team assignment will be based on how well your model predicts
# the observed values relative to the other teams.
#
#
# List of variables and values:
# A1:  b, a.
# A2:  continuous.
# A3:	 continuous.
# A4:	 u, y, l, t.
# A5:	 g, p, gg.
# A6:	 c, d, cc, i, j, k, m, r, q, w, x, e, aa, ff.
# A7:	 v, h, bb, j, n, z, dd, ff, o.
# A8:  continuous.
# A9:	 t, f.
# A10: t, f.
# A11: continuous.
# A12: t, f.
# A13: g, p, s.
# A14: continuous.
# A15: continuous.
# A16: 0,1 (whether the application was approved or denied)
