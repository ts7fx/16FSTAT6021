######################################################################################
#                                                                                    #
# This data set contains real estate information for 1000 records that detail home   #
# selling price (y), home size (x1), and a quality rating (x2). There are 400        #
# missing values that are MAR.                                                       #
#                                                                                    #
######################################################################################


#######################################################
#                                                     #
# This code illustrates:                              #
#   -Maximum likelihood estimation for missing values #
#   -Multiple imputation                              #
#                                                     #
#######################################################


#######################################################
#                                                     #
# Non-exaustive list of functions for ML:             #
#   -lavaan **                                        #
#   -mnvmle                                           #
#                                                     #
# Non-exaustive list of functions for MI:             #
#   -mice **                                          #
#   -Amelia                                           #
#   -BoBooN                                           #
#   -cat                                              #
#   -Hmisc                                            #
#   -mi                                               #
#   -mitools                                          #
#   -miP                                              #
#                                                     #
#######################################################


## Read in the data
hsdata <- read.csv("Homesalesdata.csv", header=TRUE)

## Check that the data have been read in correctly
hsdata


## ML using lavaan 
##################

## Install and load package
install.packages("lavaan")
library(lavaan)

## Define the appropriate model
model <- 'y ~ b1*x1 + b2*x2'

## Fit the model using maximum likelihood incorporating the missing information
ml.fit <- sem(model=model, data=hsdata, missing='fiml', fixed.x=FALSE)

## Review results
summary(ml.fit, rsquare=TRUE)

## Look at missing data patterns and variable coverage
inspect(ml.fit,'patterns')
inspect(ml.fit,'coverage')

## Test the overall model using the Wald test
lavTestWald(ml.fit, constraints = 
                    'b1==0 
                     b2==0')

## Incorporating auxiliary variables (assume that a non-existent x3 and x4 are relevant to missingness)
library(semTools)
aux.vars <- c('x3','x4')
auxfit <- sem.auxiliary(model=ml.fit, aux=aux.vars, data=hsdata)
summary(auxfit, rsquare=TRUE)


## MI using mice
################

## Load package
library(mice)
library(lattice)

## Look at missing data patterns
md.pattern(hsdata)

## Impute the missing values
imp.hsdata <- mice(data=hsdata, m=10, method = "pmm")

## Alterate methods for imputing missing values
imp2.hsdata <- mice(data=hsdata, m=10, method = "norm.predict")

## Must convert x2 to factor for this method to work
hsdata_ <- hsdata
hsdata_$x2<- as.factor(hsdata_$x2)
imp3.hsdata <- mice(data=hsdata_, m=10, method = c("norm.predict","","polr"))

## View imputation details
summary(imp.hsdata)

## View imputed values for y
imp.hsdata$imp$y

## View fully imputed data set
complete(imp.hsdata,1)
 
## Check the distribution of the imputed values
densityplot(imp.hsdata)

## Analyze and pool the parameters
mi.fit <- with(imp.hsdata, lm(y~x1+x2))
summary(pool(mi.fit))
dim(pool(mi.fit))
class(mi.fit)
mi.fit[1]
