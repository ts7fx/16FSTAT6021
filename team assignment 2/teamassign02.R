
###########################
#                         #
#   Team Assignment 2     #
#                         #
###########################

#   Team 2     
#   Hampton Leonard, hll4ce
#   Tyler Worthington, tjw4ry
#   Andrew Pomykalski, ajp5sb
#   Tianye Song, ts7fx

#################
## Question 1: ##
#################

# For this problem you will use the data in the file "teamassign02data01.csv" to implement   
# a simple form of the bootstrap resampling method.  Repeat (a) and (b) 1000 times:
#
#   (a) From the data set, select **with replacement** 100 random pairs (x,y).
#       You will have some repeats -- which is OK and expected.
d <- read.csv("teamassign02data01.csv")
t <-d[sample(c(1:100),100,replace = TRUE), ]

#   (b) Use your sample to generate a regression equation. Save the values of 
#       hat(beta_0) and hat(beta_1).

lm.1<-lm(y~x, data=t)
hat.b0 = coef(lm.1)[1]
hat.b1 = coef(lm.1)[2]

# repeat a&b for 1000 times:

b0vec<-vector()
b1vec<-vector()
lm.list <- list()
for (i in 1:1000){
  # for each iteration, re-sample data & regenerate linear model
  temp <-d[sample(c(1:100),100,replace = TRUE), ]
  lm.temp <- lm(y~x, data=temp)
  # store coefficients of each linear model into corresponding location in vectors initiated outside of this for-loop
  b0vec[i] = coef(lm.temp)[1]
  b1vec[i] = coef(lm.temp)[2]
  lm.list[[i]] = lm.temp
}

#   (c) Find and report a 95% confidence interval for beta_0 and beta_1 by determining
#       the 2.5th and 97.5th percentiles for each set of values.  Do the confidence 
#       intervals contain the true parameter values?

CIB0 <- quantile(b0vec, c(.025, .975)) 
CIB1 <- quantile(b1vec, c(.025, .975)) 
# find the true coefficients by doing linear regression on the population
lm.pop <- lm(y~x, data=d)
coef(lm.pop)
# 
# for B0, true value is 21.27
# The confidence interval does contain the true parameter value
# 2.5%        97.5%     
# (13.820516, 30.035864)
# for B1, true value is 4.07
# The confidence interval does contain the true parameter value
# 2.5%        97.5% 
# (3.867869,  4.264148)

#################
## Question 2: ##
#################

# Import the data set "teamassign02data02.csv" which contains 100 sets of data for 
# the variables x1, x2, ..., x20.  Repeat (a)-(c) 100 times: 
q <- read.csv("teamassign02data02.csv")
#
#   (a) Generate 100 y values according to the model y ~ N(10, var=5^2) and pair up 
#       the y-values with corresponding rows from the data set of x-values.
y<-rnorm(100, mean=10, sd = 5)
q_ <-cbind(y,q)

#   (b) On the data set from part (a), generate a multiple regression model with
#       all of the x-values are explanatory variables.
lm.mult <- lm(y~., data = q_)
summary(lm.mult)
#   (c) Determine the number of significant explanatory variables at the 5% level.
temp <- summary(lm.mult)$coefficients[2:21,4]
length(temp)

# the fourth column of the coefficient matrix is the column of all p-values
length(subset(temp, temp<=0.05))


#   (d) Determine and report the proportion of significant variables in the 100
#       simulations. Compare this proportion with the expected theoretical value.

# initiate a vector that store the number of significant variables.
# devide the resulting vector by 20, which is the total number of explanatory variables in the model
# find the mean of that proportion then compare with the expected theoretical value. 

pvec<-vector()

for (i in 1:100){
  # for each iteration, regenerate 100 y values and pair them up with the x-values
  ytemp<-rnorm(100, mean=10, sd = 5)
  qtemp <-cbind(ytemp,q)
  
  lm.temp <- lm(y~., data = qtemp)
  tempp <- summary(lm.temp)$coefficients[2:21,4]
  pvec[i]<-length(subset(tempp, tempp<=0.05))
}
mean<-mean(pvec/20)
# 0.085 for one particular run
# the expected theoretical value is 0.05, whereas our estimated proportion is 0.085. 
# Since y was just created randomly and binded to the x matrix any relationship would simply be noise. 
# However since our cutoff is 5 percent we expect to have a false positive at least 5 percent of the time.


