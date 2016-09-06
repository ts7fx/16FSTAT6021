## Tianye Song, ts7fx
## STAT 6021, HW2
## 9/6/2016
#2.20, 2.21, 2.22, 2.30 
setwd("~/Documents/16FSTAT6021/hw2")
library(gdata)

#2.20
# test if y ~ x_5
# test for linear relationship between y and x_5
# H0: B1 = 0 vs H1: B1 != 0 ?? <- is this correct??
boil <- read.xls("data-table-B18.XLS")
lm.boil <- lm(y.~X.x_5.,data=boil)
summary(lm.boil)

