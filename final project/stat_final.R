## STAT6021 final project
## do you get a loan or not?

#   Hampton Leonard, hll4ce
#   Tyler Worthington, tjw4ry
#   Andrew Pomykalski, ajp5sb
#   Tianye Song, ts7fx
library(readr)
# https://www.lendingclub.com/info/download-data.action  <-- download data
setwd("~/Downloads")


declined <- read_csv('RejectStatsD.csv', skip = 1) # skip the first line where there's a note. 
head(declined)
# proportion of missing values
sum(is.na(declined$Risk_Score)) / nrow(declined) # 82.15%
sum(is.na(declined$`Amount Requested`)) / nrow(declined) # none missing
sum(is.na(declined$`Loan Title`)) / nrow(declined) # none missing
sum(is.na(declined$`Debt-To-Income Ratio`)) / nrow(declined) # none missing
sum(is.na(declined$State)) / nrow(declined) # none missing
