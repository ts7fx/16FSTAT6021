# midterm, tianye song, ts7fx, 11/22/2016
setwd("~/Documents/git/16FSTAT6021/")
library(readr)
exam <- read_csv("exam.csv")

# converting x2 and x5 to factors
exam$x2 <- as.factor(exam$x2)
exam$x5 <- as.factor(exam$x5)

# null & full model
model.null <- lm(y~1, data=exam)
model.full <- lm(y~., data=exam)

step(model.null, scope=list(lower=model.null, upper=model.full), direction="forward")
# lm(formula = y ~ x1 + x4 + x9 + x6 + x2 + x7, data = exam)
step(model.full, scope=list(lower=model.null, upper=model.full), direction="backward")
# lm(formula = y ~ x1 + x2 + x4 + x6 + x7 + x9, data = exam)
step(model.null, scope=list(lower=model.null, upper=model.full), direction="both")
# lm(formula = y ~ x1 + x4 + x9 + x6 + x2 + x7, data = exam)

model1 <- lm(y ~ x1 + x4 + x9 + x6 + x2 + x7, data = exam)
# ensure no transformation is required
boxcox(model1)

# cv
require(DAAG)
cv.lm(data=exam, form.lm=model1, m=5, plotit=F) #overall ms 275



