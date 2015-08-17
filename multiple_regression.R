#R script for generating various regression models & respective scatterplots

require(xlsx)
norm_rename=read.xlsx("normalize.xlsx",sheetName = "Sheet1")
attach(norm_rename)

library(ISLR)
attach(Auto)


#Regressions on Sociality!!

library(DAAG)
library(caret)
social_select = norm_rename[,-c(1:3,13:14)]
fit <- lm(Degree.of.Sociality ~ ., data=social_select)
cv.lm(df=social_select, fit, m=10) # 10 fold cross-validation
