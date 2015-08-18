#R script for generating various regression models & respective scatterplots
library(caret)
library(DAAG)
library(glmnet)
require(xlsx)
norm_rename=read.xlsx("normalize.xlsx",sheetName = "Sheet1")
attach(norm_rename)

#Regressions on Sociality!!

social_select = norm_rename[,-c(1:3,13:14)]
fit <- lm(Degree.of.Sociality ~ ., data=social_select)
cv.lm(df=social_select, fit, m=10) # 10 fold cross-validation

inTrain <- createDataPartition(y=norm_rename$Degree.of.Sociality,p=0.5,list=FALSE)
training_data <- norm_rename[inTrain,]
test_data <- norm_rename[-inTrain,]


#lasso regression for getting the best fit predictors
data_d <- norm_rename[,-c(1:3)]
cv <- cv.glmnet(as.matrix(data_d),data_d$Degree.of.Sociality,alpha=1,nfolds=10)
lambda_feasible <- cv$lambda.min

# fit the model
fits <- glmnet(as.matrix(data_d),data_d$Degree.of.Sociality,
               alpha=1,nlambda=100)
res1 <- predict(fits, s=min(fits$lambda),type="coefficients")
res2 <- predict(fits, s=lambda_feasible,type="coefficients")

#regression with forward selection for getting the best fit predictors
modFit <- train(Degree.of.Sociality ~ ., method = "leapforward",
                preProc = c("center", "scale"),data=training_data)

#Regressions on Mobility!!

library(DAAG)

social_select = norm_rename[,-c(1:3,13:14)]
fit <- lm(Degree.of.Mobility ~ .,data=social_select)
cv.lm(df=social_select, fit, m=10) # 10 fold cross-validation

inTrain <- createDataPartition(y=norm_rename$Degree.of.Mobility,p=0.5,list=FALSE)
training_data <- norm_rename[inTrain,]
test_data <- norm_rename[-inTrain,]

#lasso regression for getting the best fit predictors
data_d <- norm_rename[,-c(1:3)]
cv <- cv.glmnet(as.matrix(data_d),data_d$Degree.of.Mobility,alpha=1,nfolds=10)
lambda_feasible <- cv$lambda.min

# fit the model
fits <- glmnet(as.matrix(data_d),data_d$Degree.of.Mobility,
               alpha=1,nlambda=100)
res1 <- predict(fits, s=min(fits$lambda),type="coefficients")
res2 <- predict(fits, s=lambda_feasible,type="coefficients")

#regression with forward selection for getting the best fit predictors
modFit <- train(Degree.of.Mobility ~ ., method = "leapforward",
                preProc = c("center", "scale"),data=training_data)
