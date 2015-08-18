
#R script for generating various regression models & respective scatterplots

require(rJava)
require(xlsx)
norm_rename=read.xlsx("normalize.xlsx",sheetName = "Sheet1")
attach(norm_rename)


#Changing resampling method to Repeated Cross Fold Validation
library(caret)

train_control <- trainControl(## 10-fold CV
        method = "repeatedcv",
        number = 10,
        ## repeated three times
        repeats = 3)

#Regressions on Sociality!!

inTrain <- createDataPartition(y=norm_rename$Degree.of.Sociality,p=0.5,list=FALSE)
training_data <- norm_rename[inTrain,]
test_data <- norm_rename[-inTrain,]

#Regression model for predicting sociality with layman as predictor!!

y=norm_rename$Degree.of.Sociality
x=norm_rename$Degree.of.Layman
cor(y,x)
yc = y - mean(y)
xc = x - mean(x)
beta1 = sum(yc*xc)/sum(xc^2)
beta1_n = cor(y,x)* sd(y)/sd(x)
beta0 = mean(y) - beta1*mean(x)
c(beta0,coef(lm(y~x))[1])
c(beta1,coef(lm(y~x))[2])
lm(yc~xc - 1)

library(ggplot2)
g = ggplot(norm_rename, aes(y=norm_rename$Degree.of.Sociality,x=norm_rename$Degree.of.Layman))
g = ggplot(norm, aes(y=norm_rename$Degree.of.Sociality,x=norm_rename$Degree.of.Layman))
g = g + geom_point(size=12,colour="black",alpha=0.2)
g = g + geom_point(size=12,colour="blue",alpha=0.2)
g = g + geom_smooth(method="lm",formula=y~x,colour="black")
g

plot(norm_rename$Degree.of.Layman,norm_rename$Degree.of.Sociality)
abline(lm(norm_rename$Degree.of.Sociality~norm_rename$Degree.of.Layman))

#Using Data Splitting & repeatedcv method for model accuracy

modFit = train(Degree.of.Sociality~Degree.of.Layman,method="lm",trControl=train_control,data=trainig_data)
finMod = modFit$finalModel
predicted_sociality = predict(finMod,test_data)
qplot(Degree.of.Sociality,predicted_sociality,colour=Category,data=test_data)
plot(finMod)
qplot(finMod$fitted,finMod$residuals,colour=Category,data=training_data)

plot(norm$Degree.of.Layman,norm_rename$Degree.of.Sociality)
abline(lm(norm_rename$Degree.of.Sociality~norm$Degree.of.Layman))


#Regression model for predicting sociality with operator as predictor!!

y=norm_rename$Degree.of.Sociality
x=norm_rename$Degree.of.Operator
cor(y,x)
yc = y - mean(y)
xc = x - mean(x)
beta1 = sum(yc*xc)/sum(xc^2)
beta1_n = cor(y,x)* sd(y)/sd(x)
beta0 = mean(y) - beta1*mean(x)
c(beta0,coef(lm(y~x))[1])
c(beta1,coef(lm(y~x))[2])
lm(yc~xc - 1)

library(ggplot2)

g = ggplot(norm_rename, aes(y=norm_rename$Degree.of.Sociality,x=norm_rename$Degree.of.Operator))
g = ggplot(norm, aes(y=norm_rename$Degree.of.Sociality,x=norm_rename$Degree.of.Operator))
g = g + geom_point(size=12,colour="black",alpha=0.2)
g = g + geom_point(size=12,colour="blue",alpha=0.2)
g = g + geom_smooth(method="lm",formula=y~x,colour="black")
g

plot(norm_rename$Degree.of.Operator,norm_rename$Degree.of.Sociality)
abline(lm(norm_rename$Degree.of.Sociality~norm_rename$Degree.of.Operator))

#Using Data Splitting & repeatedcv method for model accuracy

modFit = train(Degree.of.Sociality~Degree.of.Operator,trControl=train_control,method="lm",data=trainig_data)
finMod = modFit$finalModel
plot(finMod)

qplot(finMod$fitted,finMod$residuals,colour=Category,data=training_data)
predicted_sociality = predict(finMod,test_data)
qplot(Degree.of.Sociality,predicted_sociality,colour=Category,data=test_data)


plot(norm$Degree.of.Operator,norm_rename$Degree.of.Sociality)
abline(lm(norm_rename$Degree.of.Sociality~norm$Degree.of.Operator))


#Regression model for predicting sociality with expert as predictor!!

y=norm_rename$Degree.of.Sociality
x=norm_rename$Degree.of.Expert
cor(y,x)
yc = y - mean(y)
xc = x - mean(x)
beta1 = sum(yc*xc)/sum(xc^2)
beta1_n = cor(y,x)* sd(y)/sd(x)
beta0 = mean(y) - beta1*mean(x)
c(beta0,coef(lm(y~x))[1])
c(beta1,coef(lm(y~x))[2])
lm(yc~xc - 1)

library(ggplot2)

g = ggplot(norm_rename, aes(y=norm_rename$Degree.of.Sociality,x=norm_rename$Degree.of.Expert))
g = ggplot(norm, aes(y=norm_rename$Degree.of.Sociality,x=norm_rename$Degree.of.Expert))
g = g + geom_point(size=12,colour="black",alpha=0.2)
g = g + geom_point(size=12,colour="blue",alpha=0.2)
g = g + geom_smooth(method="lm",formula=y~x,colour="black")
g

plot(norm_rename$Degree.of.Expert,norm_rename$Degree.of.Sociality)
abline(lm(norm_rename$Degree.of.Sociality~norm_rename$Degree.of.Expert))

#Using Data Splitting & repeatedcv method for model accuracy

modFit = train(Degree.of.Sociality~Degree.of.Expert,trControl=train_control,method="lm",data=trainig_data)
finMod = modFit$finalModel
plot(finMod)

qplot(finMod$fitted,finMod$residuals,colour=Category,data=training_data)
predicted_sociality = predict(finMod,test_data)
qplot(Degree.of.Sociality,predicted_sociality,colour=Category,data=test_data)

plot(norm$Degree.of.Expert,norm_rename$Degree.of.Sociality)
abline(lm(norm_rename$Degree.of.Sociality~norm$Degree.of.Expert))


#Regression model for predicting sociality with Time Constraint as predictor!!

y=norm_rename$Degree.of.Sociality
x=norm_rename$Degree.of.Time.Constraint
cor(y,x)
yc = y - mean(y)
xc = x - mean(x)
beta1 = sum(yc*xc)/sum(xc^2)
beta1_n = cor(y,x)* sd(y)/sd(x)
beta0 = mean(y) - beta1*mean(x)
c(beta0,coef(lm(y~x))[1])
c(beta1,coef(lm(y~x))[2])
lm(yc~xc - 1)

library(ggplot2)

g = ggplot(norm_rename, aes(y=norm_rename$Degree.of.Sociality,x=norm_rename$Degree.of.Time.Constraint))
g = ggplot(norm, aes(y=norm_rename$Degree.of.Sociality,x=norm_rename$Degree.of.Time.Constraint))
g = g + geom_point(size=12,colour="black",alpha=0.2)
g = g + geom_point(size=12,colour="blue",alpha=0.2)
g = g + geom_smooth(method="lm",formula=y~x,colour="black")
g
plot(norm_rename$Degree.of.Time.Constraint,norm_rename$Degree.of.Sociality)
abline(lm(norm_rename$Degree.of.Sociality~norm_rename$Degree.of.Time.Constraint))


#Using Data Splitting & repeatedcv method for model accuracy

modFit = train(Degree.of.Sociality~Degree.of.Time.Constraint,trControl=train_control,method="lm",data=trainig_data)
finMod = modFit$finalModel
plot(finMod)
qplot(finMod$fitted,finMod$residuals,colour=Category,data=training_data)
predicted_sociality = predict(finMod,test_data)
qplot(Degree.of.Sociality,predicted_sociality,colour=Category,data=test_data)


#Regression model to predict sociality with answer validity as a predictor

y=norm_rename$Degree.of.Sociality
x=norm_rename$Degree.of.Answer.Validity
cor(y,x)
yc = y - mean(y)
xc = x - mean(x)
beta1 = sum(yc*xc)/sum(xc^2)
beta1_n = cor(y,x)* sd(y)/sd(x)
beta0 = mean(y) - beta1*mean(x)
c(beta0,coef(lm(y~x))[1])
c(beta1,coef(lm(y~x))[2])
lm(yc~xc - 1)

library(ggplot2)

g = ggplot(norm_rename, aes(y=norm_rename$Degree.of.Sociality,x=norm_rename$Degree.of.Answer.Validity))
g = ggplot(norm, aes(y=norm_rename$Degree.of.Sociality,x=norm_rename$Degree.of.Answer.Validity))
g = g + geom_point(size=12,colour="black",alpha=0.2)
g = g + geom_point(size=12,colour="blue",alpha=0.2)
g = g + geom_smooth(method="lm",formula=y~x,colour="black")
g
plot(norm_rename$Degree.of.Answer.Validity,norm_rename$Degree.of.Sociality)
abline(lm(norm_rename$Degree.of.Sociality~norm_rename$Degree.of.Answer.Validity))


#Using Data Splitting & repeatedcv method for model accuracy

modFit = train(Degree.of.Sociality~Degree.of.Answer.Validity,trControl=train_control,method="lm",data=trainig_data)
finMod = modFit$finalModel
plot(finMod)

qplot(finMod$fitted,finMod$residuals,colour=Category,data=training_data)
predicted_sociality = predict(finMod,test_data)
qplot(Degree.of.Sociality,predicted_sociality,colour=Category,data=test_data)


#Regression model to predict sociality with costs as a predictor

y=norm_rename$Degree.of.Sociality
x=norm_rename$Degree.of.Costs
cor(y,x)
yc = y - mean(y)
xc = x - mean(x)
beta1 = sum(yc*xc)/sum(xc^2)
beta1_n = cor(y,x)* sd(y)/sd(x)
beta0 = mean(y) - beta1*mean(x)
c(beta0,coef(lm(y~x))[1])
c(beta1,coef(lm(y~x))[2])
lm(yc~xc - 1)

library(ggplot2)

g = ggplot(norm_rename, aes(y=norm_rename$Degree.of.Sociality,x=norm_rename$Degree.of.Costs))
g = ggplot(norm, aes(y=norm_rename$Degree.of.Sociality,x=norm_rename$Degree.of.Costs))
g = g + geom_point(size=12,colour="black",alpha=0.2)
g = g + geom_point(size=12,colour="blue",alpha=0.2)
g = g + geom_smooth(method="lm",formula=y~x,colour="black")
g
plot(norm_rename$Degree.of.Costs,norm_rename$Degree.of.Sociality)
abline(lm(norm_rename$Degree.of.Sociality~norm_rename$Degree.of.Costs))


#Using Data Splitting & repeatedcv method for model accuracy

modFit = train(Degree.of.Sociality~Degree.of.Costs,trControl=train_control,method="lm",data=trainig_data)
finMod = modFit$finalModel
plot(finMod)

qplot(finMod$fitted,finMod$residuals,colour=Category,data=training_data)
predicted_sociality = predict(finMod,test_data)
qplot(Degree.of.Sociality,predicted_sociality,colour=Category,data=test_data)


#Regression model to predict sociality with knowledge codification as a predictor

y=norm_rename$Degree.of.Sociality
x=norm_rename$Degree.of.Knowledge.Codification
cor(y,x)
yc = y - mean(y)
xc = x - mean(x)
beta1 = sum(yc*xc)/sum(xc^2)
beta1_n = cor(y,x)* sd(y)/sd(x)
beta0 = mean(y) - beta1*mean(x)
c(beta0,coef(lm(y~x))[1])
c(beta1,coef(lm(y~x))[2])
lm(yc~xc - 1)

library(ggplot2)

g = ggplot(norm_rename, aes(y=norm_rename$Degree.of.Sociality,x=norm_rename$Degree.of.Knowledge.Codification))
g = ggplot(norm, aes(y=norm_rename$Degree.of.Sociality,x=norm_rename$Degree.of.Knowledge.Codification))
g = g + geom_point(size=12,colour="black",alpha=0.2)
g = g + geom_point(size=12,colour="blue",alpha=0.2)
g = g + geom_smooth(method="lm",formula=y~x,colour="black")
g
plot(norm_rename$Degree.of.Knowledge.Codification,norm_rename$Degree.of.Sociality)
abline(lm(norm_rename$Degree.of.Sociality~norm_rename$Degree.of.Knowledge.Codification))


#Using Data Splitting & repeatedcv method for model accuracy

modFit = train(Degree.of.Sociality~Degree.of.Knowledge.Codification,trControl=train_control,method="lm",data=trainig_data)
finMod = modFit$finalModel
plot(finMod)

qplot(finMod$fitted,finMod$residuals,colour=Category,data=training_data)
predicted_sociality = predict(finMod,test_data)
qplot(Degree.of.Sociality,predicted_sociality,colour=Category,data=test_data)


#Regression model to predict sociality with knowledge location as a predictor

y=norm_rename$Degree.of.Sociality
x=norm_rename$Degree.of.Location.Dependency
cor(y,x)
yc = y - mean(y)
xc = x - mean(x)
beta1 = sum(yc*xc)/sum(xc^2)
beta1_n = cor(y,x)* sd(y)/sd(x)
beta0 = mean(y) - beta1*mean(x)
c(beta0,coef(lm(y~x))[1])
c(beta1,coef(lm(y~x))[2])
lm(yc~xc - 1)

library(ggplot2)

g = ggplot(norm_rename, aes(y=norm_rename$Degree.of.Sociality,x=norm_rename$Degree.of.Knowledge.Codification))
g = ggplot(norm, aes(y=norm_rename$Degree.of.Sociality,x=norm_rename$Degree.of.Knowledge.Codification))
g = g + geom_point(size=12,colour="black",alpha=0.2)
g = g + geom_point(size=12,colour="blue",alpha=0.2)
g = g + geom_smooth(method="lm",formula=y~x,colour="black")
g
plot(norm_rename$Degree.of.Knowledge.Codification,norm_rename$Degree.of.Location.Dependency)
abline(lm(norm_rename$Degree.of.Sociality~norm_rename$Degree.of.Location.Dependency))


#Using Data Splitting & repeatedcv method for model accuracy

modFit = train(Degree.of.Sociality~Degree.of.Location.Dependency,trControl=train_control,method="lm",data=trainig_data)
finMod = modFit$finalModel
plot(finMod)

qplot(finMod$fitted,finMod$residuals,colour=Category,data=training_data)
predicted_sociality = predict(finMod,test_data)
qplot(Degree.of.Sociality,predicted_sociality,colour=Category,data=test_data)


#Regressions on Mobility!!

inTrain <- createDataPartition(y=norm_rename$Degree.of.Sociality,p=0.5,list=FALSE)
training_data <- norm_rename[inTrain,]
test_data <- norm_rename[-inTrain,]

#Regression model for predicting mobility with generality as a predictor!!

x=norm_rename$Degree.of.Generality.Of.Applicability
y=norm_rename$Degree.of.Mobility
yc = y - mean(y)
xc = x - mean(x)
beta1 = sum(yc*xc)/sum(xc^2)
beta1_n = cor(y,x)* sd(y)/sd(x)
beta0 = mean(y) - beta1*mean(x)
c(beta0,coef(lm(y~x))[1])
#plot(lm(y~xc))


library(ggplot2)

g = ggplot(norm_rename, aes(y=norm_rename$Degree.of.Mobility,x=norm_rename$Degree.of.Generality.Of.Applicability))
g = ggplot(norm, aes(y=norm_rename$Degree.of.Mobility,x=norm_rename$Degree.of.Generality.Of.Applicability))

g = g + geom_point(size=6,colour="black",alpha=0.2)
g = g + geom_point(size=6,colour="blue",alpha=0.2)
g = g + geom_smooth(method="lm",formula=y~x,colour="black")
g
plot(norm_rename$Degree.of.Generality.Of.Applicability,norm_rename$Degree.of.Mobility)
abline(lm(norm_rename$Degree.of.Mobility~norm_rename$Degree.of.Generality.Of.Applicability))

#Using Data Splitting & repeatedcv method for model accuracy

modFit = train(Degree.of.Mobility~Degree.of.Generality.Of.Applicability,trControl=train_control,method="lm",data=trainig_data)
finMod = modFit$finalModel
plot(finMod)

qplot(finMod$fitted,finMod$residuals,colour=Category,data=training_data)
predicted_sociality = predict(finMod,test_data)
qplot(Degree.of.Mobility,predicted_sociality,colour=Category,data=test_data)

plot(norm_rename$Degree.of.Generality.Of.Applicability,norm$Degree.of.Mobility)
abline(lm(norm_rename$Degree.of.Mobility~norm$Degree.of.Generality.Of.Applicability))


#Regression model for predicting mobility with Time Constraint as a predictor!!

y=norm_rename$Degree.of.Mobility
x=norm_rename$Degree.of.Time.Constraint
cor(y,x)
yc = y - mean(y)
xc = x - mean(x)
beta1 = sum(yc*xc)/sum(xc^2)
beta1_n = cor(y,x)* sd(y)/sd(x)
beta0 = mean(y) - beta1*mean(x)
c(beta0,coef(lm(y~x))[1])
c(beta1,coef(lm(y~x))[2])
lm(yc~xc - 1)

library(ggplot2)

g = ggplot(norm_rename, aes(y=norm_rename$Degree.of.Mobility,x=norm_rename$Degree.of.Time.Constraint))
g = ggplot(norm, aes(y=norm_rename$Degree.of.Mobility,x=norm_rename$Degree.of.Time.Constraint))
g = g + geom_point(size=12,colour="black",alpha=0.2)
g = g + geom_point(size=12,colour="blue",alpha=0.2)
g = g + geom_smooth(method="lm",formula=y~x,colour="black")
g

plot(norm_rename$Degree.of.Time.Constraint,norm_rename$Degree.of.Mobility)
abline(lm(norm_rename$Degree.of.Mobility~norm_rename$Degree.of.Time.Constraint))

#Using Data Splitting & repeatedcv method for model accuracy

modFit = train(Degree.of.Mobility~Degree.of.Time.Constraint,trControl=train_control,method="lm",data=trainig_data)
finMod = modFit$finalModel
plot(finMod)

qplot(finMod$fitted,finMod$residuals,colour=Category,data=training_data)
predicted_sociality = predict(finMod,test_data)
qplot(Degree.of.Mobility,predicted_sociality,colour=Category,data=test_data)

plot(norm$Degree.of.Time.Constraint,norm_rename$Degree.of.Mobility)
abline(lm(norm_rename$Degree.of.Mobility~norm$Degree.of.Time.Constraint))


#Regression model to predict mobility with answer validity as a predictor

y=norm_rename$Degree.of.Mobility
x=norm_rename$Degree.of.Answer.Validity
cor(y,x)
yc = y - mean(y)
xc = x - mean(x)
beta1 = sum(yc*xc)/sum(xc^2)
beta1_n = cor(y,x)* sd(y)/sd(x)
beta0 = mean(y) - beta1*mean(x)
c(beta0,coef(lm(y~x))[1])
c(beta1,coef(lm(y~x))[2])
lm(yc~xc - 1)

library(ggplot2)

g = ggplot(norm_rename, aes(y=norm_rename$Degree.of.Mobility,x=norm_rename$Degree.of.Answer.Validity))

g = ggplot(norm, aes(y=norm_rename$Degree.of.Mobility,x=norm_rename$Degree.of.Answer.Validity))

g = g + geom_point(size=12,colour="black",alpha=0.2)
g = g + geom_point(size=12,colour="blue",alpha=0.2)
g = g + geom_smooth(method="lm",formula=y~x,colour="black")
g
plot(norm_rename$Degree.of.Answer.Validity,norm_rename$Degree.of.Mobility)
abline(lm(norm_rename$Degree.of.Mobility~norm_rename$Degree.of.Answer.Validity))


#Using Data Splitting & repeatedcv method for model accuracy

modFit = train(Degree.of.Mobility~Degree.of.Answer.Validity,trControl=train_control,method="lm",data=trainig_data)
finMod = modFit$finalModel
plot(finMod)

qplot(finMod$fitted,finMod$residuals,colour=Category,data=training_data)
predicted_sociality = predict(finMod,test_data)
qplot(Degree.of.Mobility,predicted_sociality,colour=Category,data=test_data)


#Regression model to predict mobility with costs as a predictor

y=norm_rename$Degree.of.Mobility
x=norm_rename$Degree.of.Costs
cor(y,x)
yc = y - mean(y)
xc = x - mean(x)
beta1 = sum(yc*xc)/sum(xc^2)
beta1_n = cor(y,x)* sd(y)/sd(x)
beta0 = mean(y) - beta1*mean(x)
c(beta0,coef(lm(y~x))[1])
c(beta1,coef(lm(y~x))[2])
lm(yc~xc - 1)

library(ggplot2)

g = ggplot(norm_rename, aes(y=norm_rename$Degree.of.Mobility,x=norm_rename$Degree.of.Costs))

g = ggplot(norm, aes(y=norm_rename$Degree.of.Mobility,x=norm_rename$Degree.of.Costs))

g = g + geom_point(size=12,colour="black",alpha=0.2)
g = g + geom_point(size=12,colour="blue",alpha=0.2)
g = g + geom_smooth(method="lm",formula=y~x,colour="black")
g
plot(norm_rename$Degree.of.Costs,norm_rename$Degree.of.Mobility)
abline(lm(norm_rename$Degree.of.Mobility~norm_rename$Degree.of.Costs))


#Using Data Splitting & repeatedcv method for model accuracy

modFit = train(Degree.of.Mobility~Degree.of.Costs,trControl=train_control,method="lm",data=trainig_data)
finMod = modFit$finalModel
plot(finMod)

qplot(finMod$fitted,finMod$residuals,colour=Category,data=training_data)
predicted_sociality = predict(finMod,test_data)
qplot(Degree.of.Mobility,predicted_sociality,colour=Category,data=test_data)


#Regression model to predict mobility with knowledge codification as a predictor

y=norm_rename$Degree.of.Mobility
x=norm_rename$Degree.of.Knowledge.Codification
cor(y,x)
yc = y - mean(y)
xc = x - mean(x)
beta1 = sum(yc*xc)/sum(xc^2)
beta1_n = cor(y,x)* sd(y)/sd(x)
beta0 = mean(y) - beta1*mean(x)
c(beta0,coef(lm(y~x))[1])
c(beta1,coef(lm(y~x))[2])
lm(yc~xc - 1)

library(ggplot2)

g = ggplot(norm_rename, aes(y=norm_rename$Degree.of.Mobility,x=norm_rename$Degree.of.Knowledge.Codification))

g = ggplot(norm, aes(y=norm_rename$Degree.of.Mobility,x=norm_rename$Degree.of.Knowledge.Codification))

g = g + geom_point(size=12,colour="black",alpha=0.2)
g = g + geom_point(size=12,colour="blue",alpha=0.2)
g = g + geom_smooth(method="lm",formula=y~x,colour="black")
g
plot(norm_rename$Degree.of.Knowledge.Codification,norm_rename$Degree.of.Mobility)
abline(lm(norm_rename$Degree.of.Mobility~norm_rename$Degree.of.Knowledge.Codification))


#Using Data Splitting & repeatedcv method for model accuracy

modFit = train(Degree.of.Mobility~Degree.of.Knowledge.Codification,trControl=train_control,method="lm",data=trainig_data)
finMod = modFit$finalModel
plot(finMod)

qplot(finMod$fitted,finMod$residuals,colour=Category,data=training_data)
predicted_sociality = predict(finMod,test_data)
qplot(Degree.of.Mobility,predicted_sociality,colour=Category,data=test_data)


#Regression model to predict mobility with location as a predictor

y=norm_rename$Degree.of.Mobility
x=norm_rename$Degree.of.Location.Dependency
cor(y,x)
yc = y - mean(y)
xc = x - mean(x)
beta1 = sum(yc*xc)/sum(xc^2)
beta1_n = cor(y,x)* sd(y)/sd(x)
beta0 = mean(y) - beta1*mean(x)
c(beta0,coef(lm(y~x))[1])
c(beta1,coef(lm(y~x))[2])
lm(yc~xc - 1)

library(ggplot2)

g = ggplot(norm_rename, aes(y=norm_rename$Degree.of.Mobility,x=norm_rename$Degree.of.Location.Dependency))

g = ggplot(norm, aes(y=norm_rename$Degree.of.Mobility,x=norm_rename$Degree.of.Location.Dependency))

g = g + geom_point(size=12,colour="black",alpha=0.2)
g = g + geom_point(size=12,colour="blue",alpha=0.2)
g = g + geom_smooth(method="lm",formula=y~x,colour="black")
g
plot(norm_rename$Degree.of.Location.Dependency,norm_rename$Degree.of.Mobility)
abline(lm(norm_rename$Degree.of.Mobility~norm_rename$Degree.of.Location.Dependency))


#Using Data Splitting & repeatedcv method for model accuracy

modFit = train(Degree.of.Mobility~Degree.of.Location.Dependency,trControl=train_control,method="lm",data=trainig_data)
finMod = modFit$finalModel
plot(finMod)

qplot(finMod$fitted,finMod$residuals,colour=Category,data=training_data)
predicted_sociality = predict(finMod,test_data)
qplot(Degree.of.Mobility,predicted_sociality,colour=Category,data=test_data)


