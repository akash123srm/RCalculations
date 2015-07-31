library(ISLR)
library(tree)
"""attach(Carseats)
range(Sales)
High <- ifelse(Sales>=8,"Yes","No")
Carseats=data.frame(Carseats,High)_"""
attach(Smarket)
cor(Smarket[,-9])
training=(Year<2005)
testing=!training
training_data=Smarket[training,]
testing_data=Smarket[testing,]
Direction_testing=Direction[testing]

stock_model = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=training_data,
                  family=binomial)
summary(stock_model)

model_pred_probs=predict(stock_model,testing_data,type="response")
model_pred_probs

model_pred_Direction=rep("Up",252)
model_pred_Direction[model_pred_probs < 0.5] = "Down"
model_pred_Direction

#Create a confusion matrix to assess the stability of our model

table(model_pred_Direction,Direction_testing)

