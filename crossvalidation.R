library(ISLR)
library(boot)
attach(Auto)
model <- glm(mpg~horsepower,data = Auto)
mse_looscv=cv.glm(Auto,model)
mse_looscv

mse_looscv$delta[1]
#LOOSCV
library(boot)
set.seed(1)
mse_looscv=NULL
for(i in 1:10){
        model = glm(mpg~poly(horsepower,i),data=Auto)
        mse_looscv[i]=cv.glm(Auto,model)$delta[1]
}

mse_looscv

#k-Fold
library(boot)
set.seed(2)
mse_kvalidation=NULL
for(i in 1:10){
        model = gslm(mpg~poly(horsepower,i),data=Auto)
        mse_kvalidation[i]=cv.glm(Auto,model)$delta[1]
}
mse_kvalidation

#linear regression calculations

library(MASS)
attach(Boston)

lm.fit = lm(medv~lstat)

summary(lm.fit)
coefficients(lm.fit)
medv.res = resid(lm.fit)
plot(medv,medv.res)
abline(0, 0)    
names(Boston)
plot(lstat,medv,main="Relationship between lstat and medv",xlab="Lstat values",
     ylab="Medv values")
abline(lm.fit,col="red")

plot(lm.fit)
cor(medv,lstat,use="pairwise.complete.obs",method="spearman")

newdata=data.frame(lstat=20.00)
predict(lm.fit,newdata,type="response") 


