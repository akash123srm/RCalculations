#R script for calculating the correlations between different axes & regression models

require(gdata)
data_load = read.csv(file.choose(),header=TRUE)
attach(data_load)

#Rename the column
names(data_load)[1]="Website"
     
col_select = data_load[, c("Website","Category","Time.Constraint","Answer.Validity","Generality.Of.Applicability",
                     "Location.Dependency","Knowledge.Codification","Costs.Category","Information.Provider.Layman",
                     "Information.Provider.Operator","Information.Provider.Expert","Mobile.Context","Spatial.Coordinates",
                     "Ask.Questions","Give.Suggestions","Rate.or.Comment","Create.Personal.Profile",
                     "Others.Information.Needs","Contact.Other.Users")]

#Calculating the mean across different fields

agg=aggregate(col_select[, 3:11],list(Website=col_select$Website,Category=col_select$Category),mean)

#Processing for the Mobility Fields

agg_mobile=aggregate((col_select$Mobile.Context + col_select$Spatial.Coordinates),
              list(Website=col_select$Website,Category=col_select$Category),mean)

#Processing for the Sociality Fields

agg_social = aggregate((col_select$Ask.Questions + col_select$Give.Suggestions + 
                        col_select$Rate.or.Comment +
                        col_select$Create.Personal.Profile + col_select$Others.Information.Needs +
                        col_select$Contact.Other.Users),
                       list(Website=col_select$Website,Category=col_select$Category),mean)

#Function to normalize the mean

normalize <- function(){
        dr = data[1:18,c()]
        dr[1] = agg[1]
        dr[2] = agg[2]
        for(i in 3:5){
                dr[i] = agg[i]/2.0
               }
        dr[6] = agg[6]
        dr[7] = agg[7]
        dr[8] = agg[8]/2.0
        dr[9] = agg[9]
        dr[10] = agg[10]
        dr[11] = agg[11]
        dr[12] = agg_mobile[3]/2.0
        dr[13] = agg_social[3]/6.0
        return(dr)
        }
norm=normalize()

#Rename some columns

names(norm)[3] = "Degree.of.Time.Constraint"
names(norm)[4] = "Degree.of.Answer.Validity"
names(norm)[5] = "Degree.of.Generality.Of.Applicability"
names(norm)[6] = "Degree.of.Location.Dependency"
names(norm)[7] = "Degree.of.Knowledge.Codification"
names(norm)[8] = "Degree.of.Costs"
names(norm)[9] = "Degree.of.Layman"
names(norm)[10] = "Degree.of.Operator"
names(norm)[11] = "Degree.of.Expert"
names(norm)[12] = "Degree.of.Mobility"
names(norm)[13] = "Degree.of.Sociality"

norm_rename = norm
library(xlsx)
write.xlsx(norm,file="test.xlsx")

'''dr = data[,c()]

for(i in 1:length(col_select)){
        dr[i]=as.double(strsplit(as.character(col_select[[i]]),"%"))   
}'''

#format(round(x, 2), nsmall = 2)
#Function to calculate the Pearson Correlation between different axis

calc_pearson <- function(x){
        calc=format(round(cor(x[sapply(x, is.numeric)],use="pairwise.complete.obs",method="pearson"),4),nsmall=4)
        return(calc)
}
calc_pearson(norm)
library(xlsx)
write.xlsx(calc_pearson(norm),file="pearson.xlsx")

#Function to calculate the Spearman Correlation between different axis

calc_spearman <- function(x){
        calc=format(round(cor(x[sapply(x, is.numeric)],use="pairwise.complete.obs",method="spearman"),4),nsmall=4)
        return(calc)
}
calc_spearman(norm)
library(xlsx)
write.xlsx(calc_spearman(norm),file="spearman.xlsx")

#Function to calculate the mean of selected columns

calc_mean <- function(y){
        no_columns = ncol(y)
        means = vector()
        for(i in 1:no_columns){
                means[i] = mean(y[, i],na.rm = TRUE)
        }
        return(means)
}
calc_mean(norm)
library(xlsx)
write.xlsx(calc_mean(dr),file="mean.xlsx")

#Function to generate the scatterplots among the axis

gen_plot <- function(y){
        plots=plot(y,title="Relationship between different axes")
}
gen_plot(norm)

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
g = ggplot(norm, aes(y=norm_rename$Degree.of.Mobility,x=norm_rename$Degree.of.Generality.Of.Applicability))
g = g + geom_point(size=6,colour="black",alpha=0.2)
g = g + geom_point(size=6,colour="blue",alpha=0.2)
g = g + geom_smooth(method="lm",formula=y~x,colour="black")
g
plot(norm_rename$Degree.of.Mobility,norm$Degree.of.Generality.Of.Applicability)
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
g = ggplot(norm, aes(y=norm_rename$Degree.of.Mobility,x=norm_rename$Degree.of.Time.Constraint))
g = g + geom_point(size=12,colour="black",alpha=0.2)
g = g + geom_point(size=12,colour="blue",alpha=0.2)
g = g + geom_smooth(method="lm",formula=y~x,colour="black")
g
plot(norm$Degree.of.Time.Constraint,norm_rename$Degree.of.Mobility)
abline(lm(norm_rename$Degree.of.Mobility~norm$Degree.of.Time.Constraint))

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
g = ggplot(norm, aes(y=norm_rename$Degree.of.Sociality,x=norm_rename$Degree.of.Layman))
g = g + geom_point(size=12,colour="black",alpha=0.2)
g = g + geom_point(size=12,colour="blue",alpha=0.2)
g = g + geom_smooth(method="lm",formula=y~x,colour="black")
g
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
g = ggplot(norm, aes(y=norm_rename$Degree.of.Sociality,x=norm_rename$Degree.of.Operator))
g = g + geom_point(size=12,colour="black",alpha=0.2)
g = g + geom_point(size=12,colour="blue",alpha=0.2)
g = g + geom_smooth(method="lm",formula=y~x,colour="black")
g
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
g = ggplot(norm, aes(y=norm_rename$Degree.of.Sociality,x=norm_rename$Degree.of.Expert))
g = g + geom_point(size=12,colour="black",alpha=0.2)
g = g + geom_point(size=12,colour="blue",alpha=0.2)
g = g + geom_smooth(method="lm",formula=y~x,colour="black")
g
plot(norm$Degree.of.Expert,norm_rename$Degree.of.Sociality)
abline(lm(norm_rename$Degree.of.Sociality~norm$Degree.of.Expert))



