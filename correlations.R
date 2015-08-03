#R script for generating finding pearson & spearman correlations between different axis

require(xlsx)
norm_rename=read.xlsx("normalize.xlsx",sheetName = "Sheet1")
attach(norm_rename)

#Function to calculate the Pearson Correlation between different axis

calc_pearson <- function(x){
        calc=format(round(cor(x[sapply(x, is.numeric)],use="pairwise.complete.obs",method="pearson"),4),nsmall=4)
        return(calc)
}
calc_pearson(norm_rename)
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