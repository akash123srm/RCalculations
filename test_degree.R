options(max.print=1000000) 

require(xlsx)
require(gdata)
data_load = read.csv(file.choose(),header=TRUE)
attach(data_load)

dr = dr = data.frame()[1:52,c()]
for(i in 1:length(data_load)){
dr[i]=as.double(strsplit(as.character(data_load[[i]]),"%"))   
}
dr

calc_pearson <- function(x){
        calc=format(round(cor(x[sapply(x, is.numeric)],method="pearson"),4),nsmall=4)
        return(calc)
}
calc_pearson(dr)
library(xlsx)
write.xlsx(calc_pearson(dr),file="pearson2_test.xlsx")