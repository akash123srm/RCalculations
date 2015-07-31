data(airquality)
attach(airquality)
names(airquality)
#[1] "Ozone"   "Solar.R" "Wind"    "Temp"    "Month"   "Day"   
plot(Ozone~Solar.R,data=airquality)
#Calculate mean Ozone concentration
mean.ozone=mean(Ozone,na.rm=T)
abline(h=mean.ozone)
#use lm to fit a regression line through these data:
model_1=lm(Ozone~Solar.R,data=airquality)
model_1
seq_1 = 1:100
check = NULL
for(i in seq(along=seq_1))
{
        if(Temp[i] >=65)
        { 
                check <- c(check, seq_1[i]) 
        }
        
        
}
set.seed(100)
GetMeanandSE = function(x)
{
        m = mean(x)
        l = length(x)
        SE = sd(x)/sqrt(l)
        return(c(m,SE))
}

d = Temp
GetMeanandSE(d)

set.seed(100)
f2 <- function(x, y) {
        z1 <- x + y
        z2 <- x + 2*y
        list(z1, z2)
        
}
v1 = matrix(c(1,2,3,4,5,6),ncol=2)
v2 = matrix(c(1,2,3,4,5,6),ncol=2)
f2(v1,v2)[[1]]
names1 <- c("Dave", "John", "Ann", "Roger", "Bill", "Kathy")
f.name <- function(x){
        for (i in x){
                if(i == "Roger")
                        break
                print(i)
        }
}
f.name(names1)
set.seed(100)
calc_difference_temperature <- function(x){
        seq_1 = 1:length(x)
        vec <- vector()
        for(i in seq(along=seq_1))
        {
                vec <- c(vec,x[i+1] - x[i])
                
        }
        return(vec)
}
arg = Temp
calc_difference_temperature(arg)

# Function to calculate Pearson's correlation among all the parameters of a data
calc_correlation <- function(){
        correlation = cor(airquality,use="pairwise.complete.obs",method="pearson")
        return(correlation)
}
calc_correlation()

# Function to generate the scatterplot
generate_scatterplot <-function(){
        scatterplot = plot(airquality$Ozone,airquality$Temp,main="Scatterplot",xlab="Ozone Concentration",
                           ylab = "Temp")
        return(scatterplot)
}
generate_scatterplot()

# Function to calculate mean across of every column

calc_mean <- function(y){
        no_columns = ncol(y)
        means = vector()
        for(i in 1:no_columns){
                means[i] = mean(y[, i],na.rm = TRUE)
        }
        return(means)
}
calc_mean(airquality)
x=list(a=matrix(5:8,2,2),b=matrix(1:6,3,2))
lapply(x,function(extract) extract[,1])

mat= matrix(1:6,2,3)
print(mat)
apply(mat,2,sum)
list_test = list(rep(1,4),rep(2,3))
list_test
mapply(rep,1:2,4:3)

s= split(airquality,airquality$Month)
lapply(s,function(x) colMeans(x[,c("Ozone","Solar.R")],na.rm = TRUE))
set.seed(2)
x = rnorm(100)
log.mu <- 0.5 + 0.3 * x
y <- rpois(100,exp(log.mu))
summary(y)
plot(x,y)
calc_mean(airquality)




