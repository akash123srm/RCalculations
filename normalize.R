#R script for normalizing the axis & calculating their respective degrees

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
        dr = data.frame()[1:18,c()]
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
write.xlsx(norm,file="normalize.xlsx")

'''dr = data[,c()]
for(i in 1:length(col_select)){
dr[i]=as.double(strsplit(as.character(col_select[[i]]),"%"))   
}'''

