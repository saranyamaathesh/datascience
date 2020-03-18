##########CustomerOrderfantaloons#########
fantaloons<-read.csv(file.choose())   
View(fantaloons)
colnames(fantaloons)
fantaloons<- cbind(fantaloons,ifelse(fantaloons$Weekdays=="Male",1,0))
names(fantaloons)[3]<-paste("weekdays")
fantaloons<- cbind(fantaloons,ifelse(fantaloons$Weekend=="Male",1,0))
names(fantaloons)[4]<-paste("weekends")        
fantaloons<-fantaloons[-1]
fantaloons<-fantaloons[-1]
Stacked_Data <- stack(fantaloons)
View(Stacked_Data)
attach(Stacked_Data)

#############Normality test###############
library(nortest)
ad.test(Stacked_Data$values)
##P-Value is less than 0.05, we reject the null hypothesis in case of test with 95% confidence or 5% significance.


########## Mann-Whitney Test ########
wilcox.test(values ~ind, data=Stacked_Data) 
###p value  6.333e-05 << 0.05 hence p low null go. the alternative hypothesis is taken as solution .
###yes ,the  percentage of males versus females walking in to the store differ based on day of the week