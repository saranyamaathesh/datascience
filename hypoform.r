##########CustomerOrderForm#########
form<-read.csv(file.choose())   
View(form)
colnames(form)
form<- cbind(form,ifelse(form$Phillippines=="Error Free",1,0))
names(form)[5]<-paste("Phillippines")
form<- cbind(form,ifelse(form$Indonesia=="Error Free",1,0))
names(form)[6]<-paste("Indonesia")             
form<- cbind(form,ifelse(form$Malta=="Error Free",1,0))
names(form)[7]<-paste("Malta")                          
form<- cbind(form,ifelse(form$India=="Error Free",1,0))
names(form)[8]<-paste("India")
form<-form[-1]
form<-form[-1]
form<-form[-1]
form<-form[-1]
View(form)
Stacked_Data <- stack(form)
View(Stacked_Data)
attach(Stacked_Data)

#############Normality test###############
library(nortest)
ad.test(Stacked_Data$values) 
##P-Value is less than 0.05, we reject the null hypothesis in case of test with 95% confidence or 5% significance.


################ Mood's Median Test #################
install.packages("RVAideMemoire")
library(RVAideMemoire)
height <- read.csv(file.choose())
form
attach(form)
Stacked_Data <- stack(form)
View(Stacked_Data)
attach(Stacked_Data)
mood.medtest( values~ ind,data = Stacked_Data,exact = FALSE)
####Mood's median test p-value = 1> 0.05.hence the defective percentage varies by centre is same.