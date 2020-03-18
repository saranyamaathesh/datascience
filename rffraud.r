###random forest for fraud data set
fraud <- read.csv(file.choose())
View(fraud)
str(fraud)
fraud<- cbind(fraud,ifelse(fraud$Taxable.Income<='30000',"risky","good"))
names(fraud)[7]<-paste("taxes")
fraud<-fraud[-3]
View(fraud)
colnames(fraud)
fraud<- cbind(fraud,ifelse(fraud$Marital.Status=="Single",1,0), ifelse(fraud$Marital.Status=="Divorced",1,0),  ifelse(fraud$Marital.Status=="Married",1,0))
names(fraud)[7]<-paste("Single")
names(fraud)[8]<-paste("Divorced")
names(fraud)[9]<-paste("Married")
fraud<-fraud[-2]
View(fraud)
fraud<- cbind(fraud,ifelse(fraud$Urban=="YES",1,0))
names(fraud)[9]<-paste("Urban")
fraud<-fraud[-4]
fraud<- cbind(fraud,ifelse(fraud$Undergrad=="YES",1,0))
names(fraud)[9]<-paste("Undergrad")
fraud<-fraud[-1]
View(fraud)

library(caret)
# Data partion for model building and testing
inTraininglocal <- createDataPartition(fraud$taxes,p=.75,list=F)
training <-fraud[inTraininglocal,]
testing <- fraud[-inTraininglocal,]
View(training)
View(testing)
#factor building
training$taxes<-as.factor(training$taxes)
str(training$taxes)
testing$taxes<-as.factor(testing$taxes)
str(testing$taxes)
# Building a random forest model on training data 
library(caret)
library(randomForest)

fraud_forest <- randomForest(taxes~.,data=training,importance=TRUE)
fraud_forest

plot(fraud_forest)
pred<-predict(fraud_forest,testing)
confusionMatrix(table(pred,testing$taxes))

legend("topright",colnames(fraud_forest$err.rate),col=1:3,cex=1,fill=1:3)

varImpPlot(fraud_forest)
##### Accuracy : 0.7933  and has OOB estimate of  error rate: 20.89% 

