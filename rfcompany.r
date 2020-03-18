###random forest for company data 
company <- read.csv(file.choose())
View(company)
summary(company)
class(company)
colnames(company)
max(Sales)
company<- cbind(company,ifelse(company$ShelveLoc=="Bad",1,0), ifelse(company$ShelveLoc=="Good",1,0),  ifelse(company$ShelveLoc=="Medium",1,0))
names(company)[12]<-paste("Bad")
names(company)[13]<-paste("Good")
names(company)[14]<-paste("Medium")
company<- cbind(company,ifelse(company$Urban=="Yes",1,0))
names(company)[15]<-paste("urban")
company<- cbind(company,ifelse(company$US=="Yes",1,0))
names(company)[16]<-paste("us")
company<-company[-7]
company<-company[-9]
company<-company[-9]
View(company)
company<- cbind(company,ifelse(company$Sales>7.496,"high","Low"))
names(company)[14]<-paste("Sales")
company<-company[-1]
View(company)

#summary(forestfire_norm$area)

library(caret)
# Data partion for model building and testing
inTraininglocal <- createDataPartition(company$Sales,p=.75,list=F)
training <-company[inTraininglocal,]
testing <- company[-inTraininglocal,]
View(training)
View(testing)
#factor building
training$Sales<-as.factor(training$Sales)
str(training$Sales)
testing$Sales<-as.factor(testing$Sales)
str(testing$Sales)
# Apply max to vector
max(Sales)                                      
# Building a random forest model on training data 
library(caret)
library(randomForest)

company_forest <- randomForest(Sales~.,data=training,importance=TRUE)
company_forest

plot(company_forest)
pred<-predict(company_forest,testing)
confusionMatrix(table(pred,testing$Sales))

legend("topright",colnames(company_forest$err.rate),col=1:3,cex=1,fill=1:3)

varImpPlot(company_forest)
#####Balanced Accuracy : 0.8233  and has OOB estimate of  error rate: 17.94% 

