###desicion tree for company data 
company <- read.csv(file.choose())
View(company)
str(company)
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
library(tree)
library(caret)
library(C50)
# Data partion for model building and testing
inTraininglocal <- createDataPartition(company$Sales,p=.50,list=F)
training <-company[inTraininglocal,]
testing <- company[-inTraininglocal,]
View(training)
View(testing)
#factor building
training$Sales<-as.factor(training$Sales)
str(training$Sales)

#model building
model <- C5.0(training$Sales~.,data = training,trails=40)
# Generating the model summary
summary(model)
pred <- predict.C5.0(model,testing[,-1])
View(pred)
a <- table(testing$Sales,pred)
View(a)
sum(diag(a)/sum(a))
plot(model)
###Bagging####
acc<-c()
for(i in 1:100)
{
  print(i)
  inTraininglocal<-createDataPartition(company$Sales,p=.50,list=F)
  training1<-company[inTraininglocal,]
  testing<-company[-inTraininglocal,]
  training1$Sales<-as.factor(training1$Sales)
  str(training1$Sales)
  library(tree)
  fittree<-C5.0(training1$Sales~.,data=training1)
  pred<-predict.C5.0(fittree,testing[,-1])
  a<-table(testing$Sales,pred)
  acc<-c(acc,sum(diag(a))/sum(a))
  
}
summary(acc)
acc
plot(acc)
####Evaluation on training data (201 cases) has size :80  error :116(57.7%)



###desicion tree for fraud data set
fraud <- read.csv(file.choose())
View(fraud)
str(fraud)
fraud<- cbind(fraud,ifelse(fraud$Taxable.Income>'30000',1,0))
names(fraud)[7]<-paste("taxes")
fraud<-fraud[-3]
View(fraud)
library(tree)
library(caret)
library(C50)
# Data partion for model building and testing
inTraininglocal <- createDataPartition(fraud$taxes,p=.75,list=F)
training <-fraud[inTraininglocal,]
testing <- fraud[-inTraininglocal,]
View(training)
View(testing)
#factor building
training$taxes<-as.factor(training$taxes)
str(training$taxes)

#model building
model <- C5.0(training$taxes~.,data = training,trails=40)
# Generating the model summary
summary(model)
pred <- predict.C5.0(model,testing[,-6])
View(pred)
a <- table(testing$taxes,pred)
View(a)
sum(diag(a)/sum(a))
plot(model)
###Bagging####
acc<-c()
for(i in 1:100)
{
  print(i)
  inTraininglocal<-createDataPartition(fraud$taxes,p=.75,list=F)
  training1<-fraud[inTraininglocal,]
  testing<-fraud[-inTraininglocal,]
  training1$taxes<-as.factor(training1$taxes)
  str(training1$taxes)
  library(tree)
  fittree<-C5.0(training1$taxes~.,data=training1)
  pred<-predict.C5.0(fittree,testing[,-6])
  a<-table(testing$taxes,pred)
  acc<-c(acc,sum(diag(a))/sum(a))
  
}
summary(acc)
acc

plot(acc)
####Evaluation on training data (201 cases) has size :1  error :57(19.0%)
