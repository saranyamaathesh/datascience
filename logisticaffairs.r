affairs <- read.csv(file.choose())
View(affairs)
affairs <- cbind(affairs,ifelse(affairs$gender=="male",1,0),ifelse(affairs$children=="yes",1,0))
View(affairs)
affairs<-affairs[-5]
affairs<-affairs[-2]
View(affairs)
names(affairs)[8]<-paste("gender")
names(affairs)[9]<-paste("children")
affairs<- cbind(affairs,ifelse(affairs$affairs>'0',1,0))
affairs<-affairs[-1]
names(affairs)[9]<-paste("affair")
View(affairs)
library(caTools)
library(caret)
model <- glm(affair~.,data=affairs,family = "binomial")
summary(model)
res<-predict(model,affairs,affair="response")
res
###to reduce the AIC value
model <- glm(affair~.-education,data=affairs,family = "binomial")
summary(model)
res<-predict(model,affairs,affair="response")
res

###confusion matrix
table(ActualValue=affairs$affair,PredictedValue= res>0.05)

###ROCR
res<-predict(model,affairs,affair="response")
library(ROCR)
ROCRpred=prediction(res,affairs$affair)
ROCRpref=performance(ROCRpred,"tpr","fpr")
plot(ROCRpref,colorsize=TRUE,print.cutoffs.at=seq(.1,by=.1))
####AIC value is 625.68 from the data affair has confusion matrix== 0.78,therefore the accuracy is 78% .Then R==5.89 in which R>1 


####             PredictedValue
#  ActualValue     FALSE TRUE
#               0   436   15
#               1   125   25