bank <- read.csv(file.choose())
View(bank)

library(caTools)
library(caret)
model <- glm(y~.,data=bank,family = "binomial")
summary(model)
res<-predict(model,bank,y="response")
res
###to reduce the AIC value
model <- glm(y~.-age,data=bank,family = "binomial")
summary(model)
res<-predict(model,bank,y="response")
res

###confusion matrix
table(ActualValue=bank$y,PredictedValue= res>0.5)

###ROCR
res<-predict(model,bank,y="response")
library(ROCR)
ROCRpred=prediction(res,bank$y)
ROCRpref=performance(ROCRpred,"tpr","fpr")
plot(ROCRpref,colorsize=TRUE,print.cutoffs.at=seq(.3,by=.3))
####AIC value is 21646 from the data y has confusion matrix== 0.89,therefore the accuracy is 89% .Then R==21.6 in which R>1 


####             PredictedValue
#  ActualValue     FALSE TRUE
#           no    39291   631
#            yes  3929    1360