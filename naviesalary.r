salarytrain <- read.csv(file.choose())
View(salarytrain)
sal<-ifelse(salarytrain$Salary==" >50K","high","low")
salarytrain<-data.frame(salarytrain,sal)
salarytrain<-salarytrain[-14]


salarytest <- read.csv(file.choose())
View(salarytest)
sala<-ifelse(salarytest$Salary==" >50K","high","low")
salarytest<-data.frame(salarytest,sala)
salarytest<-salarytest[-14]


####naive bayes
library(e1071)
library(caret)
salarynb<-naiveBayes(sal~.,data=salarytrain)
salarynb
pred<-predict(salarynb,salarytest)
confusionMatrix(table(pred,salarytest$sala))

###visualization
plot(salarytest$sala,salarytest$age)
plot(salarytrain$sal,salarytrain$age)
#####Accuracy of confusion matrix is 0.8193,that is 82%.R=0.0071 in which R<7 so the accuracy is not affected
####                    pred  high   low
#####                   high  1789   810
#####                    low  1911 10550
####      