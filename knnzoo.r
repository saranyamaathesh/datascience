# Read the dataset
zoo <- read.csv(file.choose())
zoo<-zoo[-1]
View(zoo)
table(zoo$type)
zoo$type <- factor(zoo$type,levels = c("1","2","3","4","5","6","7"),labels = c("one","two","three","four","five","six","seven"))
round(prop.table(table(zoo$type))*100,1)
summary(zoo[c("hair","feathers","eggs","milk","airborne","aquatic","predator","toothed","backbone","breathes","venomous","fins","tail","legs","domestic","catsize","type")])
#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))
#Apply the normalization function to zoo dataset
zoo_n <- as.data.frame(lapply(zoo[1:16], norm))
View(zoo_n)
summary(zoo_n)


library(caret)
inTraininglocal <- createDataPartition(zoo$type,p=.75,list=F)
zoo_train <- zoo[inTraininglocal,-17]
zoo_test <- zoo[-inTraininglocal,-17]

#Get labels for training and test datasets

zoo_train_labels <- zoo[inTraininglocal,17]
zoo_test_labels <- zoo[-inTraininglocal,17]

library("class")
NROW(zoo_train_labels)
knn.40<-knn(train=zoo_train,test =zoo_test,cl=zoo_train_labels,k=40 )
Acc.40<-100*sum(zoo_train_labels==knn.40)/NROW(zoo_test_labels)
Acc.40
###another way of finding accuracy
library(caret)
confusionMatrix(table(knn.40,zoo_test_labels))
##maximum percentage accuracy graph
i=1
k.optm=1
for (i in 1:45)
{
  train_zoo_pred <- knn(train=zoo_train,test=zoo_train,cl=zoo_train_labels,k=i)
  k.optm[i]<-100*sum(zoo_test_labels==train_zoo_pred)/NROW(zoo_test_labels)
  k=i
  cat(k,'=',k.optm[i],'\n')
}
plot(k.optm,type="b",xlab = "k-value",ylab = "accuracy level")

##knn has the accuracy of 125 then by using confusion Matrix the accuracy increased by 0.5000
##maximum percentage accuracy graph, the accuracy is given at 40=133.3333
