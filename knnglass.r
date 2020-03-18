# Read the dataset
glass <- read.csv(file.choose())
View(glass)
table(glass$Type)
glass$Type <- factor(glass$Type,levels = c("1","2","3","4","5","6","7"),labels = c("one","two","three","four","five","six","seven"))
round(prop.table(table(glass$Type))*100,1)
summary(glass[c("RI","Na","Mg","Al","Si","K","Ca","Ba","Fe","Type")])
#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))
#Apply the normalization function to glass dataset
glass_n <- as.data.frame(lapply(glass[1:9], norm))
View(glass_n)
summary(glass_n)

#create training and test datasets
library(caret)
inTraininglocal <- createDataPartition(glass$Type,p=.75,list=F)
glass_train <- glass[inTraininglocal,-10]
glass_test <- glass[-inTraininglocal,-10]

#Get labels for training and test datasets

glass_train_labels <- glass[inTraininglocal,10]
glass_test_labels <- glass[-inTraininglocal,10]

library("class")
NROW(glass_train_labels)
knn.15<-knn(train=glass_train,test =glass_test,cl=glass_train_labels,k=15 )
Acc.15<-100*sum(glass_train_labels==knn.15)/NROW(glass_test_labels)
Acc.15
###another way of finding accuracy
library(caret)
confusionMatrix(table(knn.15,glass_test_labels))
##maximum percentage accuracy graph
i=1
k.optm=1
for (i in 1:20)
{
  train_glass_pred <- knn(train=glass_train,test=glass_train,cl=glass_train_labels,k=i)
  k.optm[i]<-100*sum(glass_test_labels==train_glass_pred)/NROW(glass_test_labels)
  k=i
  cat(k,'=',k.optm[i],'\n')
}
plot(k.optm,type="b",xlab = "k-value",ylab = "accuracy level")

##knn has the accuracy of 101.9231 then by using confusion Matrix the accuracy increased by 0.76238
##maximum percentage accuracy graph, the accuracy is given at 15 = 78.84615 and 20 = 67.30769 
