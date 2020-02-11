startups <- read.csv(file.choose())
View(startups)
str(startups)
startups<- cbind(startups,ifelse(startups$State=="New York",1,0),ifelse(startups$State=="California",1,0),ifelse(startups$State=="Florida",1,0))
names(startups)[6]<-paste("New.York")
names(startups)[7]<-paste("California")
names(startups)[8]<-paste("Florida")
startups<-startups[-4]
View(startups)
str(startups)
#attach(startups)
#normal_startups<-scale(startups)
## or 
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
startups_norm<-as.data.frame(lapply(startups[,-4],FUN=normalize))
#summary(startups_norm$Profit)
#summary(normal_startups)
summary(startups$Profit)
View(startups)
startups_norm <- cbind(startups_norm,startups$Profit)
View(startups_norm)
colnames(startups_norm)[7] <- "Profit"
View(startups_norm)
library(caret)
startups_train<-startups_norm[1:35,]
View(startups_train)
startups_test<-startups_norm[36:50,]


# Using multilayered feed forward nueral network
# package nueralnet
# install.packages("neuralnet")
# install.packages("nnet")
library(neuralnet)  # regression
library(nnet) # classification 

# Building model
formula_nn <- paste("Profit",paste(colnames(startups[-4]),collapse ="+"),sep="~")
View(startups)
startups_model <- neuralnet(formula = formula_nn,linear.output = F,data =startups_train)
##startups_model <- neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend+New.York+California+Florida,data = startups_train)
View(startups_model)
str(startups_model)
plot(startups_model)

# MSE sum of squared errors . least MSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
set.seed(500)
library(MASS)
data <- startups_norm
apply(data,2,function(x) sum(is.na(x)))
index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(Profit~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$Profit)^2)/nrow(test)


####Preparing to fit the neural network
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]

library(neuralnet)
n <- names(train_)
f <- as.formula(paste("Profit ~", paste(n[!n %in% "Profit"], collapse = " + ")))
nn <- neuralnet(f,data=train_,linear.output=F)
plot(nn)

####Predicting Profit using the neural network
pr.nn <- compute(nn,test_[,1:6])
pr.nn_ <- pr.nn$net.result*(max(data$Profit)-min(data$Profit))+min(data$Profit)
test.r <- (test_$Profit)*(max(data$Profit)-min(data$Profit))+min(data$Profit)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
print(paste(MSE.lm,MSE.nn))
par(mfrow=c(1,2))
plot(test$Profit,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')
plot(test$Profit,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)
plot(test$Profit,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$Profit,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))
plot(test$Profit,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$Profit,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))
library(boot)
set.seed(200)
lm.fit <- glm(Profit~.,data=data)
cv.glm(data,lm.fit,K=50)$delta[1]
set.seed(450)
cv.error <- NULL
k <- 1030
library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)
for(i in 1:k){
  index <- sample(1:nrow(data),round(0.9*nrow(data)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  nn <- neuralnet(f,data=train.cv,linear.output=F)   
  pr.nn <- compute(nn,test.cv[,1:6])
  pr.nn <- pr.nn$net.result*(max(data$Profit)-min(data$Profit))+min(data$Profit)   
  test.cv.r <- (test.cv$Profit)*(max(data$Profit)-min(data$Profit))+min(data$Profit)   
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)    
  pbar$step()
}
mean(cv.error)
cv.error
boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)
# MSE has reduced and training steps had been increased as the number of nuerons 


