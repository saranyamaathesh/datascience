concrete <- read.csv(file.choose())
View(concrete)
str(concrete)
#attach(concrete)
#normal_concrete<-scale(concrete)
## or 
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
concrete_norm<-as.data.frame(lapply(concrete[,-9],FUN=normalize))
#summary(concrete_norm$strength)
#summary(normal_concrete)
summary(concrete$strength)

concrete_norm <- cbind(concrete_norm,concrete$strength)
View(concrete_norm)
colnames(concrete_norm)[9] <- "strength"
View(concrete_norm)
concrete_train<-concrete_norm[1:773,]
View(concrete_train)
concrete_test<-concrete_norm[774:1030,]

# Using multilayered feed forward nueral network
# package nueralnet
# install.packages("neuralnet")
# install.packages("nnet")
library(neuralnet)  # regression
library(nnet) # classification 

# Building model
formula_nn <- paste("strength",paste(colnames(concrete[-9]),collapse ="+"),sep="~")
View(concrete)
#concrete_model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = concrete_train)
concrete_model <- neuralnet(formula = formula_nn,data = concrete_train)
View(concrete_model)
str(concrete_model)
plot(concrete_model)

# SSE sum of squared errors . least SSE best model
# Evaluating model performance

set.seed(500)
library(MASS)
data <- concrete
apply(data,2,function(x) sum(is.na(x)))
index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(strength~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$strength)^2)/nrow(test)


####Preparing to fit the neural network
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]

library(neuralnet)
n <- names(train_)
f <- as.formula(paste("strength ~", paste(n[!n %in% "strength"], collapse = " + ")))
nn <- neuralnet(f,data=train_,linear.output=F)
plot(nn)

####Predicting strength using the neural network
pr.nn <- compute(nn,test_[,1:8])
pr.nn_ <- pr.nn$net.result*(max(data$strength)-min(data$strength))+min(data$strength)
test.r <- (test_$strength)*(max(data$strength)-min(data$strength))+min(data$strength)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
print(paste(MSE.lm,MSE.nn))
par(mfrow=c(1,2))
plot(test$strength,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')
plot(test$strength,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)
plot(test$strength,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$strength,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))
plot(test$strength,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$strength,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))
library(boot)
set.seed(200)
lm.fit <- glm(strength~.,data=data)
cv.glm(data,lm.fit,K=1030)$delta[1]
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
  pr.nn <- compute(nn,test.cv[,1:8])
  pr.nn <- pr.nn$net.result*(max(data$strength)-min(data$strength))+min(data$strength)   
  test.cv.r <- (test.cv$strength)*(max(data$strength)-min(data$strength))+min(data$strength)   
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)    
  pbar$step()
}
mean(cv.error)
cv.error
boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)
# MSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased