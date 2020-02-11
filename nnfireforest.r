forestfire <- read.csv(file.choose())
forestfire<-forestfire[-1]
forestfire<-forestfire[-1]
View(forestfire)
colnames(forestfire)
forestfire<- cbind(forestfire,ifelse(forestfire$size_category=="small",1,0))
View(forestfire)
names(forestfire)[30]<-paste("size")
forestfire<-forestfire[-29]
str(forestfire)
#attach(forestfire)
#normal_forestfire<-scale(forestfire)
## or 
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
forestfire_norm<-as.data.frame(lapply(forestfire[,-9],FUN=normalize))
#summary(forestfire_norm$area)
#summary(normal_forestfire)
summary(forestfire$area)

forestfire_norm <- cbind(forestfire_norm,forestfire$area)
View(forestfire_norm)
colnames(forestfire_norm)[29] <- "area"
View(forestfire_norm)
forestfire_train<-forestfire_norm[1:389,]
View(forestfire_train)
forestfire_test<-forestfire_norm[389:517,]

# Using multilayered feed forward nueral network
# package nueralnet
# install.packages("neuralnet")
# install.packages("nnet")
library(neuralnet)  # regression
library(nnet) # classification 

# Building model
formula_nn <- paste("area",paste(colnames(forestfire[-9]),collapse ="+"),sep="~")
View(forestfire)
#forestfire_model <- neuralnet(area~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = forestfire_train)
forestfire_model <- neuralnet(formula = formula_nn,linear.output = F,data = forestfire_train)
View(forestfire_model)
str(forestfire_model)
plot(forestfire_model)

# SSE sum of squared errors . least SSE best model
# Evaluating model performance

set.seed(500)
library(MASS)
data <- forestfire
apply(data,2,function(x) sum(is.na(x)))
index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(area~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$area)^2)/nrow(test)


####Preparing to fit the neural network
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]
View(test_)
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("area ~", paste(n[!n %in% "area"], collapse = " + ")))
nn <- neuralnet(f,data=train_,linear.output=F)
plot(nn)

####Predicting area using the neural network
pr.nn <- compute(nn,test_[-9])
pr.nn_ <- pr.nn$net.result*(max(data$area)-min(data$area))+min(data$area)
test.r <- (test_$area)*(max(data$area)-min(data$area))+min(data$area)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
print(paste(MSE.lm,MSE.nn))
par(mfrow=c(1,2))
plot(test$area,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')
plot(test$area,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)
plot(test$area,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$area,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))
plot(test$area,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$area,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))
library(boot)
set.seed(200)
lm.fit <- glm(area~.,data=data)
cv.glm(data,lm.fit,K=517)$delta[1]
set.seed(450)
cv.error <- NULL
k <- 517
library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)
for(i in 1:k){
  index <- sample(1:nrow(data),round(0.9*nrow(data)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  nn <- neuralnet(f,data=train.cv,linear.output=F)   
  pr.nn <- compute(nn,test.cv[-9])
  pr.nn <- pr.nn$net.result*(max(data$area)-min(data$area))+min(data$area)   
  test.cv.r <- (test.cv$area)*(max(data$area)-min(data$area))+min(data$area)   
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