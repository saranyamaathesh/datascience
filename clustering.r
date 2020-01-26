###clustering on crime dataset

#hclustering
crime <- read.csv(file.choose())
View(crime)
crime=crime[,2:5]
View(crime)
normalized_data<-scale(crime)
d <- dist(normalized_data, method = "euclidean") 
fit <- hclust(d, method="complete")
plot(fit)
plot(fit, hang=-1)
rect.hclust(fit, k=3, border="red")
groups <- cutree(fit, k=3)
crimerate <-as.matrix(groups)
crimes <- data.frame(crime, crimerate)
View(crimes)
write.csv(crimes, file="crimerate.csv",row.names = F)
aggregate(crime[,-1],by=list(crimes$crimerate),mean)


###hence the crime data has three clusters based on their crimerate



#kmeans clustering
input <- read.csv(file.choose())
input=input[,2:5]
View(input)
normalized_data <- scale(input)
crimeratek <- kmeans(normalized_data, 3)
str(fit)
crime2<- data.frame(input, crimeratek$cluster)
crime2
crime3 <- crime2[,c(ncol(crime2),1:(ncol(crime2)-1))]
View(crime3)
write.csv(crime3,file = "crime3.csv",row.names = F)
View(crime3)
aggregate(input, by=list(crimeratek$cluster), FUN=mean)
twss = c()
for (i in 1:12) twss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:12, twss, type="b", xlab="Number of crime", ylab="crimerate")  
title(sub = "K-Means Clustering for murderrates")


###hence the crime data has three clusters based on their crimerate


###clustering on airlines dataset 


#hclustering
air <- read.csv(file.choose())
air=air[,2:12]
View(air)
normalized_data<-scale(air)
d <- dist(normalized_data, method = "euclidean") 
fit <- hclust(d, method="complete")
plot(fit)
plot(fit, hang=-1)
rect.hclust(fit, k=2, border="red")
groups <- cutree(fit, k=2)
awardflight<-as.matrix(groups)
freeflight <- data.frame(air, awardflight)
View(freeflight)
write.csv(freeflight, file="freeflight.csv",row.names = F)
aggregate(air[,-1],by=list(freeflight$awardflight),mean)

####hence the awardflight value of person having 1 will be accepted for frequent flight program

#kmeans clustering
input <- read.csv(file.choose())
input=input[,2:12]
View(input)
normalized_data <- scale(input)
fit <- kmeans(normalized_data, 2)
str(fit)
freeflight2<- data.frame(input, fit$cluster)
freeflight2
freeflight3 <- freeflight2[,c(ncol(freeflight2),1:(ncol(freeflight2)-1))]
View(freeflight3)
write.csv(freeflight3,file = "freeflight3.csv",row.names = F)
View(freeflight3)
aggregate(input, by=list(fit$cluster), FUN=mean)
twss = c()
for (i in 1:12) twss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:12, twss, type="b", xlab="Number of passengers", ylab="free flight rate")  
title(sub = "K-Means Clustering for passengers")
####hence the award value of person having 1 will be accepted for frequent flight program


