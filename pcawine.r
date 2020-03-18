##performing pca in wine data
wine<-read.csv(file.choose())
View(wine)
attach(wine)
cor(wine)
pcaObj<-princomp(wine, cor = TRUE, scores = TRUE, covmat = NULL)
str(pcaObj)
## princomp(wine, cor = TRUE) not_same_as prcomp(wine, scale=TRUE); similar, but different
summary(pcaObj)
str(pcaObj)
loadings(pcaObj)

plot(pcaObj) # graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)

biplot(pcaObj)

# Showing the increase of variance with considering principal components
# Which helps in choosing number of principal components
plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type="b")
#pcaObj$loadings

pcaObj$scores[,1:3] # Top 3 PCA Scores which represents the whole wine

# cbind used to bind the wine in column wise
# Considering top 3 principal component scores and binding them with wine
wine<-cbind(wine,pcaObj$scores[,1:3])
View(wine)

# preparing wine for clustering (considering only pca scores as they represent the entire wine)
clus_wine<-wine[,15:17]

# Normalizing the wine 
norm_clus<-scale(clus_wine) # Scale function is used to normalize wine
dist1<-dist(norm_clus,method = "euclidean") # method for finding the distance
# here I am considering Euclidean distance

# Clustering the wine using hclust function --> Hierarchical
fit1<-hclust(dist1,method="complete") # method here is complete linkage

plot(fit1) # Displaying Dendrogram

groups<-cutree(fit1,5) # Cutting the dendrogram for 5 clusters

quality<-as.matrix(groups) # cluster numbering 

View(quality)

final1<-cbind(quality,wine)
View(final1)
View(aggregate(final1[,-c(16:18)],by=list(quality),FUN=mean)) 
# drawn from the aggregate of the  wine on quality

write.csv(final1,file="wine_clustered.csv",row.names = F,col.names = F)
getwd()


##kmeans clustering

input=clus_wine
View(input)
normalized_data <- scale(input)
qualitykm <- kmeans(normalized_data, 3)
str(qualitykm)
qualitykm1<- data.frame(input, qualitykm$cluster)
qualitykm1
qualitykm3 <- qualitykm1[,c(ncol(qualitykm1),1:(ncol(qualitykm1)-1))]
View(qualitykm3)
write.csv(qualitykm3,file = "qualitykm3.csv",row.names = F)
View(qualitykm3)
aggregate(input, by=list(qualitykm$cluster), FUN=mean)
twss = c()
for (i in 1:18) twss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:18, twss, type="b", xlab="proportions", ylab="quality")  
title(sub = "K-Means Clustering for pca on wine data")

##the wine data has Group.1     Comp.1     Comp.2      Comp.3
#                     1       1  3.0586650  1.2082626 -0.17754116
#                     2       2 -2.5712276  0.9476868 -0.08599243
#                     3       3  0.1025044 -1.7207906  0.20507434    for kmeans clustering
