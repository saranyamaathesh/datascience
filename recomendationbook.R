
#Installing and loading the libraries
#install.packages("recommenderlab", dependencies=TRUE)
#install.packages("Matrix")
library("recommenderlab")
library(caTools)

#book ratings...3. data
best_book <- read.csv(file.choose())
best_book<-best_book[4:6]
View(best_book)
#metadata about the variable
str(best_book)
colnames(best_book)

#ratings...3. distribution
hist(best_book$ratings...3.s...3.)
#the datatype should be realratings...3.Matrix inorder to build recommendation engine
best_book_matrix <- as(best_book, 'realratings...3.Matrix')

#Popularity based 

book_recomm_model1 <- Recommender(best_book_matrix, method="POPULAR")
View(book_recomm_model1)
#Predictions for two users 
recommended_items1 <- predict(book_recomm_model1, best_book_matrix[100:110], n=3)
as(recommended_items1, "list")


## Popularity model recommends the same books for all users , we need to improve our model using # # Collaborative Filtering

#User Based Collaborative Filtering

book_recomm_model2 <- Recommender(best_book_matrix, method="UBCF")

#Predictions for two users 
recommended_items2 <- predict(book_recomm_model2, best_book_matrix[100:110], n=3)
as(recommended_items2, "list")
plot(best_book$ratings...3.)

