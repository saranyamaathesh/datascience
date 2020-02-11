sms_raw<-read.csv(file.choose())
class(sms_raw)
str(sms_raw)
sms_raw$type<-as.factor(sms_raw$type)
str(sms_raw)
table(sms_raw$type)
library(rmarkdown)
library(caret)
library(dplyr)
library(tm)
sms_corpus<-Corpus(VectorSource(sms_raw$text))
sms_corpus<-tm_map(sms_corpus,function(X)iconv(enc2utf8(X),sub = 'byte'))
class(sms_corpous)

# Cleaning data (removing unwanted symbols)
corpus_clean<-tm_map(sms_corpus,tolower)
corpus_clean<-tm_map(corpus_clean, removeNumbers)
corpus_clean<-tm_map(corpus_clean,removeWords, stopwords())
corpus_clean<-tm_map(corpus_clean,removePunctuation)
corpus_clean<-tm_map(corpus_clean,stripWhitespace)
class(corpus_clean)
sms_dtm <- DocumentTermMatrix(corpus_clean)
class(sms_dtm)


# as.character(sms_dtm)# creating training and test datasets
sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test  <- sms_raw[4170:5559, ]
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]
sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test  <- corpus_clean[4170:5559]

# check that the proportion of spam is similar
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

# indicator features for frequent words# if the word has been referred to 5 times or more
sms_dict<-findFreqTerms(sms_dtm, 5)
sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))


##convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}


# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)



##training a model on the data
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_classifier


##Evaluating model performance
sms_test_pred <- predict(sms_classifier, sms_test)
table(sms_test_pred)
prop.table(table(sms_test_pred))
library(gmodels)
CrossTable(sms_test_pred,sms_raw_test$type,prop.chisq = FALSE,prop.t = FALSE,
           prop.r = FALSE, dnn = c('predicted', 'actual'))
confusionMatrix(sms_test_pred,sms_raw_test$type)
###            Reference
###Prediction  ham spam
####      ham  1203   26
###       spam    4  157
#the Accuracy is 0.9784 that is 98% . 
