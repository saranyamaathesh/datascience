api_key<-"N3Fb45FuqBdkDxwcRePdv6O0t"
api_secret<-"I2pRV9gP7FejUzRUDvlovfs6yM0KaBmzpAtW0ezL4zMkV9iVJ8"
access_token<-"1229335377812410368-mEhO0uZlTOdEtCejsKdH5efbfD9Gon"
access_token_secret<-"3VvtpbInsalhAeLq6o24VeYYNkUasTrSOUtVyaWzQedtB"
 
library(twitteR)
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
#getting tweets
tweets<-searchTwitter("Barack Obama",n=10,lang='en')
tweets
tweetsdf<-twListToDF(tweets)
write.csv(tweetsdf,file = 'C:/Users/saranya/Obama.csv',row.names = F)
head(Obama.csv)

# Read file
Obama <- read.csv(file.choose(), header = T)
str(Obama)

# Build corpus
library(tm)
corpus <- iconv(Obama$text)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Clean text
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))

inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, removeWords, c('nbarack obama', 'Obama'))
cleanset <- tm_map(cleanset, gsub, 
                   pattern = 'presidency', 
                   replacement = 'president')
cleanset <- tm_map(cleanset, gsub, 
                   pattern = 'cleared', 
                   replacement = 'clear')
cleanset <- tm_map(cleanset, gsub, 
                   pattern = ' barack obama', 
                   replacement = 'obama')
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])
na.omit(cleanset)

# Term document matrix
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm
na.omit(tdm)
tdm
# Bar plot

w <- rowSums(tdm)
w <- subset(w, w>=1)
barplot(w,
        las = 2,
        col = rainbow(50))

# Word cloud
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),freq = w,
          max.words = 1000,
          random.order = F,
          min.freq = 0,
          colors = brewer.pal(9, 'Dark2'),
          rot.per =0.1)

library(wordcloud2)
w <- data.frame(names(w),w)
colnames(w) <- c('word', 'freq')
wordcloud2(data =wordcloud2 )
wordcloud2(w,
           size = 0.7,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)

letterCloud(w,
            word = "Obama",
            size=1)

# Sentiment analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Read file
Obama <- read.csv(file.choose(), header = T)
tweets <- iconv(Obama$text)

# Obtain sentiment scores
s <- get_nrc_sentiment(tweets)
head(s)
tweets[4]
get_nrc_sentiment('delay')

# Bar plot
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Obama Tweets')
###sentiment scores of obama has the joy as high and negative command as well
