# Visualizing Twitter Text Data about Apply using word clouds
rm(list=ls())

tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)
str(tweets)
summary(tweets)
head(tweets)

# 1) Create a corpus using the Tweet variable
library(tm)
library(SnowballC)
corpus = Corpus(VectorSource(tweets$Tweet))
str(corpus)
summary(corpus)
corpus
corpus[[1]]$content

# 2) Convert the corpus to lowercase
corpus = tm_map(corpus, tolower)
corpus[[1]]
corpus = tm_map(corpus, PlainTextDocument)

# 3) Remove punctuation from the corpus
corpus = tm_map(corpus, removePunctuation)
corpus[[1]]$content

# 4) Remove all English-language stopwords
stopwords("english")[1:10]
length(stopwords("english"))
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]$content

# 5) Build a document-term matrix out of the corpus
# matrix of word counts with rows - documents (tweets); columns - words in tweets (terms)
dtm = DocumentTermMatrix(corpus)
dtm

# 6) Convert the document-term matrix to a data frame called allTweets
allTweets = as.data.frame(as.matrix(dtm))
str(allTweets)
head(allTweets)
summary(allTweets)
ncol(allTweets)

# Building a word cloud
install.packages("wordcloud")
library(wordcloud)
?wordcloud

# a vector of the words in our dataset
colnames(allTweets)
# the frequency of each word across all tweets
colSums(allTweets)

wordcloud(colnames(allTweets), colSums(allTweets), scale = c(2, 0.25))

wordcloud(colnames(allTweets), colSums(allTweets), scale = c(2, 0.25), random.order = FALSE)

negativeTweets = subset(allTweets, tweets$Avg <= -1)
wordcloud(colnames(negativeTweets), colSums(negativeTweets), scale = c(4, 0.5))

wordcloud(colnames(allTweets), colSums(allTweets), scale = c(4, 0.5), max.words = 150, rot.per=0.5 ,random.order = FALSE, random.color=TRUE)

install.packages("RColorBrewer")
library("RColorBrewer")
display.brewer.all()
?brewer.pal
brewer.pal(7,"Accent")
brewer.pal(7,"Set2")
brewer.pal(7,"YlOrRd")

wordcloud(colnames(allTweets), colSums(allTweets), scale = c(2, 0.25), colors=brewer.pal(9,"Blues")[c(5:9)])

wordcloud(colnames(allTweets), colSums(allTweets), scale = c(3, 0.5), colors=brewer.pal(9,"Blues")[c(-1:-4)])


