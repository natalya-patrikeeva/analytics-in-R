tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)
str(tweets)
tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)

# preprocess tweets
install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)

# a corpus is a collection of documents
# convert tweets to a corpus

corpus = Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]$content

# convert all tweets to lower case
corpus = tm_map(corpus, tolower)
corpus[[1]]

corpus = tm_map(corpus, PlainTextDocument)

# remove punctuation
corpus = tm_map(corpus, removePunctuation)
corpus[[1]]$content

# remove stop words
stopwords("english")[1:10]
length(stopwords("english"))
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]$content

# stemming
corpus = tm_map(corpus, stemDocument)
corpus[[1]]$content

# matrix of word counts with rows - documents (tweets); columns - words in tweets (terms)
frequencies = DocumentTermMatrix(corpus)
frequencies
inspect(frequencies[1000:1005, 505:515]) # sparse!
findFreqTerms(frequencies, lowfreq = 20)

# sparsity threshold 0.995 means keep the words that appear in 0.5% or more of the tweets
sparse = removeSparseTerms(frequencies, 0.995)
sparse

# convert sparse matrix into a dataframe
tweetsSparse = as.data.frame(as.matrix(sparse))

# convert column names (variables) to appropriate names (not numbers)
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

# dependent variable
tweetsSparse$Negative = tweets$Negative

# split into training and test sets
library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse = subset(tweetsSparse, split == TRUE)
testSparse = subset(tweetsSparse, split == FALSE)

# CART
library(rpart)
library(rpart.plot)
tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")
prp(tweetCART)

# make predictions on test data 
predictCART = predict(tweetCART, newdata=testSparse, type="class")

# confusion matrix
table(testSparse$Negative, predictCART)
#        predictCART
#        FALSE TRUE
# FALSE   294    6
# TRUE     37   18

# accuracy - 0.8788732
(294+18)/nrow(testSparse)

# Baseline - predict Negative = FALSE 
table(testSparse$Negative)
#  FALSE  TRUE 
#  300    55 

300/nrow(testSparse) # 0.8450704

# random forest model
library(randomForest)
set.seed(123)
tweetRF = randomForest(Negative ~ ., data = trainSparse)
predictRF = predict(tweetRF, newdata=testSparse)
table(testSparse$Negative, predictRF)
#       predictRF
#       FALSE TRUE
# FALSE   293    7
# TRUE     34   21

# accuracy - 0.884507
(293+21)/nrow(testSparse)

# Logistic regression model
log = glm(Negative ~ ., data = trainSparse, family=binomial)
summary(log)
logpredictions = predict(log, newdata=testSparse, type="response")
summary(logpredictions)
table(testSparse$Negative, logpredictions > 0.5)
#        FALSE TRUE
# FALSE   253   47
# TRUE     23   32

# accuracy of logistic regression model
(253+32)/nrow(testSparse)  # 0.8028169

