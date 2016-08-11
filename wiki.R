# history of wikipedia edits of page Language
rm(list=ls())

wiki = read.csv("wiki.csv", stringsAsFactors = FALSE)
str(wiki)
wiki$Vandal = as.factor(wiki$Vandal)
summary(wiki)

# bag of words technique
library(tm)
library(SnowballC)
# a corpus is a collection of documents
# convert added words to a corpus

corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded
corpusAdded[[1]]$content

# remove stop words
stopwords("english")
length(stopwords("english"))   # should be 174

corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded[[1]]$content

corpusAdded = tm_map(corpusAdded, stemDocument)
corpusAdded[[1]]$content

dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))


# Create a corpus, remove stop words,
# stem the document, create a sparse document term matrix, and convert it to a
# data frame) to create a Removed bag-of-words dataframe,
# and prepend all of the words with the letter R.

corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved
corpusRemoved[[1]]$content

corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal

library(caTools)
set.seed(123)
split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
trainSparse = subset(wikiWords, split == TRUE)
testSparse = subset(wikiWords, split == FALSE)

# baseline of predicting Vandalist = 0 (no vandalism)
summary(wiki)
table(wikiWords$Vandal)
#accuracy
# 0    1 
# 2061 1815 
2061/nrow(wikiWords)  # 0.5317337

# CART model to predict Vandal, using all of the other variables as independent variables. 
library(rpart)
library(rpart.plot)
vandalCART = rpart(Vandal ~ ., data=trainSparse, method="class")
prp(vandalCART)

# make predictions on test data 
predictCART = predict(vandalCART, newdata=testSparse, type="class")

# confusion matrix
table(testSparse$Vandal, predictCART)
# predictCART
#     0   1
# 0 618   0
# 1 533  12
nrow(testSparse)

# accuracy
(618+12)/nrow(testSparse) # 0.5417025

# overfit ? 
# check training data accuracy
predictCART_train = predict(vandalCART, newdata = trainSparse, type="class")
table(trainSparse$Vandal, predictCART_train)
#  predictCART_train
#      0    1
# 0 1443    0
# 1 1237   33

# accuracy
(1443+33)/nrow(trainSparse)  # 0.5440472

# Create a copy of dataframe
wikiWords2 = wikiWords

# Make a new column in wikiWords2 that is 1 if "http" was in Added:
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, split == TRUE)
wikiTest2 = subset(wikiWords2, split == FALSE)
CARTnew = rpart(Vandal ~ ., data=wikiTrain2, method="class")
prp(CARTnew)
# make predictions on test data 
predictCARTnew = predict(CARTnew, newdata=wikiTest2, type="class")

# confusion matrix
table(wikiTest2$Vandal, predictCARTnew)
#  predictCARTnew
#     0   1
# 0 609   9
# 1 488  57

# accuracy
(609+57)/nrow(wikiTest2) #0.5726569

# sum the rows of dtmAdded and dtmRemoved and add them as new variables
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
summary(wikiWords2$NumWordsAdded)
mean(wikiWords2$NumWordsAdded)

# make new training and testing sets with wikiWords2.
wikiTrain = subset(wikiWords2, split == TRUE)
wikiTest = subset(wikiWords2, split == FALSE)
CARTwiki = rpart(Vandal ~ ., data = wikiTrain, method="class")
prp(CARTwiki)
CARTwikipredict = predict(CARTwiki, newdata = wikiTest, type="class")
table(wikiTest$Vandal, CARTwikipredict)
# CARTwikipredict
#     0   1
# 0 514 104
# 1 297 248
# accuracy
(514+248)/nrow(wikiTest) # 0.6552021

wikiWords3 = wikiWords2
summary(wikiWords3)
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
wikiTrain3 = subset(wikiWords3, split == TRUE)
wikiTest3 = subset(wikiWords3, split == FALSE)
CARTwiki3 = rpart(Vandal ~ ., data = wikiTrain3, method="class")
prp(CARTwiki3)
CARTwikipredict3 = predict(CARTwiki3, newdata = wikiTest3, type="class")
table(wikiTest3$Vandal, CARTwikipredict3)
# CARTwikipredict3
#     0   1
# 0 595  23
# 1 304 241
# accuracy
(595+241)/nrow(wikiTest3) # 0.7188306
