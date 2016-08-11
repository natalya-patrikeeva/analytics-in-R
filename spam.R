rm(list=ls())
emails = read.csv("emails.csv", stringsAsFactors = FALSE)
str(emails)
summary(emails)
# spam = 1, ham = 0
table(emails$spam) 
emails$text[10]
max(nchar(emails$text))
which.min(nchar(emails$text))

# Preparing the corpus
library(tm)
library(SnowballC)
corpus = Corpus(VectorSource(emails$text))
corpus
corpus[[1]]$content

# Convert corpus to lowercase
corpus = tm_map(corpus, tolower)
corpus[[1]]
corpus = tm_map(corpus, PlainTextDocument)

# remove punctuation
corpus = tm_map(corpus, removePunctuation)
corpus[[1]]$content

# Remove the English language stop words 
stopwords("english")
length(stopwords("english"))   # should be 174
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus[[1]]$content

# Stem the words
corpus = tm_map(corpus, stemDocument)
corpus[[1]]$content

# Build a document term matrix 
dtm = DocumentTermMatrix(corpus)
dtm

spdtm = removeSparseTerms(dtm, 0.95)
spdtm

emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))


# What is the word stem that shows up most frequently across all the emails in the dataset?
sort(colSums(emailsSparse))

emailsSparse$spam = emails$spam

sort(colSums(subset(emailsSparse, emailsSparse$spam == 0)) >= 5000)
table(sort(colSums(subset(emailsSparse, emailsSparse$spam == 0)) >= 5000))

# stem is a dependent varaible, not a stem
table(sort(colSums(subset(emailsSparse, emailsSparse$spam == 1)) >= 1000))

emailsSparse$spam = as.factor(emailsSparse$spam)
library(caTools)
set.seed(123)
split = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, split == TRUE)
test = subset(emailsSparse, split == FALSE)

spamLog = glm(spam ~ ., data=train, family=binomial)
summary(spamLog)
logpredictions = predict(spamLog, newdata=train, type="response")
table(logpredictions < 0.00001)
table(logpredictions >  0.99999)
table(logpredictions >= 0.00001 & logpredictions <= 0.99999 )

table(train$spam, logpredictions > 0.5)
# FALSE TRUE
#0  3052    0
#1     4  954
(3052+954)/(3052+954+4) # 0.9990025

#  training set AUC of spamLog
library(ROCR)
ROCRpredTrain = prediction(logpredictions, train$spam)
auc = as.numeric(performance(ROCRpredTrain,"auc")@y.values)
auc

# Build a CART model
library(rpart)
library(rpart.plot)
spamCART = rpart(spam ~ ., data=train, method="class")
prp(spamCART)
CARTpredictions = predict(spamCART, newdata=train)[,2]
table(train$spam, CARTpredictions > 0.5)
(2885+894)/(167+64+2885+894)
ROCRpredTrainCART = prediction(CARTpredictions, train$spam)
aucCART = as.numeric(performance(ROCRpredTrainCART,"auc")@y.values)
aucCART

library(randomForest)
set.seed(123)
spamRF = randomForest(spam ~ ., data = train, type="prob")
predictRF = predict(spamRF, type="prob")[,2]
table(train$spam, predictRF > 0.5)
(3013+914)/(3013+914+39+44)
ROCRpredTrainRF = prediction(predictRF, train$spam)
aucRF = as.numeric(performance(ROCRpredTrainRF,"auc")@y.values)
aucRF

logpredictionstest = predict(spamLog, newdata=test, type="response")
table(test$spam, logpredictionstest > 0.5)
(1257+376)/(1257+376+51+34) # 0.9505239
ROCRpredtestlog = prediction(logpredictionstest, test$spam)
auclogtest = as.numeric(performance(ROCRpredtestlog,"auc")@y.values)
auclogtest # 0.9627517


CARTpredictionstest = predict(spamCART, newdata=test)[,2]
table(test$spam, CARTpredictionstest > 0.5)
(1228+386)/(1228+386+80+24) # 0.9394645
ROCRpredTestCART = prediction(CARTpredictionstest, test$spam)
aucCARTtest = as.numeric(performance(ROCRpredTestCART,"auc")@y.values)
aucCARTtest # 0.963176

predictRFtest = predict(spamRF, newdata=test, type="prob")[,2]
table(test$spam, predictRFtest > 0.5)
(1290+385)/(1290+385+18+25) # 0.9749709
ROCRpredTestRF = prediction(predictRFtest, test$spam)
aucRFtest = as.numeric(performance(ROCRpredTestRF,"auc")@y.values)
aucRFtest # 0.9975656
