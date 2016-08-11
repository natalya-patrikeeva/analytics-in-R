# cluster-then-predict to predict future stock prices using historical stock data.
# predict whether or not the stock return in December will be positive, 
# using the stock returns for the first 11 months of the year.
rm(list=ls())
stocks = read.csv("StocksCluster.csv")
str(stocks)
summary(stocks)

# What is the maximum correlation between any two return variables in the dataset? 
cor(stocks)
sort(cor(stocks))

# Initial Logistic Regression Model
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

StocksModel = glm(PositiveDec ~ ., data=stocksTrain, family=binomial)
summary(StocksModel)

# What is the overall accuracy on the training set, using a threshold of 0.5?
logpredictions = predict(StocksModel, newdata=stocksTrain, type="response")
table(stocksTrain$PositiveDec, logpredictions > 0.5)
#  FALSE TRUE
# 0   990 2689
# 1   787 3640
(990+3640)/(990+2689+787+3640) # 0.5711818

# test set predictions.
# What is the overall accuracy of the model on the test with a threshold of 0.5?
logpredictionstest = predict(StocksModel, newdata=stocksTest, type="response")
table(stocksTest$PositiveDec, logpredictionstest > 0.5)
#   FALSE TRUE
# 0   417 1160
# 1   344 1553
(417+1553)/(417+1160+344+1553) # 0.5670697

# What is the accuracy on the test set of a baseline model that always predicts
# the most common outcome (PositiveDec = 1)?
# Confusion matrix
#                     | predicted = 0    |  predicted = 1 
#                   _______________________________________________
# Actual = 0  |   True negatives (TN)  |  False positives (FP)
# Actual = 1  |  False negatives (FN)  |   True positives (TP)
#   FALSE TRUE
# 0   0   1577
# 1   0   1897   

table(stocksTest$PositiveDec)
1897/(1897+1577) # 0.5460564

# Clustering Stocks
limitedTrain = stocksTrain
limitedTest = stocksTest
limitedTrain$PositiveDec = NULL
limitedTest$PositiveDec = NULL

# normalize by the mean and standard deviation of the variables in the training set.
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
str(normTrain)
summary(normTrain)
summary(normTest)
sd(normTest$ReturnJan)
mean(stocksTrain$ReturnJan)
mean(stocksTest$ReturnJan)

# Run k-means clustering with 3 clusters on normTrain.
k = 3
set.seed(144)
km = kmeans(normTrain, centers = k)
str(km)
table(km$cluster)
km$centers
km$size

# use the flexclust package to obtain training set and testing set cluster
# assignments for our observations.
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata = normTest)
table(clusterTest)
str(clusterTrain)
summary(clusterTrain)

# Cluster-Specific Predictions
stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
str(stocksTrain1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)
str(stocksTrain2)
str(stocksTrain3)
str(stocksTrain)
summary(stocksTrain1)
summary(stocksTrain2)
summary(stocksTrain3)
mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)

# logistic regression models for each cluster
StocksModel1 = glm(PositiveDec ~ ., data=stocksTrain1, family=binomial)
StocksModel2 = glm(PositiveDec ~ ., data=stocksTrain2, family=binomial)
StocksModel3 = glm(PositiveDec ~ ., data=stocksTrain3, family=binomial)
summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

PredictTest1 = predict(StocksModel1, newdata = stocksTest1, type = "response")
table(stocksTest1$PositiveDec, PredictTest1 > 0.5)
#  FALSE TRUE
# 0    30  471
# 1    23  774
(30+774)/(30+774+471+23) # 0.6194145

PredictTest2 = predict(StocksModel2, newdata = stocksTest2, type = "response")
table(stocksTest2$PositiveDec, PredictTest2 > 0.5)
#   FALSE TRUE
# 0   388  626
# 1   309  757
(388+757)/(388+757+626+309) # 0.5504808

PredictTest3 = predict(StocksModel3, newdata = stocksTest3, type = "response")
table(stocksTest3$PositiveDec, PredictTest3 > 0.5)
#  FALSE TRUE
# 0    49   13
# 1    21   13
(49+13)/(49+13+13+21) # 0.6458333


# combine all the test-set predictions into a single vector and all the true 
# outcomes into a single vector.
AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllOutcomes, AllPredictions > 0.5)
# AllOutcomes FALSE TRUE
#           0   467 1110
#           1   353 1544
(467+1544)/(467+1110+353+1544) # 0.5788716
