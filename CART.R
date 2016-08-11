# Trees and Random Forest to predict Judge's decision to reverse (1) or uphold (0)  a decision.
stevens = read.csv("stevens.csv")
str(stevens)
summary(stevens)
install.packages("caTools")
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, spl == TRUE)
Test = subset(stevens, spl == FALSE)
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

# make CART Tree
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, method="class", minbucket=25)
prp(StevensTree)
table(Train$Respondent)

# make predictions on test data
PredictCART = predict(StevensTree, newdata=Test, type="class")

# confusion matrix
table(Test$Reverse, PredictCART)

#     0   |   1
#   ____________
# 0 |  41    36
# 1 |  22    71

# accuracy
(41+71)/(41+36+71+22)  # 65.9%


# Logistic regression model
StevensLog = glm(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, family=binomial)
summary(StevensLog)
predictLog = predict(StevensLog, type="response", newdata=Test)
table(Test$Reverse, predictLog > 0.5)
#     0   |   1
#   ____________
# 0 |  47    30
# 1 |  27    66

# accuracy 
(47+66)/(47+30+27+66)   # 66.5 %

# Baseline model - always predict most frequent
mean(Train$Reverse) # predict 1
#     0   |   1
#   ____________
# 0 |  0     77
# 1 |  0     93

# accuracy
93/(93+77)   # 54.7 %

# ROCR curve
install.packages("ROCR")
library(ROCR)
PredictROC = predict(StevensTree, newdata=Test)
PredictROC

pred = prediction(PredictROC[,2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)

# AUC of the CART model
as.numeric(performance(pred,"auc")@y.values)


# Build a CART model with minbucket = 5
StevensTree5 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, method="class", minbucket=5)
prp(StevensTree5)

# Build a CART model with minbucket = 100
StevensTree100 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, method="class", minbucket=100)
prp(StevensTree100)

# Random Forests
install.packages("randomForest")
library(randomForest)

StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, nodesize=25, ntree=200)

# Outcome needs to be a factor to use classification instead of regression
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

# Random Forest model
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, nodesize=25, ntree=200)

# Predictions
PredictForest = predict(StevensForest, newdata = Test)

# Confusion Matrix
table(Test$Reverse, PredictForest)
#      0   |  1
#   ____________
# 0 |  40    37
# 1 |  19    74

# accuracy
(40+74)/(40+74+19+37) # 67%

# Choosing different seeds
set.seed(100)
spl100 = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train100 = subset(stevens, spl100 == TRUE)
Test100 = subset(stevens, spl100 == FALSE)

# Outcome needs to be a factor to use classification instead of regression
Train100$Reverse = as.factor(Train100$Reverse)
Test100$Reverse = as.factor(Test100$Reverse)

# Random Forest model
StevensForest100 = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train100, nodesize=25, ntree=200)

# Predictions
PredictForest100 = predict(StevensForest100, newdata = Test100)

# Confusion Matrix
table(Test100$Reverse, PredictForest100)
#      0   |  1
#   ____________
# 0 |  42    35
# 1 |  18    75

# accuracy
(42+75)/(42+75+18+35) # 68.8%

# Choosing different seeds
set.seed(200)
spl200 = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train200 = subset(stevens, spl200 == TRUE)
Test200 = subset(stevens, spl200 == FALSE)

# Outcome needs to be a factor to use classification instead of regression
Train200$Reverse = as.factor(Train200$Reverse)
Test200$Reverse = as.factor(Test200$Reverse)

# Random Forest model
StevensForest200 = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train200, nodesize=25, ntree=200)

# Predictions
PredictForest200 = predict(StevensForest200, newdata = Test200)

# Confusion Matrix
table(Test200$Reverse, PredictForest200)

# accuracy
(39+73)/(39+73+20+38) # 65.9

# =================== Cross Validation ======================
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)
numFolds = trainControl(method="cv", number=10)
cpGrid = expand.grid(.cp=seq(0.01,0.5,0.01))

# CV
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, method="rpart", trControl=numFolds, tuneGrid = cpGrid)

StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, method="class", cp = 0.18)

PredictCV = predict(StevensTreeCV, newdata = Test, type="class")
table(Test$Reverse, PredictCV)
#      0   |  1
#   ____________
# 0 |  59    18
# 1 |  29    64

# accuracy
(59+64)/(59+64+18+29)  # 0.724

# plot the tree
prp(StevensTreeCV)
