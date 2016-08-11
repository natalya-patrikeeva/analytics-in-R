# Classification of Quality of healthcare - poor and good

quality = read.csv("quality.csv")
str(quality)
table(quality$PoorCare)

# Baseline model - most frequent outcome
# Predict all outcomes are 0 (good)
# Then, accuracy of baseline model is:
98/131 

# randomly split test and train datasets.

install.packages("caTools")

library(caTools)

# want to make sure the random split is the same for everyone
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
split   # TRUE = training set

qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)
nrow(qualityTrain)
nrow(qualityTest)

# spl = sample(1:nrow(data), size=0.7 * nrow(data))

# train = data[spl,]

# test = data[-spl,]

# Logistic regression model
QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family=binomial)
summary(QualityLog)

# best model is min AIC

# Get probabilities of poor quality (y=1 for poor)
predictTrain = predict(QualityLog, type="response")
summary(predictTrain)

# predicting higher probablitiy for actual poor quality cases as expected?
# average predictions for each healthcare outcome
tapply(predictTrain, qualityTrain$PoorCare, mean)


# Confusion matrix
#                   | predicted = 0 (good)   |  predicted = 1 (poor)
#                   _______________________________________________
# Actual = 0 (good) |   True negatives (TN)  |  False positives (FP)
# Actual = 1 (poor) |  False negatives (FN)  |   True positives (TP)
# 
# TN = predicted good quality and it was good
# FN = predicted good but it was poor quality
# FP = predicted poor but it was good quality
# TP = predicted poor and it was poor

# Sensitivity = TP/(TP + FN)
# percentage of poor quality cases that we classified correctly.

# Specificity = TN/(TN + FP)
# percentage of good quality cases that we classified correctly.

# N = number of observations
# Overall accuracy = (TN+TP)/N

# Overall error rate = (FP+FN)/N

# False negative error rate = FN/(TP+FN)

# False positive error rate = FP/(TN+FP) = 1 - Specificity

# Higher threshold - lower sensitivity & higher specificity (detect few really bad cases)
# Lower threshold - higher sensitivity & lower specificity (detect all cases that migbe be poor)

# Logistic regression makes predictions about probabilities which we transform into a binary
# outcome (poor/good care) using a threshold.
table(qualityTrain$PoorCare, predictTrain > 0.5)

# sensitivity
10/25
# specificity
70/74


# change the threshold to 0.7
table(qualityTrain$PoorCare, predictTrain > 0.7)
# sensitivity
8/25

# specificity
73/74

# change the threshold to 0.2
table(qualityTrain$PoorCare, predictTrain > 0.2)
# sensitivity
16/25

# specificity
54/74

# Logistic regression model using different independent variables
QualityLog2 = glm(PoorCare ~ StartedOnCombination + ProviderCount, data = qualityTrain, family=binomial)
summary(QualityLog2)

# ROC Curves
install.packages("ROCR")
library(ROCR)

ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at = seq(0, 1 , 0.1), text.adj = c(-0.5, 0.8) )

# Make prediction on a test set
predictTest = predict(QualityLog, type="response", newdata = qualityTest)
summary(predictTest)

# confusion matrix
table(qualityTest$PoorCare, predictTest > 0.3)

# test data: 24 - good; 8 - poor
table(qualityTest$PoorCare)

# Overall accuracy -  78%
25/32

# False positive rate
5/24

# True positive rate (Sensitivity)
6/8

# Base model - predict good quality for all cases. We'd be correct 19+5 = 24 
# versus 25 cases we identified correctly in our model.

# You can compute the test set AUC (area under ROC curve).
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
ROCRperfTest = performance(ROCRpredTest, "tpr", "fpr")
plot(ROCRperfTest, main="Test Data ROC", colorize=TRUE, print.cutoffs.at = seq(0, 1 , 0.1), text.adj = c(-0.4, 1) )

auc = as.numeric(performance(ROCRpredTest,"auc")@y.values)
auc

# The AUC of a model has the following nice interpretation: given a random patient from the 
# dataset who actually received poor care, and a random patient from the dataset who actually 
# received good care, the AUC is the perecentage of time that our model will classify which 
# is which correctly.