# Predicting 10-year risk of Coronary Heart Disease
framingham = read.csv("framingham.csv")
str(framingham)

# split data into training and test data
library(caTools)
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
train = subset(framingham, split == TRUE)
test = subset(framingham, split == FALSE)
str(train)
summary(train)
# Logistic regression model with all independent variables (risk factors)
framinghamLog = glm(TenYearCHD ~ ., data = train, family=binomial)
summary(framinghamLog)

# Make predictions on test data
predictTest = predict(framinghamLog, type="response", newdata=test)

# Confusion matrix
table(test$TenYearCHD, predictTest > 0.5)
# TRUE outcome is rare - model rarely predicts a 10-year CHD risk above 50%)

# accuracy
(1069+11)/(1069+6+187+11)

# Baseline model accuracy. Most frequent outcome is 0. Always predict 0.
summary(train$TenYearCHD)
(1069+6)/(1069+6+187+11)

# Out of sample AUC.
library(ROCR)
ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Confusion matrix
#                     | predicted = 0 (no CHD)   |  predicted = 1 (CHD)
#                   _______________________________________________
# Actual = 0 (no CHD) |   True negatives (TN)  |  False positives (FP)
# Actual = 1 (CHD)    |  False negatives (FN)  |   True positives (TP)
# 
# TN = predicted no CHD and patient did not develop CHD
# FN = predicted no CHD but patient developed CHD
# FP = predicted CHD but patient did not develop CHD
# TP = predicted CHD and patient developed CHD

# Sensitivity = TP/(TP + FN)
# percentage of CHD cases that we classified correctly.
11/(11+187)
# Specificity = TN/(TN + FP)
# percentage of no CHD cases that we classified correctly.
1069/(1069+6)

