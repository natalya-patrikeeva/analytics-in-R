# FORECASTING INTEREST RATE HIKES BY THE U.S. FEDERAL RESERVE
rm(list=ls())
fedFunds = read.csv("federalFundsRate.csv", stringsAsFactors = FALSE)
str(fedFunds)
summary(fedFunds)

# What proportion of months did the Fed raise the interest rate?
mean(fedFunds$RaisedFedFunds)

# Which Federal Reserve Chair has presided over the most interest rate decisions?
which.max(table(fedFunds$Chairman))

# Convert the following variables to factors using the as.factor function:

fedFunds$Chairman =  as.factor(fedFunds$Chairman)
fedFunds$DemocraticPres = as.factor(fedFunds$DemocraticPres)
fedFunds$RaisedFedFunds = as.factor(fedFunds$RaisedFedFunds)

# Which of the following methods requires the dependent variable be stored as 
# a factor variable when training a model for classification?
# -- randomForest() method.

# Obtain a random training/testing set split.
set.seed(201)
library(caTools)
spl = sample.split(fedFunds$RaisedFedFunds, 0.7)

training = subset(fedFunds, spl == TRUE)
testing = subset(fedFunds, spl == FALSE)
str(training)
summary(training)
str(testing)

# Train a logistic regression model 
Log = glm(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data=training, family=binomial)
summary(Log)

9.121012 + -0.003427*1.7 + 0.157658*-3 + -0.047449*5.1 + -0.136451*65.3 + 0.347829*0 + -0.006931*18

# plug this into the logistic response function to get the predicted probability.
# 0.3464

# What is the meaning of the coefficient labeled "DemocraticPres1" in the 
# logistic regression summary output?
# ---- When the president is Democratic, the odds of the federal funds rate 
# increasing are 41.6% higher than in an otherise identical month 
# (i.e. identical among the variables in the model).
exp(0.347829)  # the coefficients are log odds; the odds are 1.41599 or 41.6% higher odds of federal funds rate increasing.

# obtain predictions on the test set. Then, using a probability threshold of
# 0.5, create a confusion matrix for the test set.
logpredictionstest = predict(Log, newdata=testing, type="response")
table(testing$RaisedFedFunds, logpredictionstest > 0.5)
#   FALSE TRUE
# 0    60   27
# 1    31   57

# the naive baseline model we use in this class always predicts the most
# frequent outcome in the training set for all observations in the test set.
table(training$RaisedFedFunds)
# baseline model always predicts 1
table(testing$RaisedFedFunds)

# What is the test-set AUC of the logistic regression model?
library(ROCR)
PredictROC = predict(Log, newdata=testing)
PredictROC
pred = prediction(PredictROC, testing$RaisedFedFunds)
perf = performance(pred, "tpr", "fpr")
plot(perf)
# AUC
as.numeric(performance(pred,"auc")@y.values)
# The AUC is the proportion of time the model can differentiate between a 
# randomly selected true positive and true negative.
# The proportion of the time the model can differentiate between a randomly
# selected month during which the federal funds were raised and a randomly 
# selected month during which the federal funds were not raised. 

# Which logistic regression threshold is associated with the upper-right 
# corner of the ROC plot (true positive rate 1 and false positive rate 1)?
# A model with threshold 0 predicts 1 for all observations, yielding a 100% 
# true positive rate and a 100% false positive rate.
