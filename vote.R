# HW Why people vote
rm(list=ls())
data = read.csv("gerber.csv")
str(data)
summary(data)

# Which of the four "treatment groups" had the largest percentage of people who actually voted (voting = 1)?
tapply(data$voting, data$hawthorne, mean)
tapply(data$voting, data$civicduty, mean)
tapply(data$voting, data$neighbors, mean)
tapply(data$voting, data$self, mean)

# logistic regression model
Log = glm(voting ~ hawthorne + civicduty + neighbors + self, data=data, family=binomial)
summary(Log)

predict = predict(Log, type="response")
summary(predict)

# Using a threshold of 0.3, what is the accuracy of the logistic regression model?
table(data$voting, predict > 0.3)
#      predicted 0   | predicted 1
#         ___________________________
# actual 0 |  134513     100875
# actual 1 |   56730      51966

# accuracy  - 0.5419578
sum(diag(table(data$voting, predict > 0.3)))/nrow(data)

# Using a threshold of 0.5, what is the accuracy of the logistic regression model?
table(data$voting, predict > 0.5)

#      predicted 0   | predicted 1
#         ___________________________
# actual 0 |  235388     0
# actual 1 |  108696     0

# accuracy  - 0.6841004
235388/nrow(data)

# baseline - predict 0 (did not vote)
# accuracy  - 0.6841004

# AUC of the model
# ROCR curve
library(ROCR)
PredictROC = predict(Log)
summary(PredictROC)

pred = prediction(PredictROC, data$voting)
perf = performance(pred, "tpr", "fpr")
plot(perf)

# AUC of the Logistic regression model
as.numeric(performance(pred,"auc")@y.values)

# Regression tree
CARTmodel = rpart(voting ~ hawthorne + civicduty + neighbors + self, data=data )

# Plot the tree
prp(CARTmodel)

CARTmodel2 = rpart(voting ~ hawthorne + civicduty + neighbors + self, data=data, cp=0.0)
prp(CARTmodel2)

# New tree including sex variable
CARTmodel3 = rpart(voting ~ hawthorne + civicduty + neighbors + self + sex, data=data, cp=0.0)
prp(CARTmodel3)

# In the control group, which gender is more likely to vote?
# sex (0 for male, 1 for female)
tapply(data$control, data$sex, mean)

# Create a regression tree using just the "control" variable
CARTmodelcontrol = rpart(voting ~ control, data=data, cp=0.0)
prp(CARTmodelcontrol, digits = 6)
# control group - 0.296638
# not control group - 0.34
abs(0.34-0.296638)
 
# create another tree with the "control" and "sex" variables
CARTmodelcontrolsex = rpart(voting ~ control + sex, data=data, cp=0.0)
prp(CARTmodelcontrolsex, digits=6)

abs(0.334176 - 0.290456) # women not in control vs women in control group
abs(0.345818 - 0.302795) # men not in control vs men in control group
abs(0.334176 - 0.290456) - abs(0.345818 - 0.302795)

# Log regression with sex and control
# logistic regression model
Logsexcontrol = glm(voting ~ sex + control, data=data, family=binomial)
summary(Logsexcontrol)

# 4 possibilities: (Man, Not Control), (Man, Control), (Woman, Not Control), (Woman, Control).
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(Logsexcontrol, newdata=Possibilities, type="response")

# What is the absolute difference between the tree and the logistic regression 
# for the (Woman, Control) case? 
# logistic regression: 0.2908065
# tree: 0.290456
abs(0.2908065 - 0.290456)

# combination of the "sex" and "control" variables 
# New variable: 1 designates Woman and Control Group
Logsexcontrol2 = glm(voting ~ sex + control + sex:control, data=data, family = binomial)
summary(Logsexcontrol2)

#  calculate the average for each group:
predict(Logsexcontrol2, newdata=Possibilities, type="response")

# What is the absolute difference between the tree and the new logistic regression 
# for the (Woman, Control) case? 
# logistic regression: 0.2904558
# tree: 0.290456
abs(0.2904558 - 0.290456 )

# Note: We should not use all possible interaction terms in a logistic 
# regression model due to overfitting. 