parole = read.csv("parole.csv")
str(parole)
summary(parole)
table(parole$violator)

parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

model = glm(violator ~ ., data=train, family=binomial)
summary(model)

exp(1.6119919)

Pone = 1/(1 + exp(-1 * (-4.2411574 + 0.3869904*1 + 0.8867192*1 - 0.0001756*50 - 0.1238867*3 + 0.0802954*12 + 0.6837143)))
Pone

# P(y=0)
Pzero = 1 - Pone

# Odds
Odds = Pone/Pzero
Odds
# Logit
log(Odds)

# Make predictions on test data
predictTest = predict(model, type="response", newdata=test)
summary(predictTest)

# Confusion matrix
table(test$violator, predictTest > 0.5)

# Confusion matrix
#                     | predicted = 0    |  predicted = 1 
#                   _______________________________________________
# Actual = 0  |   True negatives (TN)  |  False positives (FP)
# Actual = 1  |  False negatives (FN)  |   True positives (TP)

# Sensitivity = TP/(TP + FN)
12/(12+11)

# specificity = TN/(TN + FP)
167/(167+12)

# accuracy
(167+12)/(167+12+12+11)

# baseline
(167+12)/(167+12+12+11)

library(ROCR)
ROCRpred = prediction(predictTest, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)
