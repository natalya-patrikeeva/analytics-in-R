# multiple imputation of polling data based on non-missing values for an observation
rm(list=ls())
polling = read.csv("PollingData.csv")
str(polling)
summary(polling)
head(polling)
table(polling$Year)

# Multiple Imputation by Chained Equations
install.packages("mice")
library(mice)
simple = polling[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
summary(simple)
head(simple)

set.seed(144)
imputed = complete(mice(simple))
summary(imputed)
summary(polling)

polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA
summary(polling)

Train = subset(polling, Year < 2012)
Test = subset(polling, Year == 2012)

# baseline model - always predict 1 - 53/(47+53) = 0.53 accuracy
table(Train$Republican)

# smart baseline based on a poll
?sign
sign(20)
sign(-10)
sign(0)

table(sign(Train$Rasmussen))
table(Train$Republican, sign(Train$Rasmussen)) 

cor(Train)
str(Train)

cor(Train[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount", "Republican")])

# chose one highly correlated variable
mod1 = glm(Republican ~ PropR, data=Train, family="binomial")
summary(mod1)

pred1 = predict(mod1, type="response")
table(Train$Republican, pred1 >=0.5)

# 2-variables that are least correlated
mod2 = glm(Republican ~ SurveyUSA+DiffCount, data=Train, family="binomial")
pred2 = predict(mod2, type = "response")
table(Train$Republican, pred2 >=0.5)
summary(mod2)

# test data
table(Test$Republican, sign(Test$Rasmussen))
TestPrediction = predict(mod2, newdata = Test, type="response")
table(Test$Republican, TestPrediction >=0.5)

subset(Test, TestPrediction >= 0.5 & Republican == 0)
