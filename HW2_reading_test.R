# HW2 reading test scores PISA 2009
rm(list=ls())
pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")
str(pisaTrain)
summary(pisaTrain)
str(pisaTest)
summary(pisaTest)

# what is the average reading test score of males and females?
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

# Removing missing values
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

#  Set the reference level of the unordered factor raceeth
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

# build a linear regression model using the training set to predict readingScore
# using all the remaining variables.
lmScore = lm(readingScore ~ ., data=pisaTrain)
summary(lmScore)

# What is the training-set root-mean squared error (RMSE) of the model?
SSE = sum(lmScore$residuals^2)
RMSE = sqrt(SSE/(nrow(pisaTrain)))
RMSE
mean(pisaTrain$readingScore)

# Consider two students A and B. They have all variable values the same, except that
# student A is in grade 11 and student B is in grade 9. What is the predicted reading 
# score of student A minus the predicted reading score of student B?
11*29.542707 - 9*29.542707

# What is the meaning of the coefficient associated with variable raceethAsian?
# Predicted difference in the reading score between an Asian student and a white student
# who is otherwise identical.

# Predicting the reading scores of students in pisaTest
predTest = predict(lmScore, newdata = pisaTest)
predTest
summary(predTest)
# What is the range between the maximum and minimum predicted reading score on the test set?
max(predTest) - min(predTest)

# What is the sum of squared errors (SSE) of lmScore on the testing set?
SSE = sum((predTest - pisaTest$readingScore)^2)
SSE

# What is the root-mean squared error (RMSE) of lmScore on the testing set?
RMSE = sqrt(SSE/nrow(pisaTest))
RMSE

# What is the predicted test score used in the baseline model? 
# Remember to compute this value using the training set and not the test set.
mean(pisaTrain$readingScore)

# What is the sum of squared errors of the baseline model on the testing set? 
# HINT: We call the sum of squared errors for the baseline model the total sum of squares (SST).
SST = sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
SST

# What is the test-set R-squared value of lmScore?
R2 = 1 - SSE/SST
R2
