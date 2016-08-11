# multiclass classification problem for letter recognition - A,B,P,R

letters = read.csv("Letters_ABPR.csv")
str(letters)
summary(letters)

# create a new variable isB in the dataframe, which takes the value "TRUE" if the 
# observation corresponds to the letter B, and "FALSE" if it does not.
letters$isB = as.factor(letters$letter == "B")

# Now split the data set into a training and testing set, 
# putting 50% of the data in the training set. 
library(caTools)
set.seed(1000)
spl = sample.split(letters$isB, SplitRatio = 0.5)
train = subset(letters, spl == TRUE)
test = subset(letters, spl == FALSE)

# consider a baseline method that always predicts the most frequent outcome
# "not B"
table(letters$isB)
table(test$isB)
# What is the accuracy of this baseline method on the test set?
#      0    |   1
#   ____________
# 0 |  1175    0
# 1 |   383   0
1175/nrow(test)  #0.754172
