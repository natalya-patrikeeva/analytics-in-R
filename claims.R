Claims = read.csv("ClaimsData.csv")
str(Claims)
summary(Claims)
# percentage of patients in each cost buckets
table(Claims$bucket2009)/nrow(Claims)

library(caTools)
set.seed(88)
spl = sample.split(Claims$bucket2009, SplitRatio = 0.6)
ClaimsTrain = subset(Claims, spl == TRUE)
ClaimsTest = subset(Claims, spl == FALSE)
str(ClaimsTrain)
str(ClaimsTest)
summary(ClaimsTrain$age)

# What proportion of people in the training set (ClaimsTrain) had at least one
# diagnosis code for diabetes?
mean(ClaimsTrain$diabetes)

# Baseline model is predicting 2009 buckets as 2008 buckets 
table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)

# accuracy
(110138+10721+2774+1539+104)/nrow(ClaimsTest) # 0.68

# penalty matrix: actual outcomes on the left and predicted outcomes on the top
PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0 ), byrow = TRUE, nrow=5)
PenaltyMatrix

# penalty error of the baseline model = classification matrix x penalty matrix
as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix

sum(as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix)/nrow(ClaimsTest)
# 0.74 penalty error

# Baseline method of predicting cost bucket 1 for everyone
baseone = rep(1,nrow(ClaimsTest))
table(baseone)
table(ClaimsTest$bucket2009, baseone)

# accuracy - 0.67
# (110138 + 7787 + 3427 + 1452 + 174)/nrow(ClaimsTest)
122978/nrow(ClaimsTest)

PenaltyMatrixOne = c(0,2,4,6,8)
PenaltyMatrixOne

# penalty error
table(ClaimsTest$bucket2009, baseone)*PenaltyMatrixOne
sum(table(ClaimsTest$bucket2009, baseone)*PenaltyMatrixOne)/nrow(ClaimsTest)

# CART model to predict healthcare costs
library(rpart)
library(rpart.plot)
ClaimsTree = rpart(bucket2009 ~ . -reimbursement2009, data = ClaimsTrain, method="class", cp=0.00005)

# cp value was chosen through CV on the training set

prp(ClaimsTree)

# predictions on test set
PredictTest = predict(ClaimsTree, newdata = ClaimsTest, type="class")
table(ClaimsTest$bucket2009, PredictTest)

# accuracy - 0.71
sum(diag(table(ClaimsTest$bucket2009, PredictTest)))/nrow(ClaimsTest)

# penalty
as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix
sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix)/nrow(ClaimsTest)

# CART model with penalty matrix
ClaimsTreePen = rpart(bucket2009 ~ . -reimbursement2009, data = ClaimsTrain, method="class", cp=0.00005, parms = list(loss=PenaltyMatrix))
prp(ClaimsTreePen)
PredictTestPen = predict(ClaimsTreePen, newdata = ClaimsTest, type="class")
table(ClaimsTest$bucket2009, PredictTestPen)

# accuracy 0.647
sum(diag(table(ClaimsTest$bucket2009, PredictTestPen)))/nrow(ClaimsTest)

# penalty
as.matrix(table(ClaimsTest$bucket2009, PredictTestPen))*PenaltyMatrix
sum(as.matrix(table(ClaimsTest$bucket2009, PredictTestPen))*PenaltyMatrix)/nrow(ClaimsTest)

# comparing two CART models without and with penalty matrix
colSums(table(ClaimsTest$bucket2009, PredictTest))
144027/nrow(ClaimsTest) # 0.786165

colSums(table(ClaimsTest$bucket2009, PredictTestPen))
106515/nrow(ClaimsTest) # 0.5814074
