NBA = read.csv("NBA_train.csv")
str(NBA)
summary(NBA)
table(NBA$W, NBA$Playoffs)
NBA$PTSdiff = NBA$PTS - NBA$oppPTS
plot(NBA$PTSdiff, NBA$W)
WinsReg = lm(W ~ PTSdiff, data = NBA)
summary(WinsReg)

# W = 41 + 0.03259*PTSdiff >= 42
# PTSdiff >= 42-41/(0.0326) = 30.67

# Predict Points Scored
PointsReg = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL+ BLK, data=NBA )
summary(PointsReg)
PointsReg$residuals
SSE = sum(PointsReg$residuals^2)
SSE
RMSE = sqrt(SSE/nrow(NBA))
RMSE
mean(NBA$PTS)

# TOV is the least statistically significant vairable - remove it
PointsReg2 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL+ BLK, data=NBA )
summary(PointsReg2)

# Remove DRB
PointsReg3 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL+ BLK, data=NBA )
summary(PointsReg3)

# Remove BLK
PointsReg4 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data=NBA )
summary(PointsReg4)
str(PointsReg4)
SSE4 = sum(PointsReg4$residuals^2)
RMSE4 = sqrt(SSE4/nrow(NBA))
SSE4
RMSE4

# Load the test data 
NBA_test = read.csv("NBA_test.csv")
str(NBA_test)
summary(NBA_test)
# Predict how many points we'll see in the 2012-2013 season
PointsPredictions = predict(PointsReg4, newdata = NBA_test)
PointsPredictions
str(PointsPredictions)
summary(PointsPredictions)
# Out of sample R^2 - how well the model does with unseen data
SSE_test = sum((PointsPredictions - NBA_test$PTS)^2)
SST = sum((mean(NBA$PTS) - NBA_test$PTS)^2)
R2 = 1 - SSE_test/SST
R2
RMSE = sqrt(SSE_test/nrow(NBA_test))
RMSE
