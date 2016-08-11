climate = read.csv("climate_change.csv")
str(climate)
summary(climate) 

# Split data (308 obs) into training set - all observations up to 2006  (284)
# and a testing set - remaining years (24)
training = subset(climate, Year < 2007)
str(training)
summary(training)
testing = subset(climate, Year > 2006)
str(testing)
summary(testing)

# Linear regression model to predict Temp valiable using MEI, CO2, CH4, N2O, CFC.11, CFC.12, TSI and Aerosols
TempReg = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = training)
summary(TempReg)
# Mutiple R-squared = 0.7509
# Which variables are significant, p-value < 0.05 (*)? MEI, CO2, CH4, CFC.11, CFC.12, TSI, Aerosols

plot(training$N2O, training$Temp, xlab="N2O up to 2006", ylab="Temp difference")
plot(training$CFC.11, training$Temp, xlab="CFC-11 up to 2006", ylab="Temp difference")

# Compute the correlations between all the variables in the training set. 
# Which of the following independent variables is N2O highly correlated with 
# (absolute correlation greater than 0.7)? CO2, CH4, CFC.12
# Which of the following independent variables is CFC.11 highly correlated with? CH4, CFC.12
cor(training)

# Given that the correlations are so high, let us focus on the N2O variable and build 
# a model with only MEI, TSI, Aerosols and N2O as independent variables.
TempReg2 = lm(Temp ~ N2O + MEI + TSI + Aerosols, data=training)
summary(TempReg2)

# ?step will automate the procedure of trying different combinations of variables to find 
# a good compromise of model simplicity and R-squared. This trade-off is formalized by AIC.
TempStep = step(TempReg)
summary(TempStep)

# Using the model produced from the step function, calculate temperature predictions
# for the testing data set, using the predict function.
TempPredictions = predict(TempStep, newdata = testing)
summary(TempPredictions)
str(TempPredictions)
TempPredictions

# Out of sample R^2 - how well the model does with unseen data
SSE = sum((TempPredictions - testing$Temp)^2)
SST = sum((mean(training$Temp) - testing$Temp)^2)
R2 = 1 - SSE/SST
R2
