# HW2 FORECASTING ELANTRA SALES 
# We will build a linear regression model to predict monthly sales of the Hyundai Elantra
# using economic indicators of the United States as well as Google search queries.

elantra = read.csv("elantra.csv")
str(elantra)
summary(elantra)
elantraTrain = subset(elantra, Year < 2013)
elantraTest = subset(elantra, Year > 2012)
str(elantraTrain)
summary(elantraTrain)
str(elantraTest)
summary(elantraTest)

# Build a linear regression model to predict monthly Elantra sales.
salesReg = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries, data = elantraTrain)
summary(salesReg)

# Pr(>|t|) statistical significance indicates how likely it is that, by chance, 
# the true coefficient is zero. P ~ 1 means coeff = 0; P ~ 0 means coeff != 0 

# incorporate the seasonal effect due to the month, build a new linear regression model
# that predicts monthly Elantra sales.
salesMonthlyReg = lm(ElantraSales ~ Month + Unemployment + CPI_all + CPI_energy + Queries, data = elantraTrain)
summary(salesMonthlyReg)

# what is the absolute difference in predicted Elantra sales given that one period
# is in January and one is in March and the rest of variables are the same?
abs(110.69*1 - 110.69*3)
abs(110.69*1 - 110.69*5)

# In fact, we must convert Month to a factor variable before adding it to the model.
# Re-run the regression with the Month variable modeled as a factor variable. 
elantraTrain$MonthFac = as.factor(elantraTrain$Month)
elantraTest$MonthFac = as.factor(elantraTest$Month)
str(elantraTrain)
summary(elantraTrain)

salesMonthlyFacReg = lm(ElantraSales ~ MonthFac + Unemployment + CPI_all + CPI_energy + Queries, data = elantraTrain)
summary(salesMonthlyFacReg)

# Multicolinearity
# compute the correlations of the variables in the training set
cor(elantraTrain[c("CPI_energy", "Month","Unemployment","Queries","CPI_all")])

# Which of the following variables is Queries highly correlated with? 
cor(elantraTrain[c("Queries","Month","Unemployment","CPI_energy","CPI_all")])

# A Reduced Model
salesRedReg = lm(ElantraSales ~ MonthFac + Unemployment + CPI_all + CPI_energy, data = elantraTrain)
summary(salesRedReg)

# make predictions on the test set. 
# What is the sum of squared errors of the model on the test set?

Pred = predict(salesRedReg, newdata = elantraTest)
Pred
SSE = sum((Pred - elantraTest$ElantraSales)^2)
SSE

# What would the baseline method predict for all observations in the test set? 
mean(elantraTrain$ElantraSales)

# What is the test set R-Squared?
SST = sum((mean(elantraTrain$ElantraSales) - elantraTest$ElantraSales)^2)
SST
Rsquared = 1 - (SSE/SST)
Rsquared

# What is the largest absolute error that we make in our test set predictions?
max(abs(Pred - elantraTest$ElantraSales))

# In which period (Month,Year pair) do we make the largest absolute error in our prediction?
which.max(abs(Pred - elantraTest$ElantraSales))
elantraTest$Month[5]
elantraTest$Year[5]
