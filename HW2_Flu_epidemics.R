# HW2 Flu epidemics via search engine query data
FluTrain = read.csv("FluTrain.csv")
str(FluTrain)
summary(FluTrain)

# Looking at the time period 2004-2011, which week corresponds to the highest percentage 
# of ILI-related physician visits?

which.max(FluTrain$ILI)
FluTrain$Week[303]

# Which week corresponds to the highest percentage of ILI-related query fraction?
which.max(FluTrain$Queries)

# Plot the histogram of the dependent variable, ILI
hist(FluTrain$ILI) # most of the ILI values are smal, with a relatively small number of larger
                   # values (data is "skew right")

# Plot the natural logarithm of ILI versus Queries.
plot(log(FluTrain$ILI), FluTrain$Queries, xlab="log of ILI-related physician visits", ylab="Queries")
plot(FluTrain$Queries, log(FluTrain$ILI), xlab="Queries", ylab="log of ILI-related visits")

# Model to estimate log(ILI) = intercept + coefficient x Queries, coefficient > 0
FluTrend1 = lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)

# For a single variable linear regression model, there is a direct relationship between
# the R-squared and the correlation between the independent and the dependent variables.
# What is the relationship we infer from our problem? 
# R-squared = correlation^2
cor(log(FluTrain$ILI), FluTrain$Queries)

# Performance on the Test Set
FluTest = read.csv("FluTest.csv")
str(FluTest)
summary(FluTest)

# We can convert from predictions of log(ILI) to predictions of ILI via exponentiation.
predTest1 = exp(predict(FluTrend1, newdata = FluTest))

# What is our estimate for the percentage of ILI-related physician visits 
# for the week of March 11, 2012?
predTest1
which(FluTest$Week == "2012-03-11 - 2012-03-17")
predTest1[11]

# What is the relative error betweeen the estimate (our prediction) 
# and the observed value for the week of March 11, 2012?
(FluTest$ILI[11] - predTest1[11])/FluTest$ILI[11] 

# What is the Root Mean Square Error (RMSE) between our estimates and the actual 
# observations for the percentage of ILI-related physician visits, on the test set?
SSE = sum((predTest1 - FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))
RMSE
sqrt(mean((predTest1-FluTest$ILI)^2))

# Training a Time Series Model
# install and load the zoo package which provides a number of helpful methods for time series models

install.packages("zoo")
library(zoo)

# build a variable called ILILag2 that contains the ILI value from 2 weeks before
# the current observation.
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain)

# plot the log of ILILag2 against the log of ILI. 
# Which best describes the relationship between these two variables?
plot(log(FluTrain$ILILag2), log(FluTrain$ILI), xlab="log of ILILag2", ylab="log of ILI")

# Train a linear regression model on the FluTrain dataset to predict the log of the ILI 
# variable using the Queries variable as well as the log of the ILILag2 variable. 
FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)

# Add an ILILag2 variable to the FluTest data frame.
ILILag2Test = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2Test)
summary(FluTest)
FluTest$ILILag2

# Fill in the missing values for ILILag2 in FluTest.
str(FluTrain)
nrow(FluTrain)
FluTest$ILILag2[1] = FluTrain$ILI[nrow(FluTrain)-1]
FluTest$ILILag2[2] = FluTrain$ILI[nrow(FluTrain)]

# Obtain test set predictions of the ILI variable from the FluTrend2 model.
predTest2 = exp(predict(FluTrend2, newdata = FluTest))
predTest2

# What is the test-set RMSE of the FluTrend2 model?
RMSE2 = sqrt(mean((predTest2-FluTest$ILI)^2))
RMSE2

# In statistics and econometrics, and in particular in time series analysis, 
# an autoregressive integrated moving average (ARIMA) model is a generalization 
# of an autoregressive moving average (ARMA) model.
?arima
