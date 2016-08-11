# HW2 State Data
rm(list=ls())
data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center, state.division, state.name, state.region)
str(statedata)
summary(statedata)

# Plot all of the states' centers with latitude on the y axis and longitude on the x axis.
plot(statedata$x,statedata$y, xlab="longitude",ylab="latitude")

# determine which region of the US (West, North Central, South, or Northeast) 
# has the highest average high school graduation rate of all the states in the region.
tapply(statedata$HS.Grad, statedata$state.region,  mean)
which.max(tapply(statedata$HS.Grad, statedata$state.region,  mean))
boxplot(statedata$HS.Grad ~ statedata$state.region, xlab="State Region", ylab="HS Graduation Rate")

# Which region has the highest median murder rate?
boxplot(statedata$Murder ~ statedata$state.region, col = "lightgray", xlab="State Region", ylab="Murder Rate")
?boxplot
tapply(statedata$Murder, statedata$state.region, median)

# Which state is the outlier in the Northeast region?
Northeast = subset(statedata, statedata$state.region == "Northeast")
which.max(tapply(Northeast$Murder, Northeast$state.name, max))

# build a model to predict life expectancy by state.
str(statedata)
LifeReg = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data=statedata)
summary(LifeReg)

# plot a graph of life expectancy vs. income
plot(statedata$Income, statedata$Life.Exp, xlab="Income", ylab="Life Expectancy")

# Remove insignificant variables one at a time to deal with multicollinearity.
# Find a good model with only 4 independent variables, instead of the original 7. 
# Which variables does this model contain?
LifeReg6 = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost, data=statedata)
summary(LifeReg6)
LifeReg5 = lm(Life.Exp ~ Population + Income + Murder + HS.Grad + Frost, data=statedata)
summary(LifeReg5)
LifeReg4 = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data=statedata)
summary(LifeReg4)

# Using the simplified 4 variable model that we created, we'll now take a look at 
# how our predictions compare to the actual values.
Pred = predict(LifeReg4)
Pred
sort(Pred, decreasing = TRUE)

# Which state actually has the lowest life expectancy? 
statedata$state.name[which.min(statedata$Life.Exp)]

# Which state do we predict to have the highest life expectancy?
sort(Pred, decreasing = FALSE)

# Which state actually has the highest life expectancy?
statedata$state.name[which.max(statedata$Life.Exp)]

# For which state do we make the smallest absolute error?
sort(abs(LifeReg4$residuals), decreasing = TRUE)
