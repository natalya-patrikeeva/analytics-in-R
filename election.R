# Election Forecasting
rm(list=ls())
library(ggplot2)
library(maps)
library(ggmap)
statesMap = map_data("state")
str(statesMap)
table(statesMap$group)
ggplot(statesMap, aes(x = long, y=lat, group=group)) + geom_polygon(fill="white", color="black")
polling = read.csv("PollingImputed.csv")
str(polling)
head(polling)
Train = subset(polling, Year < 2012)
Test = subset(polling, Year == 2012)

mod2 = glm(Republican ~ SurveyUSA + DiffCount, data = Train, family = binomial)
summary(mod2)
TestPrediction = predict(mod2, newdata=Test, type="response")
summary(TestPrediction)

TestPredictionBinary = as.numeric(TestPrediction > 0.5)
table(TestPredictionBinary)
head(TestPredictionBinary)

predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
str(predictionDataFrame)

predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
str(predictionDataFrame)

predictionMap = merge(statesMap, predictionDataFrame, by="region")
str(predictionMap)
predictionMap = predictionMap[order(predictionMap$order), ]
str(statesMap)

?merge
head(predictionMap)
head(statesMap)
nrow(table(statesMap$region))
table(predictionMap$region)
nrow(table(predictionDataFrame$region))

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) +
  geom_polygon(color = "black")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) +
  geom_polygon(color = "black") + scale_fill_gradient(low="blue", high="red", guide="legend", breaks=c(0,1), labels=c("Democrat","Republican"), name="Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) +
  geom_polygon(color = "black") + scale_fill_gradient(low="blue", high="red", guide="legend", breaks=c(0,1), labels=c("Democrat","Republican"), name="Prediction 2012")

summary(TestPrediction)

str(TestPrediction)
summary(subset(predictionMap$TestPrediction, predictionMap$region == "florida"))

?geom_polygon
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) +
  geom_polygon(color = "black", size=3) + scale_fill_gradient(low="blue", high="red", guide="legend", breaks=c(0,1), labels=c("Democrat","Republican"), name="Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) +
  geom_polygon(color = "black", linetype=3) + scale_fill_gradient(low="blue", high="red", guide="legend", breaks=c(0,1), labels=c("Democrat","Republican"), name="Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) +
  geom_polygon(color = "black", alpha=0.3) + scale_fill_gradient(low="blue", high="red", guide="legend", breaks=c(0,1), labels=c("Democrat","Republican"), name="Prediction 2012")
