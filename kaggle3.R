# KAGGLE COMPETITION 
rm(list=ls())
train = read.csv("train2016.csv",  stringsAsFactors = TRUE)
test = read.csv("test2016.csv", stringsAsFactors = TRUE)
str(train)
str(test)
table(train$Party)
myvars = c("YOB","Gender","Income","HouseholdStatus","EducationLevel")
#limit = train[myvars]
limit = train
limit$Party = NULL
str(limit)
summary(limit)
TrainNumeric = lapply(limit, as.numeric)
summary(TrainNumeric)
TestNumeric = lapply(test, as.numeric)

str(TrainNumeric)
TrainNumeric = as.data.frame(TrainNumeric)
TestNumeric = as.data.frame(TestNumeric)

preproc = preProcess(TrainNumeric)
normTrain = predict(preproc, TrainNumeric)
normTest = predict(preproc, TestNumeric)
normTrain = normTrain[complete.cases(normTrain),]
normTest = normTest[complete.cases(normTest),]
str(normTrain)
set.seed(1)
km = kmeans(normTrain, centers = 4)
str(km)
table(km$cluster)
km$centers
km$size

library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)

clusterTest = predict(km.kcca, newdata = normTest)
table(clusterTest)
str(clusterTrain)
summary(clusterTrain)

# Cluster-Specific Predictions
Train1 = subset(train, clusterTrain == 1)
str(Train1)
summary(Train1)
Train1$Party
Test1 = subset(test, clusterTest == 1)

Train2 = subset(train, clusterTrain == 2)
str(Train2)
summary(Train2)
Train2$Party
Test2 = subset(test, clusterTest == 2)

Train3 = subset(train, clusterTrain == 3)
str(Train3)
summary(Train3)
Train3$Party
Test3 = subset(test, clusterTest == 3)

Train4 = subset(train, clusterTrain == 4)
str(Train4)
summary(Train4)
Train4$Party
Test4 = subset(test, clusterTest == 4)

# logistic regression models for each cluster
Model1 = glm(Party ~ . -USER_ID, data=Train1, family=binomial)
summary(Model1)
# make predictions on the test set:
PredTest1 = predict(Model1, newdata=Test1, type="response")
summary(PredTest1)
threshold = 0.5
PredTestLabels1 = as.factor(ifelse(PredTest1<threshold, "Democrat", "Republican"))
table(PredTestLabels1)   # 

Model2 = glm(Party ~ . -USER_ID, data=Train2, family=binomial)
summary(Model2)
# make predictions on the test set:
PredTest2 = predict(Model2, newdata=Test2, type="response")
summary(PredTest2)
threshold = 0.5
PredTestLabels2 = as.factor(ifelse(PredTest2<threshold, "Democrat", "Republican"))
table(PredTestLabels2)   # 

Model3 = glm(Party ~ . -USER_ID, data=Train3, family=binomial)
summary(Model3)
# make predictions on the test set:
PredTest3 = predict(Model3, newdata=Test3, type="response")
summary(PredTest3)
PredTestLabels3 = as.factor(ifelse(PredTest3<threshold, "Democrat", "Republican"))
table(PredTestLabels3)   # 

Model4 = glm(Party ~ . -USER_ID, data=Train4, family=binomial)
summary(Model4)
# make predictions on the test set:
PredTest4 = predict(Model4, newdata=Test4, type="response")
summary(PredTest4)
PredTestLabels4 = as.factor(ifelse(PredTest4<threshold, "Democrat", "Republican"))
table(PredTestLabels4)   # 
summary(PredTest4)
str(PredTestLabels4)
PredTestLabels4
Pred4 = as.data.frame(PredTestLabels4)
str(Pred4)
Pred4$USER_ID = Test4$USER_ID
Pred4
MySubmission4 = data.frame(USER_ID = Test4$USER_ID, Predictions = PredTestLabels4)

MySubmission3 = data.frame(USER_ID = Test3$USER_ID, Predictions = PredTestLabels3)
MySubmission2 = data.frame(USER_ID = Test2$USER_ID, Predictions = PredTestLabels2)
MySubmission1 = data.frame(USER_ID = Test1$USER_ID, Predictions = PredTestLabels1)
str(MySubmission3)
total = rbind(MySubmission1,MySubmission2,MySubmission3,MySubmission4)
str(total)
total[order(total$USER_ID),]
MySubmission11 = data.frame(USER_ID = test$USER_ID, Predictions = total$Predictions)
write.csv(MySubmission11, "Submission-11.csv", row.names=FALSE)  #0.46264

# make CART Tree
Tree1 = rpart(Party ~ . -USER_ID, data=Train1, method="class", minbucket=25)
prp(Tree1)
# make predictions on test data
PredictCART1 = predict(Tree1, newdata=Test1, type="class")   
summary(PredictCART1)
Tree2 = rpart(Party ~ . -USER_ID, data=Train2, method="class", minbucket=25)
prp(Tree2)
# make predictions on test data
PredictCART2 = predict(Tree2, newdata=Test2, type="class")   
summary(PredictCART2)
Tree3 = rpart(Party ~ . -USER_ID, data=Train3, method="class", minbucket=25)
prp(Tree3)
# make predictions on test data
PredictCART3 = predict(Tree3, newdata=Test3, type="class")   
summary(PredictCART3)
Tree4 = rpart(Party ~ . -USER_ID, data=Train4, method="class", minbucket=25)
prp(Tree4)
# make predictions on test data
PredictCART4 = predict(Tree4, newdata=Test4, type="class")   
summary(PredictCART4)
MySubmission2 = data.frame(USER_ID = Test2$USER_ID, Predictions = PredictCART2)
MySubmission1 = data.frame(USER_ID = Test1$USER_ID, Predictions = PredictCART1)
MySubmission3 = data.frame(USER_ID = Test3$USER_ID, Predictions = PredictCART3)
MySubmission4 = data.frame(USER_ID = Test4$USER_ID, Predictions = PredictCART4)


total = rbind(MySubmission1,MySubmission2, MySubmission3, MySubmission4)
total[order(total$USER_ID),]
MySubmission14 = data.frame(USER_ID = test$USER_ID, Predictions = total$Predictions)
write.csv(MySubmission14, "Submission-14.csv", row.names=FALSE)  #0.49569
# ============ 2 clusters
library(rpart)
library(rpart.plot)

km = kmeans(normTrain, centers = 2)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)

clusterTest = predict(km.kcca, newdata = normTest)
table(clusterTest)
str(clusterTrain)
summary(clusterTrain)

# Cluster-Specific Predictions
Train1 = subset(train, clusterTrain == 1)
str(Train1)
summary(Train1)
Train1$Party
Test1 = subset(test, clusterTest == 1)

Train2 = subset(train, clusterTrain == 2)
str(Train2)
summary(Train2)
Train2$Party
Test2 = subset(test, clusterTest == 2)

# logistic regression models for each cluster
Model1 = glm(Party ~ . -USER_ID, data=Train1, family=binomial)
summary(Model1)
# make predictions on the test set:
PredTest1 = predict(Model1, newdata=Test1, type="response")
summary(PredTest1)
threshold = 0.5
PredTestLabels1 = as.factor(ifelse(PredTest1<threshold, "Democrat", "Republican"))
table(PredTestLabels1)   # 

Model2 = glm(Party ~ . -USER_ID, data=Train2, family=binomial)
summary(Model2)
# make predictions on the test set:
PredTest2 = predict(Model2, newdata=Test2, type="response")
summary(PredTest2)
threshold = 0.5
PredTestLabels2 = as.factor(ifelse(PredTest2<threshold, "Democrat", "Republican"))
table(PredTestLabels2)   # 
MySubmission2 = data.frame(USER_ID = Test2$USER_ID, Predictions = PredTestLabels2)
MySubmission1 = data.frame(USER_ID = Test1$USER_ID, Predictions = PredTestLabels1)
total = rbind(MySubmission1,MySubmission2)
total[order(total$USER_ID),]
MySubmission12 = data.frame(USER_ID = test$USER_ID, Predictions = total$Predictions)
write.csv(MySubmission12, "Submission-12.csv", row.names=FALSE)  # 0.44109



# make CART Tree
Tree1 = rpart(Party ~ . -USER_ID, data=Train1, method="class", minbucket=25)
prp(Tree1)
# make predictions on test data
PredictCART1 = predict(Tree1, newdata=Test1, type="class")   
summary(PredictCART1)
Tree2 = rpart(Party ~ . -USER_ID, data=Train2, method="class", minbucket=25)
prp(Tree2)
# make predictions on test data
PredictCART2 = predict(Tree2, newdata=Test2, type="class")   
summary(PredictCART2)

MySubmission2 = data.frame(USER_ID = Test2$USER_ID, Predictions = PredictCART2)
MySubmission1 = data.frame(USER_ID = Test1$USER_ID, Predictions = PredictCART1)
total = rbind(MySubmission1,MySubmission2)
total[order(total$USER_ID),]
MySubmission13 = data.frame(USER_ID = test$USER_ID, Predictions = total$Predictions)
write.csv(MySubmission13, "Submission13.csv", row.names=FALSE)  #0.51149
