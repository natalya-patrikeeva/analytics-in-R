rm(list=ls())
train = read.csv("train2016.csv", stringsAsFactors = TRUE)
test = read.csv("test2016.csv")
str(train)
str(test)
table(train$Party)
table(train$Income)
levels(train$Income)

limitedtrain = train
limitedtest = test
droplevels(limitedtrain$Income)

TrainNumeric = lapply(train, as.numeric)
table(TrainNumeric$Income)
table(test$Income)
mean(TrainNumeric$Income)

TestNumeric = lapply(test, as.numeric)
table(TestNumeric$Income)
summary(TrainNumeric)
str(TrainNumeric)
TrainNumeric = as.data.frame(TrainNumeric)
TrainNumeric$Party = NULL
TestNumeric = as.data.frame(TestNumeric)

str(TrainNumeric)
str(TestNumeric)
distances = dist(TrainNumeric, method="euclidean")
clusterHier = hclust(distances, method = "ward.D")
plot(clusterHier)
rect.hclust(clusterHier, k=4, border = "red")
HierClusters = cutree(clusterHier, k = 4)
table(HierClusters)

levels(limitedtrain$Income) = c("", "$100,001 - $150,000", "$25,001 - $50,000", "$50,000 - $74,999", "$75,000 - $100,000", "over $150,000", "under $25,000")
limitedtrain$Income = factor(limitedtrain$Income, levels = c("NA", "$100,001 - $150,000", "$25,001 - $50,000", "$50,000 - $74,999", "$75,000 - $100,000", "over $150,000", "under $25,000"))
limitedtest$Income = factor(limitedtest$Income, levels = c("NA", "$100,001 - $150,000", "$25,001 - $50,000", "$50,000 - $74,999", "$75,000 - $100,000", "over $150,000", "under $25,000"))
table(limitedtrain$Income)
sum(is.na(limitedtrain$Income)) #1028
str(limitedtrain)

table(limitedtrain$HouseholdStatus)
levels(limitedtrain$HouseholdStatus) = c("", "Domestic Partners (no kids)", "Domestic Partners (w/kids)", "Married (no kids)", "Married (w/kids)", "Single (no kids)", "Single (w/kids)")
limitedtrain$HouseholdStatus = factor(limitedtrain$HouseholdStatus, levels = c("NA", "Domestic Partners (no kids)", "Domestic Partners (w/kids)", "Married (no kids)", "Married (w/kids)", "Single (no kids)", "Single (w/kids)"))
limitedtest$HouseholdStatus = factor(limitedtest$HouseholdStatus, levels = c("NA", "Domestic Partners (no kids)", "Domestic Partners (w/kids)", "Married (no kids)", "Married (w/kids)", "Single (no kids)", "Single (w/kids)"))

sum(is.na(limitedtrain$HouseholdStatus)) # 450
str(limitedtrain)

table(limitedtrain$Gender)
levels(limitedtrain$Gender) = c("", "Female","Male")
limitedtrain$Gender = factor(limitedtrain$Gender, levels = c("NA","Female","Male"))
limitedtest$Gender = factor(limitedtest$Gender, levels = c("NA","Female","Male"))
table(limitedtrain$Gender)
sum(is.na(limitedtrain$Gender)) # 113
str(limitedtrain)
limitedtrain$Gender


table(limitedtrain$EducationLevel)
levels(limitedtrain$EducationLevel) = c("", "Associate's Degree", "Bachelor's Degree", "Current K-12", "Current Undergraduate", "Doctoral Degree", "High School Diploma","Master's Degree")
limitedtrain$EducationLevel = factor(limitedtrain$EducationLevel, levels = c("NA", "Associate's Degree", "Bachelor's Degree", "Current K-12", "Current Undergraduate", "Doctoral Degree", "High School Diploma","Master's Degree"))
limitedtest$EducationLevel = factor(limitedtest$EducationLevel,  levels = c("NA", "Associate's Degree", "Bachelor's Degree", "Current K-12", "Current Undergraduate", "Doctoral Degree", "High School Diploma","Master's Degree"))

table(limitedtrain$EducationLevel)
sum(is.na(limitedtrain$EducationLevel)) # 866
str(limitedtrain)

table(limitedtrain$YOB)

table(limitedtrain$Q124742)

# create a simple logistic regression model, to predict Party using all other variables in the dataset, except for the user ID:
SimpleMod2 = glm(Party ~ . -USER_ID, data=limitedtrain, family=binomial)
summary(SimpleMod2)
# make predictions on the test set:
PredTest = predict(SimpleMod2, newdata=limitedtest, type="response")
summary(PredTest)
threshold = 0.5
PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))
table(PredTestLabels)   # 0.43247
# submit the file on Kaggle to see how well the model performs. You can make up to 5 submissions per day. 
# Let's prepare a submission file for Kaggle (for more about this, see the "Evaluation" page on the competition site):
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)
write.csv(MySubmission, "Submission-7.csv", row.names=FALSE)
# impute.mean <- function(x) replace(x, is.na(x) | is.nan(x) | is.infinite(x), mean(x[!is.na(x) & !is.nan(x) & !is.infinite(x)]))
# losses <- apply(losses, 2, impute.mean)
cor(as.numeric(train$Party), as.numeric(train$Q109244))
cor(as.numeric(train$Party), as.numeric(train$Q101596))


