# HW 3 Popularity of music records
songs = read.csv("songs.csv")
str(songs)
summary(songs)
str(subset(songs, year == 2010))
MichJack = subset(songs, artistname == "Michael Jackson")
str(MichJack)
MichJack$songtitle
which(MichJack$Top10 == 1)

summary(songs$timesignature)
str(subset(songs, timesignature == 0))
hist(songs$timesignature)

which.max(songs$tempo)
songs$songtitle[6206]

SongsTrain = subset(songs, year < 2010)
str(SongsTrain)

SongsTest = subset(songs, year > 2009)
str(SongsTest)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

cor(SongsTrain$loudness, SongsTrain$energy)

SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

# Make predictions on test data
predictTest = predict(SongsLog3, type="response", newdata=SongsTest)

# Confusion matrix
table(SongsTest$Top10, predictTest > 0.45)

# accuracy
(309+19)/(309+19+5+40)

# What would the accuracy of the baseline model be on the test set?
# pick the most frequent outcome (a song is not a Top 10 hit) for all songs.

(309+5)/(309+19+5+40)

# Confusion matrix
#                     | predicted = 0    |  predicted = 1 
#                   _______________________________________________
# Actual = 0  |   True negatives (TN)  |  False positives (FP)
# Actual = 1  |  False negatives (FN)  |   True positives (TP)

# Sensitivity = TP/(TP + FN)
19/(40+19)

# specificity = TN/(TN + FP)
309/(309+5)

