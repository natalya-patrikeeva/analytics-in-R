# HW1 Internet Privacy Poll
rm(list=ls())

poll = read.csv("AnonymityPoll.csv")
str(poll)
summary(poll)
table(poll$Smartphone)

table(poll$Sex, poll$Region)
table(poll$Region, poll$State)

# Which of the following are states in the Midwest census region?
table(poll$Region=='Midwest', poll$State)
sort(tapply(poll$Region=='Midwest', poll$State, sum, na.rm=TRUE))

# Which was the state in the South census region with the largest number of interviewees?
table(poll$Region=='South', poll$State)
sort(tapply(poll$Region=='South', poll$State, sum, na.rm=TRUE))

# Internet and Smartphone Users

# How many interviewees reported not having used the Internet and not having used a smartphone?
table(poll$Internet.Use, poll$Smartphone)

table(poll$Internet.Use == 0 & poll$Smartphone == 0)

# How many interviewees reported having used the Internet and having used a smartphone?
table(poll$Internet.Use == 1 & poll$Smartphone == 1)

# How many interviewees reported having used the Internet but not having used a smartphone?
table(poll$Internet.Use == 1 & poll$Smartphone == 0)

# How many interviewees reported having used a smartphone but not having used the Internet?
table(poll$Internet.Use == 0 & poll$Smartphone == 1)

# How many interviewees have a missing value for their Internet use?

summary(poll$Internet.Use)

# How many interviewees have a missing value for their smartphone use?
summary(poll$Smartphone)

# Create a subset of data  limited to interviewees who reported Internet use or who reported smartphone use.

limited = subset(poll, poll$Internet.Use == 1 | poll$Smartphone == 1)
str(limited)
summary(limited)

#  Summarizing Opinions about Internet Privacy

# What is the average number of pieces of personal information on the Internet, 
# according to the Info.On.Internet variable?

mean(limited$Info.On.Internet)

# How many interviewees reported a value of 0 for Info.On.Internet?

table(limited$Info.On.Internet)

# What proportion of interviewees who answered the Worry.About.Info question 
# worry about how much information is available about them on the Internet? 
# 1 = worry and 0 = not worry
table(limited$Worry.About.Info)
mean(limited$Worry.About.Info, na.rm = TRUE)

# What proportion of interviewees who answered the Anonymity.Possible question 
# think it is possible to be completely anonymous on the Internet?
# 1 = possible, 0 = not possible

mean(limited$Anonymity.Possible, na.rm=TRUE)

# What proportion of interviewees who answered the Tried.Masking.Identity 
# question have tried masking their identity on the Internet?
# 1 = has tried, 0 = has not tried

mean(limited$Tried.Masking.Identity, na.rm = TRUE)

# What proportion of interviewees who answered the Privacy.Laws.Effective 
# question find United States privacy laws effective?
# 1 = US law does, 0 = it doesn't
mean(limited$Privacy.Laws.Effective, na.rm = TRUE)

# Relating Demographics to Polling Results

hist(limited$Age, xlab='Age')

plot(limited$Age, limited$Info.On.Internet, xlab='Age', ylab='Info on Internet')

# What is the largest number of interviewees that have exactly the same value in their 
# Age variable AND the same value in their Info.On.Internet variable? 

sort(table(limited$Age, limited$Info.On.Internet))
max(table(limited$Age, limited$Info.On.Internet))

jitter(c(1, 2, 3))

# jitter adds or subtracts a small amount of random noise to the values passed to it,
# and two runs will yield different results
plot(jitter(limited$Age), jitter(limited$Info.On.Internet), xlab='Age', ylab='Info on Internet')

# Use the tapply() function to obtain the summary of the Info.On.Internet value, 
# broken down by whether an interviewee is a smartphone user.

tapply(limited$Info.On.Internet, limited$Smartphone, summary, na.rm=TRUE)

# Similarly, break down the Tried.Masking.Identity variable 
# for smartphone and non-smartphone users.
tapply(limited$Tried.Masking.Identity, limited$Smartphone, summary, na.rm=TRUE)
