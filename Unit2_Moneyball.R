# Moneyball in baseball
baseball = read.csv("baseball.csv")
str(baseball)
summary(baseball)

moneyball = subset(baseball, Year < 2002)
str(moneyball)

# plot number of wins vs team color by playoffs?

moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)
plot(moneyball$RD, moneyball$W, xlab = "Run Difference = Runs scored - Runs allowed", ylab = "Wins")
WinsReg = lm(W ~ RD, data = moneyball)
summary(WinsReg)
# W = 80.8814 + 0.1058*RD
# W >= 95 
# RD >= (95 - 80.8814)/0.1058 = 133.4

RunsReg = lm(RS ~ OBP + SLG + BA, data = moneyball)
summary(RunsReg)
RunsReg = lm(RS ~ OBP + SLG, data = moneyball)
summary(RunsReg)
# Runs = -804.63 + 2737.77*0.311 + 1584.91*0.405 = 688.705
# Choosing what players to pick
# EC = -804.63 + 2737.77*0.338 + 1584.91*0.540 = 976.5877
# JG = 979.0476  $1,065,000
# FM = 798.3635
# GM = 760.7468
# CP = 976.16    $300,000

# Allowing Runs
RunsAllowed = lm(RA ~ OOBP + OSLG, data = moneyball)
summary(RunsAllowed)
# Runs Allowed = -837.38 + 2913.60*0.297 + 1514.29*0.370 = 588.2465

moneyball_01 = subset(baseball,Team = "OAK" & Year == 2001)
str(moneyball_01)
summary(moneyball_01)
# Linear regression model for Runs Scored
OA2002 = lm(RS ~ OBP + SLG, data=moneyball_01)
summary(OA2002)
# Linear regression model for Runs Allowed
RA_2002 = lm(RA ~ OOBP + OSLG, data = moneyball_01)
summary(RA_2002)

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)

# What is the correlation between teamRank and wins2012?
cor(teamRank,wins2012)
cor(teamRank,wins2013)
plot(teamRank,wins2012,xlab="Team Rank (1=won the World Series)", ylab="Wins in 2012")
plot(teamRank,wins2013,xlab="Team Rank (1=won the World Series)", ylab="Wins in 2013")
