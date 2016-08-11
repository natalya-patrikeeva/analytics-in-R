# HW1 Chicago crime data
# Read the csv file
mvt = read.csv("mvtWeek1.csv")
# Structure of the dataset
str(mvt)
# Statistical summary
summary(mvt)
nrow(mvt)

# Understanding dates in R
mvt$Date[1]

# convert date to "yyyy-mm-dd" format
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))

summary(DateConvert)
DateConvert[1]
mvt$Date[1]

# Add month and weeday variables
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)

mvt$Date = DateConvert
str(mvt)
summary(mvt)

# Using the table command, answer the following questions.
# In which month did the fewest motor vehicle thefts occur?
table(mvt$Month)
which.min(table(mvt$Month))

# On which weekday did the most motor vehicle thefts occur?
table(mvt$Weekday)
which.max(table(mvt$Weekday))

# Each observation in the dataset represents a motor vehicle theft, 
# and the Arrest variable indicates whether an arrest was later made for this theft. 
# Which month has the largest number of motor vehicle thefts for which an arrest was made?
tapply(mvt$Arrest, mvt$Month, sum)
which.max(tapply(mvt$Arrest, mvt$Month, sum))

# Visualizing Crime Trends
# histogram of the variable Date
hist(mvt$Date, breaks=100, xlab="Year", ylab="Crime", main="Crime rate over time")

# Analyze how arrests have changed over time
# Create a boxplot of the variable "Date", sorted by the variable "Arrest"
boxplot(mvt$Date ~ mvt$Arrest, xlab="False=no Arrest, True=Arrest")

# For what proportion of motor vehicle thefts in 2001 was an arrest made?
table(mvt$Year, mvt$Arrest)

# table(mvt$Year, mvt$Arrest)[1,2] is a number of arrests made in 2001 = TRUE
table(mvt$Year, mvt$Arrest)[1,2]

# Total number of arrest in 2001 = TOTAL
table(mvt$Year, mvt$Arrest)[1,1] + table(mvt$Year, mvt$Arrest)[1,2]

# Proportion of motor vehicle theft in 2001: TRUE/TOTAL
table(mvt$Year, mvt$Arrest)[1,2] / (table(mvt$Year, mvt$Arrest)[1,1] + table(mvt$Year, mvt$Arrest)[1,2])

# For what proportion of motor vehicle thefts in 2007 was an arrest made?
table(mvt$Year, mvt$Arrest)[7,2] / (table(mvt$Year, mvt$Arrest)[7,1] + table(mvt$Year, mvt$Arrest)[7,2])

# For what proportion of motor vehicle thefts in 2012 was an arrest made?
table(mvt$Year, mvt$Arrest)[12,2] / (table(mvt$Year, mvt$Arrest)[12,1] + table(mvt$Year, mvt$Arrest)[12,2])

# Popular Locations
# We want to find the top five locations where motor vehicle thefts occur.
# Street, Parking Lot, Alley, Gas Station, Driveway
sort(table(mvt$LocationDescription))

# Create a subset of data with observations for which the theft happened in one of these 5 locations
Top5 = subset(mvt, LocationDescription == 'STREET' | LocationDescription == 'PARKING LOT/GARAGE(NON.RESID.)' | LocationDescription == 'ALLEY' | LocationDescription == 'GAS STATION' | LocationDescription == 'DRIVEWAY - RESIDENTIAL')
nrow(Top5)
# Running the following command generates a lot of output with 0
table(Top5$LocationDescription)
# Make tables nicer to read, run:
Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$LocationDescription)
str(Top5)

# One of the locations has a much higher arrest rate than the other locations. Which is it?
# Since arrest data type is TRUE and FALSE then you can use mean() and tapply() to calculate the proportion.
tapply(Top5$Arrest,Top5$LocationDescription, mean)
which.max(tapply(Top5$Arrest,Top5$LocationDescription, mean))

# To calculate the proportion of  False (no arrest)
tapply(!Top5$Arrest,Top5$LocationDescription, mean)

# Alternatively, use:
prop.table(table(Top5$Arrest,Top5$LocationDescription),2) # makes the column add to 1.

# On which day of the week do the most motor vehicle thefts at gas stations happen?
GasStation = subset(Top5, LocationDescription == 'GAS STATION')
table(GasStation$Weekday)
which.max(table(GasStation$Weekday))
sum(table(GasStation$Weekday))

# On which day of the week do the fewest motor vehicle thefts in residential driveways happen?
DrivewayResidential = subset(Top5, LocationDescription == 'DRIVEWAY - RESIDENTIAL')
str(DrivewayResidential)
table(DrivewayResidential$Weekday)
which.min(table(DrivewayResidential$Weekday))
