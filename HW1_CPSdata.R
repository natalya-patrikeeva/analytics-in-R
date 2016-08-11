# HW1 CPS Dataset - September 2013
rm(list=ls())

CPS = read.csv("CPSData.csv")
str(CPS)
summary(CPS)
sort(table(CPS$Region)) 
sort(table(CPS$State))

# What proportion of interviewees are citizens of the United States?
table(CPS$Citizenship)
prop.table(table(CPS$Citizenship))[1]+prop.table(table(CPS$Citizenship))[2]

# For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity? 
HispanicData = subset(CPS, Hispanic == 1)
table(HispanicData$Race)
as.numeric(table(HispanicData$Race) >= 250)
which(as.numeric(table(HispanicData$Race) >= 250) == 1)
table(HispanicData$Race)[which(as.numeric(table(HispanicData$Race) >= 250) == 1)]

# Evaluating Missing Values
str(CPS$Married)
# is.na(CPS$Married) returns T = N/A or F - if Married variable is present
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married)) # The Married variable being missing (T) is related to Age value
table(CPS$Citizenship, is.na(CPS$Married))

# How many states had all interviewees living in a non-metropolitan area 
# (aka they have a missing MetroAreaCode value)? For this question, 
# treat the District of Columbia as a state (even though it is not technically a state).
# TRUE means non-metropolitan area

table(CPS$State, is.na(CPS$MetroAreaCode))
which(table(subset(CPS$State, !is.na(CPS$MetroAreaCode))) == 0)
# How many states had all interviewees living in a metropolitan area? 
# Again, treat the District of Columbia as a state.
table(subset(CPS$State, is.na(CPS$MetroAreaCode == "TRUE")))
which(table(subset(CPS$State, is.na(CPS$MetroAreaCode ))) == 0)

# Which region of the United States has the largest proportion of interviewees
# living in a non-metropolitan area?

prop.table(table(CPS$Region, is.na(CPS$MetroAreaCode)), 1)

# Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?

tapply(is.na(CPS$MetroAreaCode), CPS$State, mean)
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

# Integrating Metropolitan Area Data
MetroAreaMap = read.csv("MetroAreaCodes.csv")
CountryMap = read.csv("CountryCodes.csv")
str(MetroAreaMap)
summary(MetroAreaMap)
str(CountryMap)
summary(CountryMap)

# The following command merges the two data frames on these columns, 
# overwriting the CPS data frame with the result:
# all.x=TRUE means we want to keep all rows from the "x" data frame (CPS), even if some of the rows' 
# MetroAreaCode doesn't match any codes in MetroAreaMap. In database terminology, 
# this parameter makes the operation a left outer join instead of an inner join.
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
str(CPS)
sort(table(CPS$MetroArea))

# How many interviewees have a missing value for the new metropolitan area variable? TRUE = missing value
table(is.na(CPS$MetroArea))

# Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity?
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))

# Determine the number of metropolitan areas in the United States from which 
# at least 20% of interviewees are Asian.
sort(tapply(CPS$Race=="Asian", CPS$MetroArea, mean))
which(tapply(CPS$Race=="Asian", CPS$MetroArea, mean) >= 0.20)

# Determine which metropolitan area has the smallest proportion of interviewees 
# who have received no high school diploma.

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))

# Integrating Country of Birth Data
CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
str(CPS)
# How many interviewees have a missing value for the new country of birth variable?
table(is.na(CPS$Country))
# Among all interviewees born outside of North America, which country was the most common place of birth?
sort(table(CPS$Country))
sort(table(CPS$CountryOfBirthCode))

# What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, NY-NJ-PA" 
# metropolitan area have a country of birth that is not the United States? 

tapply(CPS$Country!= "United States", CPS$MetroArea, mean, na.rm=TRUE)

# Which metropolitan area has the largest number (note -- not proportion) 
# of interviewees with a country of birth in India?
CoBIndia = subset(CPS, CPS$Country == 'India')
summary(CoBIndia)
sort(table(CoBIndia$MetroArea))

sort(tapply(CPS$Country == 'India', CPS$MetroArea, sum, na.rm=TRUE))

# Brazil
sort(table(subset(CPS$MetroArea, CPS$Country == 'Brazil')), na.rm=TRUE)

# Somalia
sort(table(subset(CPS$MetroArea, CPS$Country == 'Somalia')), na.rm=TRUE)
