# HW1 Stock Dynamics
# Read the csv files
getwd()
IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
Boeing = read.csv("BoeingStock.csv")
str(IBM)
summary(IBM)
str(GE)
summary(GE)
str(ProcterGamble)
summary(ProcterGamble)
str(CocaCola)
summary(CocaCola)
str(Boeing)
summary(Boeing)

# Convert the dates from factor m/d/yy into a "Date" object yyyy-mm-dd
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
IBM$Date
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")
str(IBM)

# What is the earliest year in our datasets? 
# 1970 - from str()

# What is the standard deviation of the stock price of Procter & Gamble over this time period?
sd(ProcterGamble$StockPrice)

# Visualizing Stock Dynamics
plot(CocaCola$Date, CocaCola$StockPrice, xlab = "Year", ylab = "Price ($)", main="Price of CocaCola Stock over time", type="l", col="red" )
# Add a line to the same plot, lty=2 makes a dashed line
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col='blue')
# Draw a vertical line at a certain date
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1983-01-01")), lwd=2)

# Visualizing Stock Dynamics 1995-2005
# See color options with colors()
# lty=2 for dashed line, lty=3 dotted, lty=4 dashes-dots, lty=5 long-dashed
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type='l', col='red', ylim=c(0,210), xlab = "Year", ylab = "Price ($)", main="Price of CocaCola Stock 1995-2005")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col='sienna')
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col='purple3')
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col='turquoise2')
lines(GE$Date[301:432], GE$StockPrice[301:432], col='forestgreen')

# Comparing September 1997 to November 1997, which companies saw a decreasing trend in their stock price? 
which(GE$Date=="1997-09-01") # gives index 333
which(GE$Date=="1997-11-01") # gives index 335
plot(CocaCola$Date[333:335], CocaCola$StockPrice[333:335], type='l', col='red', ylim=c(0,210), xlab = "Year", ylab = "Price ($)", main="Price of CocaCola Stock 1995-2005")
lines(IBM$Date[333:335], IBM$StockPrice[333:335], col='sienna')
lines(Boeing$Date[333:335], Boeing$StockPrice[333:335], col='purple3')
lines(ProcterGamble$Date[333:335], ProcterGamble$StockPrice[333:335], col='turquoise2')
lines(GE$Date[333:335], GE$StockPrice[333:335], col='forestgreen')

# In the last two years of this time period (2004 and 2005) which stock seems 
# to be performing the best, in terms of increasing stock price?
which(GE$Date=="2004-01-01") # gives index 409
which(GE$Date=="2005-12-01") # gives index 432

plot(CocaCola$Date[409:432], CocaCola$StockPrice[409:432], type='l', col='red', ylim=c(0,210), xlab = "Year", ylab = "Price ($)", main="Price of CocaCola Stock 1995-2005")
lines(IBM$Date[409:432], IBM$StockPrice[409:432], col='sienna')
lines(Boeing$Date[409:432], Boeing$StockPrice[409:432], col='purple3')
lines(ProcterGamble$Date[409:432], ProcterGamble$StockPrice[409:432], col='turquoise2')
lines(GE$Date[409:432], GE$StockPrice[409:432], col='forestgreen')

# Monthly Trends
# Lastly, let's see if stocks tend to be higher or lower during certain months. 
# Calculate the mean stock price of IBM, sorted by months.
tapply(IBM$StockPrice, months(IBM$Date), mean)
AveStockIBM = tapply(IBM$StockPrice, months(IBM$Date), mean)
HighStockMonths = as.numeric(AveStockIBM > mean(IBM$StockPrice, na.rm=TRUE))
mean(IBM$StockPrice)
which(HighStockMonths==1)
AveStockIBM[which(HighStockMonths==1)]

# Repeat for GE
tapply(GE$StockPrice, months(GE$Date), mean)
AveStockGE = tapply(GE$StockPrice, months(GE$Date), mean)
HighStockMonthsGE = as.numeric(AveStockGE > mean(GE$StockPrice, na.rm=TRUE))
mean(GE$StockPrice)
which(HighStockMonthsGE==1)
AveStockGE[which(HighStockMonthsGE==1)]

# CocaCola
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
AveStockCC = tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
HighStockMonthsCC = as.numeric(AveStockCC > mean(CocaCola$StockPrice, na.rm=TRUE))
mean(CocaCola$StockPrice)
which(HighStockMonthsCC==1)
AveStockCC[which(HighStockMonthsCC==1)]

which.max(AveStockCC)
which.max(AveStockGE)

# Boeing
tapply(Boeing$StockPrice, months(Boeing$Date), mean)
AveStockBoeing = tapply(Boeing$StockPrice, months(Boeing$Date), mean)
HighStockMonthsBoeing = as.numeric(AveStockBoeing > mean(Boeing$StockPrice, na.rm=TRUE))
mean(Boeing$StockPrice)
which(HighStockMonthsBoeing==1)
AveStockBoeing[which(HighStockMonthsBoeing==1)]

# ProcterGamble
tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean)
