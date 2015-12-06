library(quantmod)
library(PerformanceAnalytics)

rm(list=ls())

nameOfStrategy <- "TASI Moving Average Strategy"

# Load the data
TASI <- as.xts(read.zoo("TASINDX_AH_20151203.csv", format="%Y%m%d", header=T, sep=",",
                        colClasses=c("NULL", "character", "numeric", "numeric", "numeric", "numeric", "integer")))
colnames(TASI) <- c("Open", "High", "Low", "Close", "Volume")

# Specify dates for data
trainingStartDate <- as.Date("2000-01-01")
trainingEndDate <- as.Date("2010-01-01")
outOfSampleStartDate <- as.Date("2010-01-02")
trainingData <- window(TASI, start=trainingStartDate, end=trainingEndDate)
testData <- window(TASI, start=outOfSampleStartDate)

# This is where we define the trading strategy
# Check moving averages at the end of the day and use as the buy signal
# Enter a trade when MA50 > MA200 and exit when MA50 < MA200
returns <- Delt(Cl(trainingData))
mavga <- SMA(Cl(trainingData), n=50)
mavgb <- SMA(Cl(trainingData), n=200)
signal <- mavga / mavgb
signal <- apply(signal, 1, function(x) { if (is.na(x)) { return(0) } else { if (x > 1) { return(1) } else { return(0) } } })
returns <- returns[-1, ]
signal <- signal[-1]
tradingReturns <- signal * returns
colnames(tradingReturns) <- "Running Strategy: MAVGa50.b200"

# Calculate performance metrics
pMetric <- matrix(c(colSums(tradingReturns), SharpeRatio.annualized(tradingReturns), maxDrawdown(tradingReturns)), nrow=1, ncol=3)
colnames(pMetric) <- c("Profit", "SharpeRatio", "MaxDrawDown")
pMetric

charts.PerformanceSummary(tradingReturns, main=paste(nameOfStrategy, "- Training"), geometric=F)

# TO DO: adjust the back-tested (observed) performance by the expected return of a rule
# with no predictive power havign an equivalent position bias.
# The benchmark for any rule can be defined as the expected return of a nonpredictive
# rule with an equivalent position bias.

# Calculating the Expected Return of a rule with no predictive power 
# having an equivalent position bias as the MA50-MA200-Cross rule.
# Using the formula: ER = [p(L) * ADC] - [p(S) * ADC]
# ADC: Average Daily Change
# p(L): proportion long of the rule; i.e. MA50-MA200-Cross
# p(S): proportion short of the rule; i.e. MA50-MA200-Cross
adc <- mean(returns)
pS <- table(signal)[1]
pL <- table(signal)[2]
exp.return <- (pL * adc) - (pS * adc)

# Adjusting the back-tested (observed) performance by the expected return of rule
# with no predictive power having an equivalent position bias.
pMetric[1] - exp.return

