library(quantmod)
library(PerformanceAnalytics)

rm(list=ls())

# Load the data
TASI <- as.xts(read.zoo("TASINDX_AH_20151203.csv", format="%Y%m%d", header=T, sep=",",
                        colClasses=c("NULL", "character", "numeric", "numeric", "numeric", "numeric", "integer")))
colnames(TASI) <- c("Open", "High", "Low", "Close", "Volume")

# Specify dates for data
trainingStartDate <- as.Date("2000-01-01")
trainingEndDate <- as.Date("2010-01-01")
trainingData <- window(TASI, start=trainingStartDate, end=trainingEndDate)

returns <- Delt(Op(trainingData), type="log")
mavga <- SMA(Cl(trainingData), n=50)
mavgb <- SMA(Cl(trainingData), n=200)
signal <- mavga / mavgb
signal <- apply(signal, 1, function(x) { if (is.na(x)) { return(0) } else { if (x > 1) { return(1) } else { return(0) } } })
returns <- as.xts(Next(returns, k=2))
tradingReturns <- signal * (returns - mean(returns, na.rm=T))
colnames(tradingReturns) <- "Running Strategy: MAVGa50.b200"
tradingReturns <- na.omit(tradingReturns)

# Calculate performance metrics
pMetric <- matrix(c(colSums(tradingReturns), SharpeRatio.annualized(tradingReturns), maxDrawdown(tradingReturns)), nrow=1, ncol=3)
colnames(pMetric) <- c("Profit", "SharpeRatio", "MaxDrawDown")
pMetric

