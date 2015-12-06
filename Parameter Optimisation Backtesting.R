library("quantmod")
library("PerformanceAnalytics")

rm(list=ls())

nameOfStrategy <- "TASI Moving Average Strategy"

# Specify dates for downloading data, training models and running simulation
trainingStartDate <- as.Date("2000-01-01")
trainingEndDate <- as.Date("2010-01-01")
outOfSampleStartDate <- as.Date("2010-01-02")

# Load the data
TASI <- as.xts(read.zoo("TASINDX_AH_20151203.csv", format="%Y%m%d", header=T, sep=",",
                        colClasses=c("NULL", "character", "numeric", "numeric", "numeric", "numeric", "integer")))
colnames(TASI) <- c("Open", "High", "Low", "Close", "Volume")

trainingData <- window(TASI, start=trainingStartDate, end=trainingEndDate)
testData <- window(TASI, start=outOfSampleStartDate)
indexReturns <- Delt(Cl(window(TASI, start=outOfSampleStartDate)))
colnames(indexReturns) <- "TASI Buy&Hold"

TradingStrategy <- function(mktdata, mavga_period, mavgb_period)
{
  # This is where we define the trading strategy
  # Check moving averages at the end of the day and use as the buy signal
  # Enter a trade when MAVGa > MAVGb and exit when MAVGa < MAVGb
  
  # Lets print the name of whats running
  runName <- paste("MAVGa", mavga_period, ".b", mavgb_period, sep="")
  print(paste("Running Strategy: ", runName))
  
  # Calculate the Open Close return
  returns <- Delt(Cl(mktdata))
  
  # Calculate the moving averages
  mavga <- SMA(Cl(mktdata), n=mavga_period)
  mavgb <- SMA(Cl(mktdata), n=mavgb_period)
  
  signal <- mavga / mavgb
  # If mavga > mavgb go long
  # If mavga < mavgb exit
  signal <- apply(signal, 1, function(x) { if (is.na(x)) { return(0) } else { if (x > 1) { return(1) } else { return(0) } } })
  
  # Remove the NA in the first period
  returns <- returns[-1, ]
  signal <- signal[-1]
  
  tradingreturns <- signal * returns
  colnames(tradingreturns) <- runName
  
  return (tradingreturns)
}

RunIterativeStrategy <- function(mktdata)
{
  # This function will run the TradingStrategy
  # It will iterate over a given set of input variables
  # In this case we try lots of different periods for the moving average
  
  firstRun <- TRUE
  
  for (a in seq(30, 70, 10)) {
    for (b in seq(180, 220, 10)) {
      
      runResult <- TradingStrategy(mktdata, a, b)
      
      if (firstRun) {
        
        firstRun <- FALSE
        results <- runResult
        
      } else {
        
        results <- cbind(results, runResult)
        
      }
    }
  }
  
  return(results)
}

CalculatePerformanceMetric <- function(returns, metric)
{
  # Get given some returns in columns
  # Apply the function metric to the data
  
  print(paste("Calculating Performance Metric:", metric))
  
  metricFunction <- match.fun(metric)
  metricData <- as.matrix(metricFunction(returns))
  
  # Some functions return the data the wrong way round
  # Hence cant label columns to need to check and transpose it
  if (nrow(metricData) == 1) {
    
    metricData <- t(metricData)
    
  }
  
  colnames(metricData) <- metric
  
  return (metricData)
}

PerformanceTable <- function(returns)
{
  pMetric <- CalculatePerformanceMetric(returns, "colSums")
  pMetric <- cbind(pMetric,CalculatePerformanceMetric(returns, "SharpeRatio.annualized"))
  pMetric <- cbind(pMetric,CalculatePerformanceMetric(returns, "maxDrawdown"))
  colnames(pMetric) <- c("Profit", "SharpeRatio", "MaxDrawDown")
  
  print("Performance Table")
  print(pMetric)
  
  return (pMetric)
}

OrderPerformanceTable <- function(performanceTable, metric)
{
  return (performanceTable[order(performanceTable[, metric], decreasing=TRUE), ])
}

SelectTopNStrategies <- function(returns, performanceTable, metric, n)
{
  # Metric is the name of the function to apply to the column to select the Top N
  # n is the number of strategies to select
  pTab <- OrderPerformanceTable(performanceTable, metric)
  
  if (n > ncol(returns)) {
    n <- ncol(returns)
  }
  
  strategyNames <- rownames(pTab)[1:n]
  topNMetrics <- returns[, strategyNames]
  return (topNMetrics)
}

FindOptimumStrategy <- function(trainingData)
{
  # Optimise the strategy
  trainingReturns <- RunIterativeStrategy(trainingData)
  pTab <- PerformanceTable(trainingReturns)
  toptrainingReturns <- SelectTopNStrategies(trainingReturns, pTab, "SharpeRatio", 5)
  charts.PerformanceSummary(toptrainingReturns, main=paste(nameOfStrategy, "- Training"), geometric=FALSE)
  
  return (pTab)
}

# pTab is the performance table of the various parameters tested
pTab <- FindOptimumStrategy(trainingData)

# Test out of sample
dev.new()
# Manually specify the parameter that we want to trade here, just because a strategy is at the top of
# pTab it might not be good (maybe due to overfit)
outOfSampleReturns <- TradingStrategy(testData, mavga_period=50, mavgb_period=200)
finalReturns <- cbind(outOfSampleReturns, indexReturns)
charts.PerformanceSummary(finalReturns, main=paste(nameOfStrategy, "- Out of Sample"), geometric=FALSE)
