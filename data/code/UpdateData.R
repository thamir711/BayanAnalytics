##########################################
## Update data files
## Before running this script make sure you do the following:
## 1. Download the one-year price data containing all stocks/indices from GulfBase
## 2. Place the price data file (only one file) in /gulfbase
## 3. Rename the price data file to "PriceData.csv"
## 4. Now you can run this script
##########################################
library(quantmod)

thePath <- "~/R/BayanAnalytics/data/"
theFiles <- list.files(path=thePath, pattern=".csv")
src.data <- read.csv(paste(thePath, "gulfbase/PriceData.csv", sep=""))
colnames(src.data) <- c("Symbol", "Index", "Open", "High", "Low", "Close", "Volume")

for (ii in theFiles) {
  data <- read.csv(paste(thePath, ii, sep=""))
  data <- xts(data[, c("Open","High","Low","Close","Volume")], order.by=as.Date(data[, "Index"], format="%Y-%m-%d"))
  lastHistoricalDate = index(data[nrow(data), ])
  
  symbol <- substr(ii, 1, nchar(ii) - 4)
  symbol.data <- src.data[src.data[, "Symbol"] == symbol, ]
  
  recent <- xts(symbol.data[, c("Open", "High", "Low", "Close", "Volume")], 
              order.by=as.Date(strptime(symbol.data[, "Index"], format="%Y%m%d")))
  
  pos <- match(as.Date(lastHistoricalDate, format="%Y-%m-%d"), index(recent))
  
  if (!is.na(pos)) { 
    if (pos == nrow(recent))
      print("File already up-to-date")
    
    if (pos < nrow(recent)) {
      dt <- NULL
      dt <- rbind(data, recent[(pos + 1):nrow(recent), ])
      
      write.zoo(dt, paste(thePath, ii, sep=""), sep=",", row.names=FALSE) 
    }
  }
  
  if (is.na(pos))
    print("Error: dates do not match")
}
