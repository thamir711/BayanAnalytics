##########################################
## Daily prices from GulfBase
##########################################
library(quantmod)

thePath <- "~/R/BayanAnalytics/data/"
source(paste(thePath, "code/ListOfInstruments.R", sep=""))

for (ii in theInstruments) {
  print(ii)
  
  src.data <- read.csv(paste(thePath, "gulfbase/", ii, ".csv", sep=""))
  colnames(src.data) <- c("Symbol", "Index", "Open", "High", "Low", "Close", "Volume")
  data <- xts(src.data[, c("Open", "High", "Low", "Close", "Volume")], 
              order.by=as.Date(strptime(src.data[, "Index"], format="%Y%m%d")))
  
  write.zoo(data, paste(thePath, ii, ".csv", sep=""), sep=",", row.names=FALSE)
}
