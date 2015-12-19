##########################################
## Daily prices from GulfBase
## WARNING: To download symbols for the first time (i.e. not existing in /gulfbase),
##          make sure that you include ONLY the new symbols in ListOfInstruments.R file.
##          Otherwise, you'll overwrite the up-to-date symbol files in /data.
##          After downloading the new symbols, update the ListOfInstruments.R file 
##          to include all the symbols you want to update using UpdateData.R file.
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
