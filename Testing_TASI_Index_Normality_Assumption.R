library(quantmod)
library(moments)

dataPath <- "~/R/BayanAnalytics/data/"
startDate <- "2007-01-06"

symbols <- c("TASI",
             "TASI.BFS",
             "TASI.PCI",
             "TASI.CMT",
             "TASI.RTL",
             "TASI.EU",
             "TASI.AFI",
             "TASI.TIT",
             "TASI.INS",
             "TASI.MUI",
             "TASI.INI",
             "TASI.BDC",
             "TASI.RED",
             "TASI.TRA",
             "TASI.MAP",
             "TASI.HTT")

firsttime <- TRUE
for (symbol in symbols) {
  data <- read.csv(paste(dataPath, symbol, ".csv", sep=""))
  obs.rows <- seq(which(data[, "Index"] == startDate), nrow(data))
  
  if (firsttime) {
    returns <- data.frame(Index=factor(data[obs.rows, "Index"]))
    firsttime <- FALSE
  }
  
  print(paste(symbol, ": ", length(obs.rows), " rows", sep=""))
  returns[, symbol] <- as.numeric(Delt(Cl(data[obs.rows, ]), type="log"))
}
returns <- xts(returns[, -1], order.by=as.Date(returns[, "Index"], format="%Y-%m-%d"))
returns <- returns[-1, ]
