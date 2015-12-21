library(quantmod)

dataPath <- "~/R/BayanAnalytics/data/"
startDate <- as.Date("2000-01-01")

symbol <- "TASI"
TASI <- read.csv(paste(dataPath, symbol, ".csv", sep=""))
TASI <- xts(TASI[, c("Open","High","Low","Close","Volume")], order.by=as.Date(TASI[, "Index"], format="%Y-%m-%d"))
TASI <- window(TASI, start=startDate)

symbol <- "TASI.BFS"
TASI.BFS <- read.csv(paste(dataPath, symbol, ".csv", sep=""))
TASI.BFS <- xts(TASI.BFS[, c("Open","High","Low","Close","Volume")], order.by=as.Date(TASI.BFS[, "Index"], format="%Y-%m-%d"))
TASI.BFS <- window(TASI.BFS, start=startDate)

TASI_returns <- Delt(Cl(TASI), type="arithmetic")
TASI_logReturns <- Delt(Cl(TASI), type="log")

TASI.BFS_returns <- Delt(Cl(TASI.BFS), type="arithmetic")
TASI.BFS_logReturns <- Delt(Cl(TASI.BFS), type="log")

plot(as.numeric(TASI_returns[-1, ]), as.numeric(TASI.BFS_returns[-1, ]))
cor(as.numeric(TASI_returns[-1, ]), as.numeric(TASI.BFS_returns[-1, ]))
