library(quantmod)
library(moments)

dataPath <- "~/R/BayanAnalytics/data/"
startDate <- as.Date("2007-01-06")

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

returns <- data.frame()

for (symbol in symbols) {
  data <- read.csv(paste(dataPath, symbol, ".csv", sep=""))
  data <- xts(data[, c("Open","High","Low","Close","Volume")], order.by=as.Date(data[, "Index"], format="%Y-%m-%d"))
  data <- window(data, start=startDate)
  
  print(paste(symbol, ": ", length(data), " rows", sep=""))
  returns <- cbind(returns, Delt(Cl(data), type="log"))
}

colnames(returns) <- symbols
returns <- returns[-1, ]

plot(returns[, "TASI"], auto.grid=FALSE, minor.ticks=FALSE, main="TASI Daily Returns", ylab="log return")
length(returns[, "TASI"])          # Number of observatons
mean(returns[, "TASI"])            # Mean returns
sd(returns[, "TASI"])              # Standard deviaiton per day
sd(returns[, "TASI"]) * sqrt(250)  # Volatility
skewness(returns[, "TASI"])        # Skewness
kurtosis(returns[, "TASI"]) - 3    # Excess kurtosis

# Calculating number of observations below the lowest bound of 99% confidence internal
0.01 * length(returns[, "TASI"])  # Number of observations below the lower bound of 99% under the assumption of normaility.
lower.bound <- - 2.33 * sd(returns[, "TASI"])
sum(returns[, "TASI"] < lower.bound) # Number of "actual" observations below the lower bound of 99%. Clearly TASI violate the assumpton of normality.

