library(quantmod)
library(ellipse)

dataPath <- "~/R/BayanAnalytics/data/etc/"
symbols <- c("WTI",
             "TASI.BFS",
             "DFM.BAN",
             "DFM.IFS",
             "ADX.BAN",
             "ADX.IFS",
             "KSE.BAN",
             "KSE.FIS",
             "BAH.CBN",
             "MSM.BKI",
             "QE.BFS"
             )

returns <- data.frame()
for (symbol in symbols) {
  df <- read.csv(paste(dataPath, symbol, ".csv", sep=""))
  if (symbol == "WTI") {
    returns <- data.frame(WTI=as.numeric(Delt(df$Close, type="log"))[-1])
  } else {
    data <- df[-(1:(nrow(df)-393)), "Close"]
    returns[, symbol] <- as.numeric(Delt(data, type="log"))[-1]
  }
  
  print(paste(symbol, ": ", length(data), " rows", sep=""))
}
rm(df, data, dataPath, symbol)

plot(returns$KSE.BAN, returns$KSE.FIS)
cor(returns)
symnum(cor(returns))

# Plot the correlation matrix.
ctab <- cor(returns)
ctab <- round(ctab, 2)
plotcorr(ctab, numbers=TRUE, type="lower", diag=TRUE, mar=c(0.1, 0.1, 0.1, 0.1), cex.lab=0.75)
