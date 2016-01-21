library(quantmod)
dataPath <- "~/R/BayanAnalytics/data/etc/"

symbols <- c("TASI",
             "DFM",
             "ADX",
             "KSE",
             "BSE",
             "MSM",
             "QE")

returns <- list()
for (symbol in symbols) {
  data <- read.csv(paste(dataPath, symbol, ".csv", sep=""))
  print(paste(symbol, ": ", nrow(data), " rows", sep=""))
  returns[[symbol]] <- as.numeric(Delt(Cl(data), type="log"))[-1]
}
rm(data, dataPath, symbol)

## QQ plot of TASI vs other GCC indices distributions
## The two samples do not have the same size, so one must compute the same sets of 
## sample quantiles for each and plots them. 

for (symbol in symbols[2:length(symbols)]) {
  n <- ifelse(length(returns[[symbol]]) < length(returns$TASI), 
              length(returns[[symbol]]), length(returns$TASI))
  q <- (1:n)/(n+1)  ## computing sets of sample quantiles
  
  qqplot(quantile(returns$TASI, probs=q), quantile(returns[[symbol]], probs=q), xlab="TASI return", ylab=paste(symbol, "return"))
  lmfit <- lm(quantile(returns[[symbol]], probs=c(.25, .75)) ~ quantile(returns$TASI, c(.25, .75)))
  abline(lmfit)
}
