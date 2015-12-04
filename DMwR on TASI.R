library(quantmod)

tasi <- read.table("TASINDX_AH_20151001.csv", header=T, sep=",")
colnames(tasi) <- c("ticker", "date", "open", "high", "low", "close", "vol")
close <- rev(tasi$close)
ma50 <- runmean(close, 50, endrule="NA")
ma200 <- runmean(close, 200, endrule="NA")
tasi <- data.frame(close, ma50, ma200)
rm(close, ma50, ma200)
col <- c("black", "red", "green", "blue", "magenta", "cyan")
plot(tasi$close, col=col[1], type="l")
lines(tasi$ma50, col=col[2])
lines(tasi$ma200, col=col[3])
tasi$trend <- factor(ifelse(tasi$ma50 > tasi$ma200, "MA50>MA200", "MA50<MA200"))
tasi$return <- c(NA, diff(log(tasi$close)))
histogram(~return | trend, data=tasi)

# Reading "zoo" series from text files (i.e. GulfBase.com price data) and converting
# to "xts" series
TASI <- as.xts(read.zoo("TASINDX_AH_20151001.csv", format="%Y%m%d", 
                        tz="UTC+3", header=T, sep=",", 
                        colClasses=c("NULL", "character", "numeric", "numeric", "numeric", "numeric", "integer")))

# Better not to use the parameter "tz" because if used then xts object will be indexed
# using POSIXct,POSIXt types which causes problems with quantmod's function specifyModel.
# It seems specifyModel works only with xts objects indexed using Date type.
TASI <- as.xts(read.zoo("TASINDX_AH_20151001.csv", format="%Y%m%d", 
                        header=T, sep=",", 
                        colClasses=c("NULL", "character", "numeric", "numeric", "numeric", "numeric", "integer")))

colnames(TASI) <- c("Open", "High", "Low", "Close", "Volume")
str(TASI)
head(TASI)
tail(TASI)

# Creating financial charts
chartSeries(TASI, type="candlesticks", subset="last 6 months", 
            name="TASI Last 6 Months 2015", theme="white", TA=NULL, dn.col="red")

# DMwR "What to Predict?"
T.ind <- function(quotes, tgt.margin = 0.025, n.days = 10) {
  v <- apply(HLC(quotes), 1, mean)
  r <- matrix(NA, ncol = n.days, nrow = NROW(quotes))
  for (x in 1:n.days) r[, x] <- Next(Delt(v, k = x), x)
  x <- apply(r, 1, function(x) sum(x[x > tgt.margin | x < -tgt.margin]))
  if (is.xts(quotes))
    xts(x, time(quotes))
  else x
}

avgPrice <- function(p) apply(HLC(p), 1, mean)
addAvgPrice <- newTA(FUN = avgPrice, col = 1, legend = "AvgPrice")
addT.ind <- newTA(FUN = T.ind, col = "red", legend = "tgtRet")
addAvgPrice(on = 1)
addT.ind()
addVo()
addBBands()
addSMA(50, col="red")
addSMA(200, col="green")
listTA()

# Initial set of predictors; applicable to TASI
myATR <- function(x) ATR(HLC(x))[, "atr"]
mySMI <- function(x) SMI(HLC(x))[, "SMI"]
myADX <- function(x) ADX(HLC(x))[, "ADX"]
myAroon <- function(x) aroon(x[, c("High", "Low")])$oscillator
myBB <- function(x) BBands(HLC(x))[, "pctB"]
myCLV <- function(x) EMA(CLV(HLC(x)))[, 1]
myMACD <- function(x) MACD(Cl(x))[, 2]
mySAR <- function(x) SAR(x[, c("High", "Close")])[, 1]
myVolat <- function(x) volatility(OHLC(x), calc = "garman")[, 1]

# Predictors not applicable to TASI
myChaikinVol <- function(x) Delt(chaikinVolatility(x[, c("High", "Low")]))[, 1]
# EMV is not applicable where volume is 0
myEMV <- function(x) EMV(x[, c("High", "Low")], x[, "Volume"])[, 2]
# MFI is not applicable where volume is 0
myMFI <- function(x) MFI(x[, c("High", "Low", "Close")], x[, "Volume"])

# Using randomForest
library(randomForest)
tasi.data.model <- specifyModel(T.ind(TASI) ~ Delt(Cl(TASI), k=1:10) +
                             myATR(TASI) + mySMI(TASI) + myADX(TASI) + 
                             myAroon(TASI) + myBB(TASI) + myCLV(TASI) +
                             CMO(Cl(TASI)) + EMA(Delt(Cl(TASI))) + 
                             myVolat(TASI) + myMACD(TASI) + 
                             RSI(Cl(TASI)) + mySAR(TASI) + 
                             runMean(Cl(TASI)) + runSD(Cl(TASI)))

set.seed(1243)
tasi.rf <- buildModel(tasi.data.model, method='randomForest',
                 training.per=c(start(TASI), index(TASI["2009-12-30"])),
                 ntree=50, importance=T)


varImpPlot(tasi.rf@fitted.model, type=1)
tasi.imp <- importance(tasi.rf@fitted.model, type=1)
rownames(tasi.imp)[which(tasi.imp > 10)]
