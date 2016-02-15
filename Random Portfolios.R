## data <- read.csv("data/etc/EOD_20160204.csv", header=T, stringsAsFactors=F)
## colnames(data) <- c("Ticker", "Date", "Open", "High", "Low", "Close", "Volume")

library(quantmod)
library(reshape2)
stock.data <- read.csv("data/gulfbase/WTD20160211.csv", header=T, stringsAsFactors=F)
stock.data <- stock.data[1:860, c(1, 2, 3, 6)]  ## Exclude sector indices rows; exclude High, Low and Volume columns
colnames(stock.data) <- c("Ticker", "Date", "Open", "Close")
data.molten <- melt(stock.data, id.vars=c("Ticker", "Date"))

## Casting using "date" id.variable
data.cast.date <- dcast(data.molten, ... ~ Date)
stock.data <- data.cast.date[data.cast.date$variable == "Open", c("Ticker", "20160207")]
stock.data <- cbind(stock.data, data.cast.date[data.cast.date$variable == "Close", c("20160211")])
colnames(stock.data) <- c("Ticker", "Open", "Close")
stock.data <- cbind(stock.data, Delt(stock.data$Open, stock.data$Close, type="log"))
colnames(stock.data) <- c("Ticker", "Open", "Close", "Return")
rm(data.molten, data.cast.date)

## h <- hist(stock.data$Return, xlab="return", main="TASI log returns")
## rug(jitter(stock.data$Return))

portfolio.value <- 1000000
max.stocks <- 20     ## maximum number of stocks in a portfolio
no.portfolios <- 100000  ## number of random portfolios

portfolio.assets <- list()
portfolio.weights <- list()
portfolio.returns <- numeric()

for (ii in 1:no.portfolios) {
  n <- sample(1:nrow(stock.data), sample(1:max.stocks, 1))
  r <- runif(length(n))
  w <- r/sum(r)
  
  portfolio.assets[[ii]] <- stock.data[n, "Return"]
  names(portfolio.assets[[ii]]) <- stock.data[n, "Ticker"]
  portfolio.weights[[ii]] <- w
  names(portfolio.weights[[ii]]) <- stock.data[n, "Ticker"]
  
  portfolio.returns <- c(portfolio.returns, sum(w * stock.data[n, "Return"]))
}

h <- hist(portfolio.returns, xlab="Log return", main="Random Portfolio Returns")
rug(jitter(portfolio.returns))
d <- density(portfolio.returns)
xfit <- d$x
yfit <- d$y * diff(h$mids[1:2]) * length(portfolio.returns)
lines(xfit, yfit, col="blue", lwd=2)
legend(x="topright", c("kernel density estimaiton"), lty=1, col="blue", cex=0.8, bty="n")
## Overlaying the normal density curve
xfit <- seq(min(portfolio.returns), max(portfolio.returns), length=length(portfolio.returns))
yfit <- dnorm(xfit, mean=mean(portfolio.returns), sd=sd(portfolio.returns))
yfit <- yfit * diff(h$mids[1:2]) * length(portfolio.returns)
lines(xfit, yfit, col="red", lwd=2)
legend(x="topright", c("kernel density estimaiton", "normal curve"), lty=c(1, 1), col=c("blue", "red"), cex=0.8, bty="n")
