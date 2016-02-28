library(quantmod)
library(reshape2)

weekly.files <- c("WTD20160211.csv", "WTD20160218.csv", "WTD20160225.csv")
portfolio.value <- 1000000
max.stocks <- 20     ## maximum number of stocks in a portfolio
no.portfolios <- 100000  ## number of random portfolios
plot.returns <- TRUE

portfolio.mean <- numeric()
portfolio.sd <- numeric()

for (fi in weekly.files) {
  raw.data <- read.csv(paste("data/gulfbase/", fi, sep=""), header=T, stringsAsFactors=F)
  ## Find TASI.BFS rows; these rows mark the begining of the indices which are excluded 
  ## from the list of assets available for constructing random portfolios
  rw <- which(raw.data[, 1]== "TASI.BFS")
  lastRow <- rw[1] - 1                         ## the last row which is an asset; i.e. not an index
  fromDate <- as.character(raw.data[rw[5], 2]) ## the week's starting date
  toDate <- as.character(raw.data[rw[1], 2])   ## the week's ending date
  print(paste(fi, lastRow, fromDate, toDate, length(rw)))
  
  ## Since the raw data contains weekly returns, rw must equal 5 days
  stopifnot(length(rw) == 5)
  
  stock.data <- raw.data[1:lastRow, c(1, 2, 3, 6)]  ## Exclude High, Low and Volume columns
  colnames(stock.data) <- c("Ticker", "Date", "Open", "Close")
  data.molten <- melt(stock.data, id.vars=c("Ticker", "Date"))
  
  ## Casting using "date" id.variable
  data.casted <- dcast(data.molten, ... ~ Date)
  ## Get the week's opening prices
  stock.data <- data.casted[data.casted$variable == "Open", c("Ticker", fromDate)]
  ## Get the week's closing prices
  stock.data <- cbind(stock.data, data.casted[data.casted$variable == "Close", toDate])
  colnames(stock.data) <- c("Ticker", "Open", "Close")
  ## Get the log weekly return for each stock
  stock.data <- cbind(stock.data, Delt(stock.data$Open, stock.data$Close, type="log"))
  colnames(stock.data) <- c("Ticker", "Open", "Close", "Return")
  rm(raw.data, data.molten, data.casted)
  
  portfolio.assets <- list()
  portfolio.weights <- list()
  portfolio.returns <- numeric()
  
  for (ii in 1:no.portfolios) {
    n <- sample(1:nrow(stock.data), sample(1:max.stocks, 1))  ## randomly select the assets in the random portfolio
    r <- runif(length(n)); w <- r/sum(r)  ## randomly choose the asets weights in the random portfolio
    
    portfolio.assets[[ii]] <- stock.data[n, "Return"]
    names(portfolio.assets[[ii]]) <- stock.data[n, "Ticker"]
    portfolio.weights[[ii]] <- w
    names(portfolio.weights[[ii]]) <- stock.data[n, "Ticker"]
    
    portfolio.returns <- c(portfolio.returns, sum(w * stock.data[n, "Return"]))
  }
  portfolio.mean <- c(portfolio.mean, mean(portfolio.returns))
  ## names(portfolio.mean) <- c(names(portfolio.mean), toDate)
  portfolio.sd <- c(portfolio.sd, sd(portfolio.returns))
  ## names(portfolio.sd) <- c(names(portfolio.sd), toDate)
  
  if (plot.returns) {
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
  }
}
names(portfolio.mean) <- weekly.files
names(portfolio.sd) <- weekly.files

portfolio.mean
portfolio.sd

## END EXECUTION

## stock.data <- read.csv("data/gulfbase/WTD20160211.csv", header=T, stringsAsFactors=F)
stock.data <- read.csv("data/gulfbase/WTD20160218.csv", header=T, stringsAsFactors=F)
stock.data <- stock.data[1:860, c(1, 2, 3, 6)]  ## Exclude sector indices rows; exclude High, Low and Volume columns
colnames(stock.data) <- c("Ticker", "Date", "Open", "Close")
data.molten <- melt(stock.data, id.vars=c("Ticker", "Date"))

## Casting using "date" id.variable
data.cast.date <- dcast(data.molten, ... ~ Date)
stock.data <- data.cast.date[data.cast.date$variable == "Open", c("Ticker", "20160214")]
stock.data <- cbind(stock.data, data.cast.date[data.cast.date$variable == "Close", c("20160218")])
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
