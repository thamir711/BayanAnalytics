data <- read.csv("data/etc/EOD_20160204.csv", header=T, stringsAsFactors=F)
colnames(data) <- c("Ticker", "Date", "Open", "High", "Low", "Close", "Volume")

portfolio.value <- 1000000
max.stocks <- 3
no.portfolios <- 4

random.portfolios <- list()

for (ii in 1:no.portfolios) {
  random.portfolios[[ii]] <- sample(1:max.stocks, 1)
}

data <- read.csv("data/gulfbase/WTD20160211.csv")
data <- data[, c(1, 2, 3, 6)]
colnames(data) <- c("Ticker", "Date", "Open", "Close")
library(reshape2)
data <- melt(data, id.vars=c("Ticker", "Date"))
