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
returns <- returns[-1, ]
df <- returns
returns <- xts(returns[, -1], order.by=as.Date(returns[, "Index"], format="%Y-%m-%d"))

plot(returns[, "TASI"], auto.grid=FALSE, minor.ticks=FALSE, main="TASI Daily Returns", ylab="log return")
length(returns[, "TASI"])          ## Number of observatons
mean(returns[, "TASI"])            ## Mean returns
sd(returns[, "TASI"])              ## Standard deviaiton per day
sd(returns[, "TASI"]) * sqrt(250)  ## Volatility
skewness(returns[, "TASI"])        ## Skewness
kurtosis(returns[, "TASI"]) - 3    ## Excess kurtosis

## Calculating number of observations below the lowest bound of 99% confidence internal
0.01 * length(returns[, "TASI"])  # Number of observations below the lower bound of 99% under the assumption of normaility.
lower.bound <- - 2.33 * sd(returns[, "TASI"])
sum(returns[, "TASI"] < lower.bound) # Number of "actual" observations below the lower bound of 99%. Clearly TASI violate the assumpton of normality.
## Notice the dates in which observations are below the lower bound of 99%
returns[which(returns[, "TASI"] < lower.bound), "TASI"]

## Plotting observations below the lowest bound of 99% CI
plot(returns[, "TASI"], auto.grid=FALSE, minor.ticks=FALSE, main="TASI Daily Returns", ylab="log return")
points(returns[which(returns[, "TASI"] < lower.bound), "TASI"], col="red", pch=20)
## Plotting observations above the highest bound of 99% CI
points(returns[which(returns[, "TASI"] > -lower.bound), "TASI"], col="green", pch=20)
abline(h=lower.bound, col="red", lty=2)

## Plotting observations conditional on months
plot(as.vector(returns$TASI), .indexmon(returns$TASI))

## Using lattice graphics
library(lattice)
library(Hmisc)
xyplot(returns$TASI)

## A conditional box plot of TASI index log returns
tasi.index <- data.frame(Return=as.vector(returns$TASI), Month=factor(.indexmon(returns$TASI)+1))
bwplot(Month ~ Return, data=tasi.index, xlab="log return", ylab="Month", panel=panel.bpplot, probs=seq(.01, .49, by=.01), datadensity=TRUE)
bwplot(Month ~ Return, data=tasi.index, xlab="log return", ylab="Month")

## Using ggplot2
qplot(Return, Month, data=tasi.index)
tasi.index$IsBelow <- tasi.index$Return < lower.bound
qplot(Return, Month, data=tasi.index, color=IsBelow)
## Histogram uing facets
qplot(Return, data=tasi.index, facets=IsBelow ~ .)
## Histogram by groups
qplot(Return, data=tasi.index, fill=IsBelow)

## Statistical analysis for TASI sectors
tasi.count <- lapply(returns, nrow)
tasi.mean <- lapply(returns, mean)
tasi.sd <- lapply(returns, sd)
tasi.volatility <- lapply(returns, function(ret) sd(ret) * sqrt(250))
tasi.skewness <- lapply(returns, skewness)
tasi.kurtosis <- lapply(returns, function(ret) kurtosis(ret) - 3)
tasi.below.obs <- lapply(returns, function(ret) sum(ret < (-2.33 * sd(ret))))

## Using ggplot2 to plot histograms and density for all sectors
## df is the return data.frame without changing it into xts
library(reshape2)
df.melt <- melt(df[, colnames(df)[-1]], measure.vars=colnames(df)[-1])
ggplot(df.melt, aes(x=value)) + geom_histogram(fill="white", colour="black") + facet_grid(variable ~ ., scales="free")
ggplot(df.melt, aes(x=value), y=..density..) + geom_histogram(fill="white", colour="black") + geom_density() + facet_grid(variable ~ .)
ggplot(df.melt, aes(x=value), y=..density..) + geom_density() + facet_grid(variable ~ ., scales="free")
ggplot(df.melt, aes(x=variable, y=value)) + geom_boxplot(outlier.size=1.5, outlier.shape=21) + stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")

## Using ggplot2 to plot histograms for 3 sectors at a time
df.temp <- subset(df.melt, variable %in% c("TASI.BFS", "TASI.PCI", "TASI.CMT"))
df.temp$variable <- factor(df.temp$variable)
ggplot(df.temp, aes(x=variable, y=value)) + geom_boxplot(outlier.size=1.5, outlier.shape=21) + stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white") + labs(title="Box plot of sectors return", x="sector", y="log return")

df.temp <- subset(df.melt, variable %in% c("TASI.RTL", "TASI.EU", "TASI.AFI"))
df.temp$variable <- factor(df.temp$variable)
ggplot(df.temp, aes(x=variable, y=value)) + geom_boxplot(outlier.size=1.5, outlier.shape=21) + stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white") + labs(title="Box plot of sectors return", x="sector", y="log return")

df.temp <- subset(df.melt, variable %in% c("TASI.TIT", "TASI.INS", "TASI.MUI"))
df.temp$variable <- factor(df.temp$variable)
ggplot(df.temp, aes(x=variable, y=value)) + geom_boxplot(outlier.size=1.5, outlier.shape=21) + stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white") + labs(title="Box plot of sectors return", x="sector", y="log return")

df.temp <- subset(df.melt, variable %in% c("TASI.INI", "TASI.BDC", "TASI.RED"))
df.temp$variable <- factor(df.temp$variable)
ggplot(df.temp, aes(x=variable, y=value)) + geom_boxplot(outlier.size=1.5, outlier.shape=21) + stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white") + labs(title="Box plot of sectors return", x="sector", y="log return")

df.temp <- subset(df.melt, variable %in% c("TASI.TRA", "TASI.MAP", "TASI.HTT"))
df.temp$variable <- factor(df.temp$variable)
ggplot(df.temp, aes(x=variable, y=value)) + geom_boxplot(outlier.size=1.5, outlier.shape=21) + stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white") + labs(title="Box plot of sectors return", x="sector", y="log return")
