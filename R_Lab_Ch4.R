library(quantmod)
library(ggplot2)

source("GetReturnsData.R")

## Histogram of the daily log returns on TASI index from 6-Jan-2007 to 31-Dec-2015.
hist(returns$TASI, breaks=30)                           ## 30 cells, full range
hist(returns$TASI, breaks=30, xlim=c(-0.04, 0.04))      ## 30 cells, central range
hist(returns$TASI, breaks=20, xlim=c(-0.04, 0.04))      ## 20 cells, central range
hist(returns$TASI, breaks=80, xlim=c(-0.04, 0.04))      ## 80 cells, central range

## TASI returns histogram with normal curve
x <- returns$TASI
h <- hist(x, breaks=30, xlab="return", main="TASI log returns", ylim=c(0, 650))
rug(jitter(x))
## Overlaying the kernel density estimation
d <- density(x, adjust=1.5)
xfit <- d$x
yfit <- d$y * diff(h$mids[1:2]) * length(x)
lines(xfit, yfit, col="blue", lwd=2)
legend(x="topright", c("kernel density estimaiton"), lty=1, col="blue", cex=0.8, bty="n")
## Overlaying the normal density curve
xfit <- seq(min(x), max(x), length=length(x))
yfit <- dnorm(xfit, mean=mean(x), sd=sd(x))
## yfit <- dnorm(xfit, mean=median(x), sd=mad(x))
yfit <- yfit * diff(h$mids[1:2]) * length(x)
lines(xfit, yfit, col="red", lwd=2)
legend(x="topright", c("kernel density estimaiton", "normal curve"), lty=c(1, 1), col=c("blue", "red"), cex=0.8, bty="n")

## TASI sectors returns histogram with normal curve
for (symbol in symbols) {
  x <- returns[, symbol]
  h <- hist(x, breaks=30, xlab="return", main=paste(symbol, " log returns", sep=""), ylim=c(0, 750))
  rug(jitter(x))
  ## Overlaying the kernel density estimation
  d <- density(x, adjust=1.5)
  xfit <- d$x
  yfit <- d$y * diff(h$mids[1:2]) * length(x)
  lines(xfit, yfit, col="blue", lwd=2)
  legend(x="topright", c("kernel density estimaiton"), lty=1, col="blue", cex=0.8, bty="n")
  ## Overlaying the normal density curve
  xfit <- seq(min(x), max(x), length=length(x))
  yfit <- dnorm(xfit, mean=mean(x), sd=sd(x))
  ## yfit <- dnorm(xfit, mean=median(x), sd=mad(x))
  yfit <- yfit * diff(h$mids[1:2]) * length(x)
  lines(xfit, yfit, col="red", lwd=2)
  legend(x="topright", c("kernel density estimaiton", "normal curve"), lty=c(1, 1), col=c("blue", "red"), cex=0.8, bty="n")
}

## Using Kernel Density Estimation (KDE)
density(returns$TASI)
plot(density(returns$TASI))
lines(density(returns$TASI, adjust=1/3), lty=2)               ## adjust = 1/3, undersmoothed and overfit
lines(density(returns$TASI, adjust=3), lty=3)                 ## adjust = 3, oversmoothed and underfit
rug(jitter(returns$TASI))
## Using KDE on random data having the same mean and sd as TASI returns
plot(density(rnorm(2245, mean=mean(returns$TASI), sd=sd(returns$TASI))))

## Using ggplot2
qplot(returns$TASI)
ggplot(returns, aes(x=TASI)) + geom_histogram(binwidth=diff(range(returns$TASI))/30)

## Normal Q-Q Plot
qqnorm(returns$TASI, datax=TRUE)
qqline(returns$TASI, datax=TRUE)

## Plotting Fig. 4.11
plot(density(rlnorm(150, 0, 1)))
qqnorm(rlnorm(150, 0, 1), datax=TRUE)
qqline(rlnorm(150, 0, 1), datax=TRUE)

## Plot KDE using all the kernels
kernels <- eval(formals(density.default)$kernel)
plot(density(returns$TASI))
for (i in 2:length(kernels)) lines(density(returns$TASI, kernel=kernels[i]), col=i)
legend(x="topright", legend = kernels, col = seq(kernels), lty = 1, cex = .8, y.intersp = 1)

## Plotting TASI Density Function and Normal Q-Q Plot
par(mfrow=c(1, 2))
plot(density(returns$TASI, kernel="rectangular", adjust=3/4, from=-0.045, to=0.045), main="TASI log returns", xlab="log returns")
qqnorm(returns$TASI, datax=TRUE)
qqline(returns$TASI, datax=TRUE)
par(mfrow=c(1, 1))

## QQ plot between TASI log returns and t-student distribution
df <- 2
qqplot(returns$TASI, rt(length(returns$TASI), df))  ## The two samples have the same size, then need only plot their order statistics against each other.
lmfit <- lm(qt(c(.25, .75), df=df) ~ quantile(sort(returns$TASI), c(.25, .75)))
abline(lmfit)

## The two samples do not have the same size, then one computes the same sets of sample quantiles for each and plots them.
n <- length(returns$TASI); grid <- (1:n)/(n+1)
qqplot(sort(returns$TASI), qt(grid, df=df))
lmfit <- lm(qt(c(.25, .75), df=df) ~ quantile(sort(returns$TASI), c(.25, .75)))
abline(lmfit)

## Figure 4.15: Normal and t probability plots
par(mfrow=c(3, 2))
qqplot(returns$TASI, rnorm(length(returns$TASI)), main="Normal probability plot", xlab="log return", ylab="normal quantiles")
lmfit <- lm(qnorm(c(.25, .75)) ~ quantile(sort(returns$TASI), c(.25, .75)))
abline(lmfit)
# t-distribution with 1 degrees of freedom
df <- 1
qqplot(returns$TASI, rt(length(returns$TASI), df), main="t-probability, df = 1", xlab="log return", ylab="t-quantiles")
lmfit <- lm(qt(c(.25, .75), df=df) ~ quantile(sort(returns$TASI), c(.25, .75)))
abline(lmfit)
# t-distribution with 2 degrees of freedom
df <- 2
qqplot(returns$TASI, rt(length(returns$TASI), df), main="t-probability, df = 2", xlab="log return", ylab="t-quantiles")
lmfit <- lm(qt(c(.25, .75), df=df) ~ quantile(sort(returns$TASI), c(.25, .75)))
abline(lmfit)
# t-distribution with 4 degrees of freedom
df <- 4
qqplot(returns$TASI, rt(length(returns$TASI), df), main="t-probability, df = 4", xlab="log return", ylab="t-quantiles")
lmfit <- lm(qt(c(.25, .75), df=df) ~ quantile(sort(returns$TASI), c(.25, .75)))
abline(lmfit)
# t-distribution with 8 degrees of freedom
df <- 8
qqplot(returns$TASI, rt(length(returns$TASI), df), main="t-probability, df = 8", xlab="log return", ylab="t-quantiles")
lmfit <- lm(qt(c(.25, .75), df=df) ~ quantile(sort(returns$TASI), c(.25, .75)))
abline(lmfit)
# t-distribution with 15 degrees of freedom
df <- 15
qqplot(returns$TASI, rt(length(returns$TASI), df), main="t-probability, df = 15", xlab="log return", ylab="t-quantiles")
lmfit <- lm(qt(c(.25, .75), df=df) ~ quantile(sort(returns$TASI), c(.25, .75)))
abline(lmfit)
par(mfrow=c(1, 1))
