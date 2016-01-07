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

