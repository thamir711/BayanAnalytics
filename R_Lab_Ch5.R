library(quantmod)
library(ggplot2)

source("GetReturnsData.R")

## TASI returns histogram with t-distribution (df=2) curve
x <- returns$TASI
h <- hist(x, breaks=30, xlab="return", main="TASI log returns", prob=T)
rug(jitter(x))
## Overlaying the t-density curve
xfit <- seq(min(x), max(x), length=length(x))
yfit <- dt(xfit, df=2)
yfit <- yfit * diff(h$mids[1:2]) * length(x)
lines(xfit, yfit, col="red", lwd=2)

curve(dt(x, df=2), add=T, col="blue")

x <- returns$TASI
hx <- dnorm(x)
plot(x, hx, type="l", lty=2, xlab="Returns", ylab="Density", main="Comparison of t Distributions")
degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
for (i in 1:4) {
  lines(x, dt(x, degf[i]), lwd=2, col=colors[i])
}



x <- seq(-4, 4, length=100)
hx <- dnorm(x)
degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, hx, type="l", lty=2, xlab="x value", ylab="Density", main="Comparison of t Distributions")

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)