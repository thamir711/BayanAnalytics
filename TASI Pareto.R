## Fitting TASI returns into a Pareto distribution

library(gPdtest)
library(ggplot2)
source('~/R/BayanAnalytics/GetReturnsData.R')

## Considerint the threshold; i.e. theta, as the daily VaR at the 95% CI
theta <- sd(returns$TASI) * qnorm(0.05)
x <- -1 * returns$TASI[returns$TASI < theta]  ## Pareto random variable must be greater than 0
min(x)
length(x)
fit <- gpd.fit(x, "amle")
h <- hist(x, main="Pareto distribution of TASI left tail", xlab="Left tail returns")
rug(jitter(x))
xfit <- x
yfit <- (fit[1]/x^(1+fit[1])) * diff(h$mids[1:2]) * length(x)
points(xfit, yfit, pch=19)
legend(x="topright", c("tail index = 0.2637"), cex=0.8, bty="n")
lines(xfit, yfit)



## Considering the threshold as mu - 2 * sigma
theta <- mean(returns$TASI) - 2 * sd(returns$TASI)
x <- -1 * returns$TASI[returns$TASI < theta]
h <- hist(x)
rug(jitter(x))
fit <- gpd.fit(x, "amle")
xfit <- x
yfit <- fit[1]/xfit^(1+fit[1])
lines(xfit, yfit)
points(xfit, yfit)
