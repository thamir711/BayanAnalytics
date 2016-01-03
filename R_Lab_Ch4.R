library(quantmod)
library(ggplot2)

source("GetReturnsData.R")

## Histogram of the daily log returns on TASI index from 6-Jan-2007 to 31-Dec-2015.
hist(returns$TASI, breaks=30)                           ## 30 cells, full range
hist(returns$TASI, breaks=30, xlim=c(-0.04, 0.04))      ## 30 cells, central range
hist(returns$TASI, breaks=20, xlim=c(-0.04, 0.04))      ## 20 cells, central range
hist(returns$TASI, breaks=80, xlim=c(-0.04, 0.04))      ## 80 cells, central range

## Using ggplot2
qplot(returns$TASI)
