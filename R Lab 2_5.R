library(quantmod)
library(ellipse)

dataPath <- "~/R/BayanAnalytics/data/"
startDate <- as.Date("2007-01-06")

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

returns <- data.frame()

for (symbol in symbols) {
  data <- read.csv(paste(dataPath, symbol, ".csv", sep=""))
  data <- xts(data[, c("Open","High","Low","Close","Volume")], order.by=as.Date(data[, "Index"], format="%Y-%m-%d"))
  data <- window(data, start=startDate)
  
  print(paste(symbol, ": ", length(data), " rows", sep=""))
  returns <- cbind(returns, Delt(Cl(data), type="log"))
}

colnames(returns) <- symbols
returns <- returns[-1, ]

cor(returns)
symnum(cor(returns))

# Exclude TASI All Index from the correlation table.
# Plot the correlation matrix.
ctab <- cor(returns[, -1])
ctab <- round(ctab, 2)
plotcorr(ctab, numbers=TRUE, type="lower", diag=TRUE, mar=c(0.1, 0.1, 0.1, 0.1), cex.lab=0.75)

# Finding the sectors with the strongest correlation.
# To use the max() function, we need to remove the correlation between a sector and itself (i.e. correlation = 1)
ctab[ctab == 1] <- NA
max(ctab, na.rm=TRUE) # the max correlation is found to be 0.85
table(ctab == 0.85)
ctab == 0.85 # check visually, the strongest correlation is found to be beween TASI.INI and TASI.BDC

# TASI.INI and TASI.BDC dialy log returns have the highest positive correlation.
cor(returns[, "TASI.INI"], returns[, "TASI.BDC"])
plot(as.numeric(returns[, "TASI.INI"]), as.numeric(returns[, "TASI.BDC"]), xlab="TASI.INI", ylab="TASI.BDC")

# Finding the sectors with the weakest correlation.
min(ctab, na.rm=TRUE) # the max correlation is found to be 0.85
table(ctab == 0.35)
ctab == 0.35 # check visually, the weakest correlation is found to be beween TASI.EU and TASI.MAP

# TASI.EU and TASI.MAP dialy log returns have the lowest positive correlation.
cor(returns[, "TASI.EU"], returns[, "TASI.MAP"])
plot(as.numeric(returns[, "TASI.EU"]), as.numeric(returns[, "TASI.MAP"]), xlab="TASI.EU", ylab="TASI.MAP")


# TASI and TASI.PCI returns have the highest positive correlation.
cor(returns[, "TASI"], returns[, "TASI.PCI"])
plot(as.numeric(returns[, "TASI"]), as.numeric(returns[, "TASI.PCI"]))

# TASI and TASI.MAP returns have the lowest positive correlation.
cor(returns[, "TASI"], returns[, "TASI.MAP"])
plot(as.numeric(returns[, "TASI"]), as.numeric(returns[, "TASI.MAP"]))
