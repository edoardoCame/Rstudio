
library(readr)
EURUSD_H1 <- read_delim("EURUSD_H1.csv", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE)


close.prices <- EURUSD_H1$Close

vwap <- VWMA(price = close.prices, volume = EURUSD_H1$Volume)

plot(close.prices, type="l")
lines(vwap, col="red")

df <- data.frame(close.prices, lag.xts(vwap, k = 1))
colnames(df) <- c("close", "vwap")


signals <- ifelse(test = df$close > df$vwap, yes = 1, no = -1)






rm(list=ls())
