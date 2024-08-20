library(quantmod)
library(tseries)
library(roll)
library(cryptoQuotes)

ticker1 <- na.approx(getSymbols.yahoo("EURGBP=X", from="2010-01-01", to=Sys.Date(), auto.assign=FALSE))

ticker1sma <- SMA(ticker1[,4], n=10)

df <- data.frame(ticker1[,4], ticker1sma)[10:length(ticker1sma),]

reg <- roll_lm(df[,1], df[,2], width = 20)

df.new <- data.frame(df[,1], df[,2], reg$coefficients[,2])[20:length(ticker1sma),]

colnames(df.new) <- c("CLOSE", "SMA", "REG COEFFICIENT")


hedge.dynamic <-(df.new$SMA - df.new$`REG COEFFICIENT` * df.new$CLOSE)
plot(hedge.dynamic,type="l")








