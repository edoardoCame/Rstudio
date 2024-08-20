library(quantmod)
library(PerformanceAnalytics)

ticker <- "ETH-USD"
ticker_2 <- "BTC-USD"
maxDate <- "2010-07-01"
starting_capital <- 100
ticker1 <- getSymbols.yahoo(ticker, from=maxDate, to=Sys.Date(), env =  .GlobalEnv, auto.assign=FALSE)[,1:4]
ticker2 <- getSymbols.yahoo(ticker_2, from=maxDate, to=Sys.Date(), env =  .GlobalEnv, auto.assign=FALSE)[,1:4]

hedge1 <- ticker1 / ticker2

ticker1.daily.returns <- Delt(hedge1[,1], hedge1[,4])

ticker1.lag1 <- lag(hedge1[,4], k=1)
ticker1.lag2 <- lag(hedge1[,4], k=8)

df <- data.frame(ticker1[,4],ticker1.lag1, ticker1.lag2,ticker1.daily.returns)

above_below <- na.omit(ifelse(ticker1.lag1>ticker1.lag2, ticker1.daily.returns, ticker1.daily.returns*-1))


#equity calculation
equity <- array(NA, dim=length(above_below))
equity[1] <- starting_capital
for (i in 2:length(above_below)) {
  equity[i] <- (equity[i-1] * above_below[i]) + equity[i-1]
}

plot(equity, type="l")




rm(list=ls())
