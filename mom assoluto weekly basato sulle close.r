library(quantmod)
library(PerformanceAnalytics)
library(magrittr)
library(readxl)


maxDate <- "1980-01-01"
starting_capital <- 100
ticker <- "BTC-USD"

ticker1 <- getSymbols(ticker, auto.assign = FALSE, from=maxDate)
ticker1.returns.daily <- dailyReturn(ticker1)
ticker1.returns.weekly <- weeklyReturn(ticker1)

t1_weekly <- to.weekly(ticker1)
t1_weekly_lag1 <- lag(t1_weekly$ticker1.Close, k=1)
t1_weekly_lag4 <- lag(t1_weekly$ticker1.Close, k=5)

#data_frame_lags <- data.frame(t1_weekly$ticker1.Close, t1_weekly_lag1$ticker1.Close,
                              #t1_weekly_lag4$ticker1.Close)


#lag1>lag4 if true return current weekly return

a_b <- ifelse(t1_weekly_lag1>t1_weekly_lag4, ticker1.returns.weekly, 0)

#test1 <- data.frame(a_b, t1_weekly, t1_weekly_lag1, t1_weekly_lag4)


above_below <- na.exclude(a_b)

equity <- array(NA, dim=length(above_below))
equity[1] <- starting_capital
for (i in 2:length(above_below)) {
  equity[i] <- (equity[i-1] * above_below$ticker1.Close[i]) + equity[i-1]
}

#returns_equity <- data.frame(equity, above_below$ticker1.Close)

###COMPARAZIONE COL MERCATO buy n hold###

#metto i ritorni del mercato partendo dalla 5a posizione per allinearmi con l'equity
t1weekreturns <- ticker1.returns.weekly$weekly.returns[5:length(ticker1.returns.weekly$weekly.returns)]       
buy_n_hold <- array(NA,dim=length(t1weekreturns$weekly.returns))
buy_n_hold[1] <- starting_capital
for (i in 2:length(t1weekreturns$weekly.returns)) {
  buy_n_hold[i] <- (buy_n_hold[i-1] * t1weekreturns$weekly.returns[i]) + buy_n_hold[i-1]
}



plot(equity, type="l")
lines(buy_n_hold, col="red")


##drawdowns##

#chart.Drawdown(above_below)
#chart.Drawdown(ticker1.returns.weekly)

rm(list=ls())
