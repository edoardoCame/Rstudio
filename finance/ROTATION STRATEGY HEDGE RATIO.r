library(quantmod)
library(PerformanceAnalytics)



backtest.monthly <- function(tic1,tic2,maxDate,ticker.period="monthly", 
                             sma.period=20,starting.capital=100){
  
  ticker1 <- getSymbols(tic1, auto.assign=FALSE, from=maxDate, to=Sys.Date(), env = .GlobalEnv, periodicity=ticker.period)[,1:4]
  ticker2 <- getSymbols(tic2, auto.assign=FALSE, from=maxDate, to=Sys.Date(), env = .GlobalEnv,periodicity=ticker.period)[,1:4]
  hedge.ratio <- ticker1[,4] / ticker2[,4]
  
  ticker1.returns <- Delt(ticker1[,1], ticker1[,4])
  ticker2.returns <- Delt(ticker2[,1], ticker2[,4])
  hedge.ratio.sma <- SMA(hedge.ratio, n = sma.period)
  
  #returns dataframe
  df <- data.frame(ticker1.returns, ticker2.returns, lag.xts(hedge.ratio, k=1), lag.xts(hedge.ratio.sma,k=1))
  colnames(df) <- c("ticker1.returns", "ticker2.returns", "hedge ratio", "hedge SMA")
  
  #condizioni rotazione
  df.naomit <- df[21:length(df[,1]),]
  
  equity.returns <- ifelse(df.naomit$`hedge ratio`>df.naomit$`hedge SMA`, df.naomit$ticker1.returns,
                           df.naomit$ticker2.returns)
  
  
  equity <- array(NA, dim=length(equity.returns))
  equity[1] <- starting.capital
  for (i in 2:length(equity.returns)) {
    equity[i] <- (equity[i-1] * equity.returns[i]) + equity[i-1]
  }
  
  
  buynhold1 <- array(NA, dim=length(equity.returns))
  buynhold1[1] <- starting.capital
  for (i in 2:length(df.naomit$ticker1.returns)) {
    buynhold1[i] <- (buynhold1[i-1] * df.naomit$ticker1.returns[i]) + buynhold1[i-1]
  }
  
  
  buynhold2 <- array(NA, dim=length(equity.returns))
  buynhold2[1] <- starting.capital
  for (i in 2:length(df.naomit$ticker2.returns)) {
    buynhold2[i] <- (buynhold2[i-1] * df.naomit$ticker2.returns[i]) + buynhold2[i-1]
  }
  
  
  
  
  plot(equity, type="l")
  lines(buynhold1, col="red")
  lines(buynhold2, col="blue")
  return(print(list(final.equity = equity[length(equity)], bench1=buynhold1[length(buynhold1)], bench2=buynhold2[length(buynhold2)])))
  
  
}

backtest.monthly(tic1 = "SPY", tic2="GLD", maxDate = "2005-01-01")



backtest.daily <- function(maxDate,tic1,tic2,ticker.period="daily",
                           sma.period=20,starting.capital=100){

  ticker1 <- getSymbols(tic1, auto.assign=FALSE, from=maxDate, to=Sys.Date(), env = .GlobalEnv, periodicity=ticker.period)[,1:4]
  ticker2 <- getSymbols(tic2, auto.assign=FALSE, from=maxDate, to=Sys.Date(), env = .GlobalEnv,periodicity=ticker.period)[,1:4]
  hedge.ratio <- ticker1[,4] / ticker2[,4]
  
  ticker1.returns <- dailyReturn(ticker1)
  ticker2.returns <- dailyReturn(ticker2)
  hedge.ratio.sma <- SMA(hedge.ratio, n = sma.period)
  
  #returns dataframe
  df <- data.frame(ticker1.returns, ticker2.returns, lag.xts(hedge.ratio, k=2), lag.xts(hedge.ratio.sma,k=2))
  colnames(df) <- c("ticker1.returns", "ticker2.returns", "hedge ratio", "hedge SMA")
  
  #condizioni rotazione
  df.naomit <- df[(sma.period+2):length(df[,1]),]
  
  equity.returns <- ifelse(df.naomit$`hedge ratio`>df.naomit$`hedge SMA`, df.naomit$ticker1.returns,
                           df.naomit$ticker2.returns)
  
  
  equity <- array(NA, dim=length(equity.returns))
  equity[1] <- starting.capital
  for (i in 2:length(equity.returns)) {
    equity[i] <- (equity[i-1] * equity.returns[i]) + equity[i-1]
  }
  
  
  buynhold1 <- array(NA, dim=length(equity.returns))
  buynhold1[1] <- starting.capital
  for (i in 2:length(df.naomit$ticker1.returns)) {
    buynhold1[i] <- (buynhold1[i-1] * df.naomit$ticker1.returns[i]) + buynhold1[i-1]
  }
  
  
  buynhold2 <- array(NA, dim=length(equity.returns))
  buynhold2[1] <- starting.capital
  for (i in 2:length(df.naomit$ticker2.returns)) {
    buynhold2[i] <- (buynhold2[i-1] * df.naomit$ticker2.returns[i]) + buynhold2[i-1]
  }
  
  
  
  
  plot(equity, type="l")
  lines(buynhold1, col="red")
  lines(buynhold2, col="blue")
  return(print(list(strategy=equity[length(equity)], bench1=buynhold1[length(buynhold1)],bench2=buynhold2[length(buynhold2)],
                    equityDD=maxDrawdown(equity.returns), tic1DD=maxDrawdown(ticker1.returns), tic2DD=maxDrawdown(ticker2.returns))))
  
}

backtest.daily(tic1 = "QQQ",tic2 = "GLD",maxDate="2005-01-01", 
               sma.period = 10)



backtest.daily.onlyLONGtic1 <- function(maxDate,tic1,tic2,ticker.period="daily",
                           sma.period=20,starting.capital=100){
  
  ticker1 <- getSymbols(tic1, auto.assign=FALSE, from=maxDate, to=Sys.Date(), env = .GlobalEnv, periodicity=ticker.period)[,1:4]
  ticker2 <- getSymbols(tic2, auto.assign=FALSE, from=maxDate, to=Sys.Date(), env = .GlobalEnv,periodicity=ticker.period)[,1:4]
  hedge.ratio <- ticker1[,4] / ticker2[,4]
  
  ticker1.returns <- dailyReturn(ticker1)
  ticker2.returns <- dailyReturn(ticker2)
  hedge.ratio.sma <- SMA(hedge.ratio, n = sma.period)
  
  #returns dataframe
  df <- data.frame(ticker1.returns, ticker2.returns, lag.xts(hedge.ratio, k=2), lag.xts(hedge.ratio.sma,k=2))
  colnames(df) <- c("ticker1.returns", "ticker2.returns", "hedge ratio", "hedge SMA")
  
  #condizioni rotazione
  df.naomit <- df[(sma.period+2):length(df[,1]),]
  
  equity.returns <- ifelse(df.naomit$`hedge ratio`>df.naomit$`hedge SMA`, df.naomit$ticker1.returns,
                           0)
  
  
  equity <- array(NA, dim=length(equity.returns))
  equity[1] <- starting.capital
  for (i in 2:length(equity.returns)) {
    equity[i] <- (equity[i-1] * equity.returns[i]) + equity[i-1]
  }
  
  
  buynhold1 <- array(NA, dim=length(equity.returns))
  buynhold1[1] <- starting.capital
  for (i in 2:length(df.naomit$ticker1.returns)) {
    buynhold1[i] <- (buynhold1[i-1] * df.naomit$ticker1.returns[i]) + buynhold1[i-1]
  }
  
  
  buynhold2 <- array(NA, dim=length(equity.returns))
  buynhold2[1] <- starting.capital
  for (i in 2:length(df.naomit$ticker2.returns)) {
    buynhold2[i] <- (buynhold2[i-1] * df.naomit$ticker2.returns[i]) + buynhold2[i-1]
  }
  
  
  
  
  plot(equity, type="l")
  lines(buynhold1, col="red")
  lines(buynhold2, col="blue")
  return(print(list(strategy=equity[length(equity)], bench1=buynhold1[length(buynhold1)],bench2=buynhold2[length(buynhold2)]),
               equityDD=maxDrawdown(equity.returns), tic1DD=maxDrawdown(ticker1.returns), tic2DD=maxDrawdown(ticker2.returns)))
  
}
backtest.daily.onlyLONGtic1(maxDate = "2010-01-01", tic1 = "SPY", tic2 = "^VIX",sma.period = 3)




#vari test tentando di creare una tripla rotazione

maxDate <- "2005-01-01"
ticker1 <- getSymbols.yahoo("SPY", env = globalenv(), auto.assign=FALSE, from = maxDate, to = Sys.Date())
ticker2 <- getSymbols.yahoo("QQQ", env = globalenv(), auto.assign=FALSE, from = maxDate, to = Sys.Date())
ticker3 <- getSymbols.yahoo("GLD", env = globalenv(), auto.assign=FALSE, from = maxDate, to = Sys.Date())

hedge1 <- ticker2[,1] / ticker1[,1]
hedge2 <- hedge1 / ticker3[,1]
hedge3 <- ticker1[,1] / ticker3[,1]

plot(hedge1, type="l")
plot(hedge2, type="l")
plot(hedge3, type="l")
lines(x = DEMA(hedge3, n = 100), col = "red")

