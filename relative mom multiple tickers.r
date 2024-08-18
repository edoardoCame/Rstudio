library(quantmod)
library(PerformanceAnalytics)


backtest.relativeMom <- function(maxDate,starting.capital=100,tic1,tic2,tic3,tic4,tic5){
  ticker1 <- getSymbols(tic1, from=maxDate, to=Sys.Date(), env = .GlobalEnv, auto.assign = FALSE)
  ticker2 <- getSymbols(tic2, from=maxDate, to=Sys.Date(), env = .GlobalEnv, auto.assign = FALSE)
  ticker3 <- getSymbols(tic3, from=maxDate, to=Sys.Date(), env = .GlobalEnv, auto.assign = FALSE)
  ticker4 <- getSymbols(tic4, from=maxDate, to=Sys.Date(), env = .GlobalEnv, auto.assign = FALSE)
  ticker5 <- getSymbols(tic5, from=maxDate, to=Sys.Date(), env = .GlobalEnv, auto.assign = FALSE)
  df <- data.frame
  
}


maxDate <- "2010-01-01"
ticker1 <- getSymbols("AAPL", from=maxDate, to=Sys.Date(), env = .GlobalEnv, auto.assign = FALSE)
ticker2 <- getSymbols("PFE", from=maxDate, to=Sys.Date(), env = .GlobalEnv, auto.assign = FALSE)
ticker3 <- getSymbols("GLD", from=maxDate, to=Sys.Date(), env = .GlobalEnv, auto.assign = FALSE)
ticker4 <- getSymbols("AMZN", from=maxDate, to=Sys.Date(), env = .GlobalEnv, auto.assign = FALSE)
ticker5 <- getSymbols("SPY", from=maxDate, to=Sys.Date(), env = .GlobalEnv, auto.assign = FALSE)

mom.period <- 7
df <- data.frame(ROC(ticker1[,4], n = mom.period, na.pad = FALSE), ROC(ticker2[,4], n = mom.period,na.pad = FALSE),
                 ROC(ticker3[,4], n = mom.period, na.pad = FALSE), ROC(ticker4[,4], n = mom.period, na.pad = FALSE),
                 ROC(ticker5[,4], n = mom.period, na.pad = FALSE))

df2 <- data.frame(dailyReturn(ticker1),
                  dailyReturn(ticker2),
                  dailyReturn(ticker3),
                  dailyReturn(ticker4),
                  dailyReturn(ticker5))[(mom.period+2):length(ticker1[,1]),]


#per determinare la colonna del migliore
test1 <- apply(df, MARGIN = 1, FUN = which.max)
test1.right <- test1[]




