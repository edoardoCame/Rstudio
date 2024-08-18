library(quantmod)
library(PerformanceAnalytics)
library(tseries)
library(urca)
library(zoo)
library(roll)
library(readxl)

rollinghedge_longonlyBacktest <- function(tic1, tic2, start.date, timeframe="daily",end.date=Sys.Date(),
                                          BBperiod=20, BBStdev=2, regression.lookback=20, feerate=0){
  
  
  ticker1 <- na.locf(getSymbols.yahoo(Symbols = tic1, from=start.date, to = end.date, 
                                        auto.assign=FALSE, periodicity = timeframe))
  
  ticker2 <- na.locf(getSymbols.yahoo(Symbols = tic2, from=start.date, to = end.date, 
                                        auto.assign=FALSE, periodicity = timeframe))
  
  
  
  
  #calcolo la regressione lineare ed estraggo il coefficiente per calcolare l'hedge ratio adattivo
  
  df <- roll_lm(x= ticker1[,4], y = ticker2[,4], width = regression.lookback)
  
  hedge.dynamic <-(ticker2[,4] - df$coefficients[,2] * ticker1[,4])
  
  tasso_normale <- ticker2[,4] / ticker1[,4]
  
  #plotto l'hedge ratio adattivo + le sue bande di bollinger se richiesto dall'utente
  
  
  #print(plot(hedge.dynamic, type="l", main="spread"))
  
  
  ##############
  #ENTRY LOGIC##
  ##############
  
  
  
  bbands <- BBands(hedge.dynamic, n = BBperiod, sd=BBStdev)$pctB
  
  
  buy.signals <- ifelse(bbands < 0, yes = 1, no = ifelse(bbands >= 0.5, yes = 0, no = NA))
  
  short.signals <- ifelse(bbands > 1, yes = -1, no = ifelse(bbands <= 0.5, yes = 0, no= NA))
  
  
  
  buy.signals[1] <- 0
  short.signals[1] <- 0
  
  buy.signals <- na.locf(buy.signals, na.rm = FALSE)
  short.signals <- na.locf(short.signals, na.rm = FALSE)
  
  
  portfolioSignals <- buy.signals + short.signals
  
  
  
  pricedata <- data.frame(price=hedge.dynamic, returns = ROC(tasso_normale, n = 1), 
                          zscore=bbands, entry_exit = lag.xts(portfolioSignals, k = 2))[30:length(hedge.dynamic),]
  
  colnames(pricedata) <- c("hedgeratio", "returns", "zscore", "signals")
  
  
  
  
  equity <- array(NA, dim=length(pricedata$hedgeratio))
  equity[1] <- 100
  
  for (i in 2:length(pricedata$hedgeratio)) {
    
    if (pricedata$signals[i] != pricedata$signals[i-1]) {
    
    equity[i] <- equity[i-1] * pricedata$returns[i] * pricedata$signals[i] * 1   + equity[i-1] - equity[i-1] * feerate/100 
    
    }
    
    else {
      
      equity[i] <- equity[i-1] * pricedata$returns[i] * pricedata$signals[i] * 1  + equity[i-1] 
    }
  }
  
  
  
  print(plot(equity, type="l", main=paste("bbperiod:", BBperiod, "stdev:", BBStdev, "reg lookback:",regression.lookback)))
  
  
  
  return(equity[length(equity)])
  
  
  
}





rollinghedge_longonlyBacktest(tic1="GLD", tic2 = "USO", start.date = "2006-05-24", end.date = Sys.Date(),
                              BBperiod = 5, BBStdev = 1, regression.lookback = 20)





#OVERFITTARE QUESTA COSA:
rollinghedge_longonlyBacktest(tic2="EURUSD=X", tic1 = "CHF=X", start.date = "2004-01-01", BBperiod = 25, 
                              BBStdev = 1, regression.lookback = 70, feerate = 0.015)




################################################################################################################






#ottimizzazione del backtest
range_reglookback <- seq(from=10, to= 100, by=10)
range_bb <- seq(from=5,to=220,by=20)
range_stdev <- seq(from=1,to=5,by=2)

combinations <- expand.grid(range_reglookback,range_bb, range_stdev)


combinations$equity <- mapply(FUN = rollinghedge_longonlyBacktest, tic2="EURUSD=X",
                              tic1="CHF=X", start.date="2004-01-01", BBperiod=combinations$Var2,
                              BBStdev=combinations$Var3, regression.lookback=combinations$Var1, feerate=0.01)


colnames(combinations) <- c("range lookback", "range bb", "range stdev", "equity")



library(plotly)

plot_ly(data = combinations, x= ~combinations$`range lookback`, y=~combinations$`range bb`, 
        z=~combinations$equity, type="scatter3d") %>%
  add_markers(color=combinations$`range stdev`)

#scatter with equity and regression range lookback
plot_ly(data=combinations, x= ~combinations$`range lookback`, y= ~combinations$equity) %>%
  add_markers(color= ~combinations$`range stdev`)



#scatter with equity and bb lookback
plot_ly(data=combinations, x= ~combinations$`range bb`, y= ~combinations$equity) %>%
  add_markers(color= ~combinations$`range stdev`)


