library(readxl)
library(quantmod)

library(roll)

#test statistici sul prezzo di BTC



BTCUSDT <- read_xlsx("btc hourly data from 2018.xlsx", sheet = 1)


ETHUSDT <- read_xlsx("eth hourly data from 2018.xlsx", sheet = 1)



ticker1 <- BTCUSDT

ticker2 <- ETHUSDT

X = na.approx(ticker1[,5])
Y= na.approx(ticker2[,5])




adaptiveHedgeBacktest <- function(leverage=1,regLookback=20,bbLookback=20,bbStdev=1){
  
  
  #calcolo la regressione lineare ed estraggo il coefficiente per calcolare l'hedge ratio adattivo
  lookback <- regLookback
  df <- roll_lm(x= X, y = Y, width = lookback)
  
  
  #normale con la sottrazione
  #hedge.dynamic <-(Y - df$coefficients[,2] * X)
  
  #provo a testarlo con la frazione
  hedge.dynamic <-(Y / (df$coefficients[,2] * X))
  
  
  #plotto l'hedge ratio adattivo
  #print(plot(hedge.dynamic, type="l", main="spread"))
  
  
  
  
  
  #dataframe con il coefficiente da tenere in considerazione
  
  bbands <- BBands(hedge.dynamic, n = bbLookback, sd=bbStdev)[,4]
  
  buy.signals <- ifelse(bbands < 0, yes = 1, no = ifelse(bbands >= 0.5, yes = 0, no = NA))
  short.signals <- ifelse(bbands > 1, yes = -1, no = ifelse(bbands <= 0.5, yes = 0, no= NA))
  
  buy.signals[1] <- 0
  short.signals[1] <- 0
  
  buy.signals <- na.locf(buy.signals, na.rm = FALSE)
  short.signals <- na.locf(short.signals, na.rm = FALSE)
  signals <- buy.signals + short.signals
  
  
  #dataframe sulle posizioni
  dfPositions <- data.frame(ticker1 = X * - df$coefficients[,2] * signals, ticker2= Y * signals, signals)
  
  
  
  #data frame y2
  dfTickerReturns <- data.frame(ROC(X, n = 1), ROC(Y, n = 1))
  
  
  #dataframe returns per posizione
  
  dfReturns <- data.frame(ticker1=lag.xts(dfPositions[,1], k = 1)*dfTickerReturns[,1],
                          ticker2=lag.xts(dfPositions[,2], k = 1)*dfTickerReturns[,2])
  
  
  
  
  pnl <- dfReturns[,1] + dfReturns[,2]
  pnl <- na.fill(pnl, fill = 0)
  
  
  ret <- pnl / (lag.xts(abs(dfPositions[,1]), k = 1) + lag.xts(abs(dfPositions[,2]), k = 1))
  
  ret[is.na(ret)] <- 0
  
  total.returns <- ret
  
  
  
  equity <- array(NA, dim = length(total.returns))
  equity[1] <- 100
  for (i in 2:length(total.returns)) {
    equity[i] <- equity[i-1] * total.returns[i]*leverage + equity[i-1]
  }
  print(plot(equity, type="l", main="spread trading ETHBTC"))
  
  
  
  #statistiche per tracciare il progresso dell'ottimizzazione
  print(equity[length(equity)])
  
  cat("processing row:", regLookback, bbLookback, bbStdev, "\n")
  
  
  
  return(equity[length(equity)])
  
  
}

adaptiveHedgeBacktest(regLookback = 70, bbLookback = 2, bbStdev = 0.5)





range_reg <- seq(from=2, to=40, by=10)
range_bb <- seq(from=2,to=30,by=10)
range_stdev <- seq(from=0.5,to=2,by=0.5)
combinations <- expand.grid(range_reg, range_bb, range_stdev)


combinations$result <- mapply(adaptiveHedgeBacktest, regLookback=combinations$Var1, 
                             bbLookback=combinations$Var2, bbStdev=combinations$Var3)




#provo a plottare qualcosa con plotly
#FUNZIONA!
library(plotly)

plot_ly(data = combinations, x= ~combinations$Var1, y=~combinations$Var2, z=~combinations$result, type="scatter3d") %>%
  add_markers(color=~combinations$Var3)


plot_ly(y=~combinations$result, x=~combinations$Var1)



##################################################
#LINEARITA' DELL'EQUITY
##################################################



adaptiveHedgeBacktest <- function(leverage=1,regLookback=20,bbLookback=20,bbStdev=1){
  
  
  #calcolo la regressione lineare ed estraggo il coefficiente per calcolare l'hedge ratio adattivo
  lookback <- regLookback
  df <- roll_lm(x= X, y = Y, width = lookback)
  
  
  #normale con la sottrazione
  #hedge.dynamic <-(Y - df$coefficients[,2] * X)
  
  #provo a testarlo con la frazione
  hedge.dynamic <-(Y / (df$coefficients[,2] * X))
  
  
  #plotto l'hedge ratio adattivo
  #print(plot(hedge.dynamic, type="l", main="spread"))
  
  
  
  
  
  #dataframe con il coefficiente da tenere in considerazione
  
  bbands <- BBands(hedge.dynamic, n = bbLookback, sd=bbStdev)[,4]
  
  buy.signals <- ifelse(bbands < 0, yes = 1, no = ifelse(bbands >= 0.5, yes = 0, no = NA))
  short.signals <- ifelse(bbands > 1, yes = -1, no = ifelse(bbands <= 0.5, yes = 0, no= NA))
  
  buy.signals[1] <- 0
  short.signals[1] <- 0
  
  buy.signals <- na.locf(buy.signals, na.rm = FALSE)
  short.signals <- na.locf(short.signals, na.rm = FALSE)
  signals <- buy.signals + short.signals
  
  
  #dataframe sulle posizioni
  dfPositions <- data.frame(ticker1 = X * - df$coefficients[,2] * signals, ticker2= Y * signals, signals)
  
  
  
  #data frame y2
  dfTickerReturns <- data.frame(ROC(X, n = 1), ROC(Y, n = 1))
  
  
  #dataframe returns per posizione
  
  dfReturns <- data.frame(ticker1=lag.xts(dfPositions[,1], k = 1)*dfTickerReturns[,1],
                          ticker2=lag.xts(dfPositions[,2], k = 1)*dfTickerReturns[,2])
  
  
  
  
  pnl <- dfReturns[,1] + dfReturns[,2]
  pnl <- na.fill(pnl, fill = 0)
  
  
  ret <- pnl / (lag.xts(abs(dfPositions[,1]), k = 1) + lag.xts(abs(dfPositions[,2]), k = 1))
  
  ret[is.na(ret)] <- 0
  
  total.returns <- ret
  
  
  
  equity <- array(NA, dim = length(total.returns))
  equity[1] <- 100
  for (i in 2:length(total.returns)) {
    equity[i] <- equity[i-1] * total.returns[i]*leverage + equity[i-1]
  }
  print(plot(equity, type="l", main="spread trading ETHBTC"))
  
  
  
  #statistiche per tracciare il progresso dell'ottimizzazione
  print(equity[length(equity)])
  
  
  #statistiche per tracciare il progresso dell'ottimizzazione
  print( coef(lm(equity ~ seq_along(equity)))[2] )
  
  cat("processing row:", regLookback, bbLookback, bbStdev, "\n")
  
  return(coef(lm(equity ~ seq_along(equity)))[2])
  
  
}




combinations$linearity <- mapply(adaptiveHedgeBacktest, regLookback=combinations$Var1, 
                              bbLookback=combinations$Var2, bbStdev=combinations$Var3)
 



