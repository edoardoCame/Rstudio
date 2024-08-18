library(quantmod)
library(PerformanceAnalytics)
library(tseries)
library(urca)
library(zoo)
library(roll)

#############################
#ROLLING HEDGE RATIO FUNCTION
#############################


rolling.hedge <- function(tic1,tic2,start.date,end.date=Sys.Date(),lookback=20,bands=FALSE, bandsLookback=5,bandsStdev=0.5,title="spread"){
  
  #importo i ticker da Yahoo Finance
  ticker1 <- na.approx(getSymbols.yahoo(Symbols = tic1, from=start.date, to = end.date, auto.assign=FALSE))
  ticker2 <- na.approx(getSymbols.yahoo(Symbols = tic2, from=start.date, to = end.date, auto.assign=FALSE))
  
  #calcolo la regressione lineare ed estraggo il coefficiente per calcolare l'hedge ratio adattivo
  df <- roll_lm(x= ticker1[,4], y = ticker2[,4], width = lookback)
  hedge.dynamic <-(ticker2[,4] - df$coefficients[,2] * ticker1[,4])
  
  #riporto l'esponente di Hurst per indicare la stazionarietà dell'hedge ratio adattivo
  print(HurstIndex(hedge.dynamic))
  
  #plotto l'hedge ratio adattivo + le sue bande di bollinger se richiesto dall'utente
  print(plot(hedge.dynamic, type="l", main=title))
  if (bands==TRUE) {
    bbands <- BBands(hedge.dynamic, n=bandsLookback, sd=bandsStdev)
    lines(bbands[,1], col="red")
    lines(bbands[,3], col="red")
  }
}



rolling.hedge(tic1="GLD", tic2="USO", start.date = "2006-05-24", 
              end.date = "2012-04-09", lookback = 20, bands = FALSE)


rolling.hedge(tic1 = "BTC-USD", tic2 = "ETH-USD", start.date = "2018-01-01", lookback = 10)

rolling.hedge(tic2 = "NZDUSD=X", tic1 = "AUDUSD=X", start.date = "2020-01-01", lookback = 20, bands = TRUE)







#################################################
##ROLLING HEDGE RATIO BACKTEST STRUCTURE
#################################################

start.date <- "2008-01-01"
end.date <- Sys.Date()
ticker1 <- na.approx(getSymbols.yahoo(Symbols = "AUDUSD=X", from=start.date, to = end.date, 
                                      auto.assign=FALSE, periodicity = "daily"))

ticker2 <- na.approx(getSymbols.yahoo(Symbols = "NZDUSD=X", from=start.date, to = end.date, 
                                      auto.assign=FALSE, periodicity = "daily"))

#calcolo la regressione lineare ed estraggo il coefficiente per calcolare l'hedge ratio adattivo
lookback <- 5
df <- roll_lm(x= ticker1[,4], y = ticker2[,4], width = lookback)
hedge.dynamic <-(ticker2[,4] - df$coefficients[,2] * ticker1[,4])


#plotto l'hedge ratio adattivo
plot(hedge.dynamic, type="l", main="spread")





#dataframe con il coefficiente da tenere in considerazione

bbands <- BBands(hedge.dynamic, n = 60, sd=0.2)$pctB

buy.signals <- ifelse(bbands < 0, yes = 1, no = ifelse(bbands >= 0.5, yes = 0, no = NA))
short.signals <- ifelse(bbands > 1, yes = -1, no = ifelse(bbands <= 0.5, yes = 0, no= NA))

buy.signals[1] <- 0
short.signals[1] <- 0

buy.signals <- na.locf(buy.signals, na.rm = FALSE)
short.signals <- na.locf(short.signals, na.rm = FALSE)
signals <- buy.signals + short.signals



######################
#data frame positions
######################

#mi assicuro che davanti all'hedge ratio ci sia sempre un meno in questo modo la mia posizione è sempre
#inversa rispetto a quella del ticker2

dfPositions <- data.frame(audusd = ticker1[,4] * - df$coefficients[,2] * signals, nzdusd=ticker2[,4] * signals, signals)


#data frame y2
dfTickerReturns <- data.frame(ROC(ticker1[,4], n = 1), ROC(ticker2[,4], n = 1))


#dataframe returns per posizione

dfReturns <- data.frame(AUD=lag.xts(dfPositions$AUDUSD.X.Close, k = 1)*dfTickerReturns$AUDUSD.X.Close,
                        NZD=lag.xts(dfPositions$NZDUSD.X.Close, k = 1)*dfTickerReturns$NZDUSD.X.Close)

pnl <- dfReturns$AUD + dfReturns$NZD
pnl <- na.fill(pnl, fill = 0)

ret <- pnl / (lag.xts(abs(dfPositions$AUDUSD.X.Close), k = 1) + lag.xts(abs(dfPositions$NZDUSD.X.Close), k = 1))

ret[is.na(ret)] <- 0

total.returns <- ret





equity <- array(NA, dim = length(total.returns))
equity[1] <- 100
for (i in 2:length(total.returns)) {
  equity[i] <- equity[i-1] * total.returns[i] + equity[i-1]
}
plot(equity, type="l")









###############################
#PROVO A RENDERLO UNA FUNZIONE:
###############################



adaptiveHedgeBacktest <- function(tic1,tic2,start.date="2008-01-01",end.date=Sys.Date(),leverage=1,regLookback=20,bbLookback=20,bbStdev=1){
  
  
  
  ticker1 <- na.approx(getSymbols.yahoo(Symbols = tic1, from=start.date, to = end.date, 
                                        auto.assign=FALSE, periodicity = "daily"))
  
  ticker2 <- na.approx(getSymbols.yahoo(Symbols = tic2, from=start.date, to = end.date, 
                                        auto.assign=FALSE, periodicity = "daily"))
  
  #calcolo la regressione lineare ed estraggo il coefficiente per calcolare l'hedge ratio adattivo
  lookback <- regLookback
  df <- roll_lm(x= ticker1[,6], y = ticker2[,6], width = lookback)
  hedge.dynamic <-(ticker2[,6] - df$coefficients[,2] * ticker1[,6])
  
  
  #plotto l'hedge ratio adattivo
  print(plot(hedge.dynamic, type="l", main="spread"))
  
  
  
  
  
  #dataframe con il coefficiente da tenere in considerazione
  
  bbands <- BBands(hedge.dynamic, n = bbLookback, sd=bbStdev)$pctB
  
  buy.signals <- ifelse(bbands < 0, yes = 1, no = ifelse(bbands >= 0.5, yes = 0, no = NA))
  short.signals <- ifelse(bbands > 1, yes = -1, no = ifelse(bbands <= 0.5, yes = 0, no= NA))
  
  buy.signals[1] <- 0
  short.signals[1] <- 0
  
  buy.signals <- na.locf(buy.signals, na.rm = FALSE)
  short.signals <- na.locf(short.signals, na.rm = FALSE)
  signals <- buy.signals + short.signals
  
  
  #dataframe sulle posizioni
  dfPositions <- data.frame(ticker1 = ticker1[,6] * - df$coefficients[,2] * signals, ticker2=ticker2[,6] * signals, signals)
  
  
  
  #data frame y2
  dfTickerReturns <- data.frame(ROC(ticker1[,6], n = 1), ROC(ticker2[,6], n = 1))
  
  
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
  print(plot(equity, type="l", main=paste("spread trading with rolling reg", tic1, tic2), 
             xlab=paste("from", start.date, "to", end.date)))
  
  
  
  
}



adaptiveHedgeBacktest(tic1 = "EWA", tic2 = "EWC", regLookback = 21, bbLookback = 50)


adaptiveHedgeBacktest(tic1 = "GLD", tic2 = "USO", start.date = "2006-05-25", end.date = "2012-04-09", 
                      regLookback = 20, bbLookback = 20, bbStdev = 1, leverage = 1)



adaptiveHedgeBacktest(tic1 = "GLD", tic2 = "USO", start.date = "2006-05-25", end.date = Sys.Date(), 
                      regLookback = 20, bbLookback = 20, bbStdev = 1)

#FUNZIONA! E' IDENTICO!


adaptiveHedgeBacktest(tic1 = "TXG.TO", tic2 = "OGC.TO", regLookback = 20, bbLookback = 10, bbStdev = 2,
                      start.date = "2011-01-01")



#vari backtest interessanti in generale
adaptiveHedgeBacktest(tic1 = "EURUSD=X", tic2 = "GBPUSD=X", start.date = "2008-01-01", regLookback = 21, 
                      bbLookback = 50, bbStdev = 1)

adaptiveHedgeBacktest(tic1 = "BTC-USD", tic2 = "BCH-USD", start.date = "2018-01-01", regLookback = 25,
                      bbLookback = 10, bbStdev = 0.1, leverage = 1)


adaptiveHedgeBacktest(tic1 = "AUDUSD=X", tic2 = "NZDUSD=X", start.date = "2008-01-01", regLookback = 20,
                      bbLookback = 60, bbStdev = 2, leverage = 1)



###############################################################

################################################
#Proviamo ad ottimizzare i parametri di backtest
################################################




range_reg <- seq(from=5, to=100, by=10)
range_bb <- seq(from=5,to=100,by=10)
range_stdev <- seq(from=0.25,to=2,by=0.25)
combinations <- expand.grid(range_reg, range_bb, range_stdev)

##devo creare una funzione sintetica che non plotti niente ma che dia semplicemente
##il risultato.



##############################
#FUNZIONE CHE RIPORTA L'EQUITY
##############################

adaptiveHedgeBacktest <- function(tic1="EURUSD=X",tic2="GBPUSD=X",start.date="2008-01-01",end.date=Sys.Date(),regLookback=20,bbLookback=20,bbStdev=1){
  
  
  
  ticker1 <- (na.approx(getSymbols.yahoo(Symbols = tic1, from=start.date, to = end.date, 
                                        auto.assign=FALSE, periodicity = "daily")))
  
  ticker2 <- (na.approx(getSymbols.yahoo(Symbols = tic2, from=start.date, to = end.date, 
                                        auto.assign=FALSE, periodicity = "daily")))
  
  #calcolo la regressione lineare ed estraggo il coefficiente per calcolare l'hedge ratio adattivo
  lookback <- regLookback
  df <- roll_lm(x= (ticker1[,6]), y = (ticker2[,6]), width = lookback)
  hedge.dynamic <- ((ticker2[,6]) - df$coefficients[,2] * (ticker1[,6]))
  
  
  #plotto l'hedge ratio adattivo
  #print(plot(hedge.dynamic, type="l", main="spread"))
  
  
  
  
  
  #dataframe con il coefficiente da tenere in considerazione
  
  bbands <- BBands(hedge.dynamic, n = bbLookback, sd=bbStdev)$pctB
  
  buy.signals <- ifelse(bbands < 0, yes = 1, no = ifelse(bbands >= 0.5, yes = 0, no = NA))
  short.signals <- ifelse(bbands > 1, yes = -1, no = ifelse(bbands <= 0.5, yes = 0, no= NA))
  
  buy.signals[1] <- 0
  short.signals[1] <- 0
  
  buy.signals <- na.locf(buy.signals, na.rm = FALSE)
  short.signals <- na.locf(short.signals, na.rm = FALSE)
  signals <- buy.signals + short.signals
  
  
  #dataframe sulle posizioni
  dfPositions <- data.frame(ticker1 = ticker1[,6] * - df$coefficients[,2] * signals, ticker2=ticker2[,6] * signals, signals)
  
  
  
  #data frame y2
  dfTickerReturns <- data.frame(ROC(ticker1[,6], n = 1), ROC(ticker2[,6], n = 1))
  
  
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
    equity[i] <- equity[i-1] * total.returns[i] + equity[i-1]
  }
  print(plot(equity, type="l", main= paste("chart for: (reg, bb, stdev)", regLookback, bbLookback, bbStdev)))
  
  
  #statistiche per tracciare il progresso dell'ottimizzazione
  print(equity[length(equity)])
  
  cat("processing row:", regLookback, bbLookback, bbStdev, "\n")
  
  return(equity[length(equity)])
  
  
  
  
}



combinations$result <- mapply(adaptiveHedgeBacktest, regLookback=combinations$Var1, 
                              bbLookback=combinations$Var2, bbStdev=combinations$Var3)





#provo a plottare qualcosa con plotly
#FUNZIONA!
library(plotly)

plot_ly(data = combinations, x= ~combinations$Var1, y=~combinations$Var2, z=~combinations$result, type="scatter3d") %>%
  add_markers(color=~combinations$Var3)



plot_ly(y=~combinations$result, x=~combinations$Var1)



#lo salvo in un file excel in modo da modellare meglio i grafici
library(openxlsx)
write.xlsx(combinations, file = "C:/Users/edoardo/OneDrive/BUSINESS/R directory/algotrading CHAN tests/backtest scripts/combinations AUDNDZ.xlsx")







###################################
#FUNZIONE CHE RIPORTA LA LINEARITA'
###################################




adaptiveHedgeBacktest <- function(tic1="ETH-USD",tic2="BTC-USD",start.date="2018-01-01",end.date=Sys.Date(),regLookback=20,bbLookback=20,bbStdev=1){
  
  
  
  ticker1 <- na.approx(getSymbols.yahoo(Symbols = tic1, from=start.date, to = end.date, 
                                        auto.assign=FALSE, periodicity = "daily"))
  
  ticker2 <- na.approx(getSymbols.yahoo(Symbols = tic2, from=start.date, to = end.date, 
                                        auto.assign=FALSE, periodicity = "daily"))
  
  #calcolo la regressione lineare ed estraggo il coefficiente per calcolare l'hedge ratio adattivo
  lookback <- regLookback
  df <- roll_lm(x= ticker1[,6], y = ticker2[,6], width = lookback)
  hedge.dynamic <-(ticker2[,6] - df$coefficients[,2] * ticker1[,6])
  
  
  #plotto l'hedge ratio adattivo
  #print(plot(hedge.dynamic, type="l", main="spread"))
  
  
  
  
  
  #dataframe con il coefficiente da tenere in considerazione
  
  bbands <- BBands(hedge.dynamic, n = bbLookback, sd=bbStdev)$pctB
  
  buy.signals <- ifelse(bbands < 0, yes = 1, no = ifelse(bbands >= 0.5, yes = 0, no = NA))
  short.signals <- ifelse(bbands > 1, yes = -1, no = ifelse(bbands <= 0.5, yes = 0, no= NA))
  
  buy.signals[1] <- 0
  short.signals[1] <- 0
  
  buy.signals <- na.locf(buy.signals, na.rm = FALSE)
  short.signals <- na.locf(short.signals, na.rm = FALSE)
  signals <- buy.signals + short.signals
  
  
  #dataframe sulle posizioni
  dfPositions <- data.frame(ticker1 = ticker1[,6] * - df$coefficients[,2] * signals, ticker2=ticker2[,6] * signals, signals)
  
  
  
  #data frame y2
  dfTickerReturns <- data.frame(ROC(ticker1[,6], n = 1), ROC(ticker2[,6], n = 1))
  
  
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
    equity[i] <- equity[i-1] * total.returns[i] + equity[i-1]
  }
  #print(plot(equity, type="l", main= paste("chart for: (reg, bb, stdev)", regLookback, bbLookback, bbStdev)))
  
  
  #statistiche per tracciare il progresso dell'ottimizzazione
  print( coef(lm(equity ~ seq_along(equity)))[2] )
  
  cat("processing row:", regLookback, bbLookback, bbStdev, "\n")
  
  return(coef(lm(equity ~ seq_along(equity)))[2])
  
  
  
  
}



combinations$coeff <- mapply(adaptiveHedgeBacktest, regLookback=combinations$Var1, 
                              bbLookback=combinations$Var2, bbStdev=combinations$Var3)








rm(list=ls())


