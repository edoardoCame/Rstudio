

load("~/Desktop/ordina in base alla data/dataframe merged.RData")

library(PerformanceAnalytics)
library(tseries)
library(pracma)
library(zoo)
library(roll)
library(urca)
library(quantmod)
library(readr)
library(tseries)


#importo i dati
EURUSD <- read_delim("~/Desktop/ordina in base alla data/EURUSD.csv", 
                     delim = ";", escape_double = FALSE, col_names = FALSE, 
                     trim_ws = TRUE)[,1:5]
colnames(EURUSD) <- c("Date", "Open", "High", "Low", "Close")
EURUSD.xts <- xts(EURUSD[, -1], order.by = as.POSIXct(EURUSD$Date, format = "%Y-%m-%d %H:%M:%S"))   
GBPUSD <- read_delim("~/Desktop/ordina in base alla data/GBPUSD.csv", 
                     delim = ";", escape_double = FALSE, col_names = FALSE, 
                     trim_ws = TRUE)[,1:5]
colnames(GBPUSD) <- c("Date", "Open", "High", "Low", "Close")
GBPUSD.xts <- xts(GBPUSD[, -1], order.by = as.POSIXct(GBPUSD$Date, format = "%Y-%m-%d %H:%M:%S"))   

#merge dei dati in base al timestamp
test1 <- merge.xts(EURUSD.xts$Close, GBPUSD.xts$Close, fill=NA)
colnames(test1) <- c("EURUSD", "GBPUSD")

test1$EURUSD <- na.locf(test1$EURUSD)
test1$GBPUSD <- na.locf(test1$GBPUSD)

#subset della serie temporale:
test2 <- window(test1, start = NULL, end = '2019-01-01') #comando window selezioni le date.






##################################################
#FUNZIONE PER CALCOLARE L'HEDGE RATIO "OTTIMALE"##
##################################################


optimal.hedgeAdj <- function(ticker1, ticker2){
  
  
  
  lr <- lm(ticker2 ~ ticker1) 
  
  beta <- lr$coefficients[2]
  
  #zeta <- lr$residuals[,1] #questi sono i residuals che 
  #sono identici all'hedge ratio di chan, NON usarli
  
  
  
  hedge.ratio <- (ticker2 - beta * ticker1)
  
  print(plot(hedge.ratio, type="l", main= paste("hedge ratio beta:", beta)))
  print(adf.test(hedge.ratio, k=1))
  return(beta)
}


REG.BETA <- optimal.hedgeAdj(ticker1 = test2[,1], ticker2 = test2[,2])




#la cointegrazione esiste e lo spread è stazionario.
johansentest <- summary(ca.jo(test1, type="eigen", K=2))












######################################################
#WALK FORWARD ANALYSIS MANUALE
#-----------------------------------------------------

walkforward.hedgeratio <- test1[,2] - REG.BETA * test1[,1]
stationarity.result <- adf.test(walkforward.hedgeratio, k=1)



#BISOGNEREBBE TESTARE I RITORNI DI QUESTO SPREAD "WALK FORWARD" CON UNA FUNZIONE
#DI BACKTEST!

test1$EURret <- ROC(test1$EURUSD)
test1$GBPret <- ROC(test1$GBPUSD)
test1$spreadReturns <- (test1$GBPret - REG.BETA * test1$EURret) #ritorni del portafoglio


backtestFunction <- function(bbPeriod=20, bbStdev=3, feeRate=0.02, feesCalculation=FALSE){
  
  
#bbands logic:
bbands <- BBands(walkforward.hedgeratio, n = bbPeriod, sd=bbStdev)$pctB
buy.signals <- ifelse(bbands < 0, yes = 1, no = ifelse(bbands >= 0.5, yes = 0, no = NA))
short.signals <- ifelse(bbands > 1, yes = -1, no = ifelse(bbands <= 0.5, yes = 0, no= NA))
buy.signals[1] <- 0
short.signals[1] <- 0
buy.signals <- na.locf(buy.signals, na.rm = FALSE)
short.signals <- na.locf(short.signals, na.rm = FALSE)

portfolioSignals <- buy.signals + short.signals
  
lalla <- unclass(portfolioSignals)
  
portfolioReturns <- lag.xts(portfolioSignals, k=1) * test1$spreadReturns

ritorniCumulativi <<- portfolioReturns


equity <- array(NA, dim = length(portfolioReturns))
equity[1] <- 100

if (feesCalculation==TRUE) {
    
for (i in 2:length(equity)) {
  
  if (identical(lalla[i-1], lalla[i])) {
    equity[i] <- equity[i-1] * portfolioReturns[i] + equity[i-1] 
  } else{
    equity[i] <- equity[i-1] * portfolioReturns[i] + equity[i-1] - equity[i-1] * (feeRate/100)
  }
  
}

} else {
  
  equity <- cumprod(1 + na.omit(portfolioReturns)) - 1
}

  plot(equity, type="l", main="cointegration approach EURGBP")
}


#funziona molto bene con questi settings; abbassare la stdev e togliere
#le fees per vedere se funziona così bene con una frequenza piu alta dei trades.
backtestFunction(bbPeriod = 180, bbStdev = 6, feeRate = 0.02, feesCalculation = FALSE)
backtestFunction(bbPeriod = 240, bbStdev = 6, feeRate = 0.02)
backtestFunction(bbPeriod = 180, bbStdev = 6, feeRate = 0.02)


backtestFunction(bbPeriod = 1440, bbStdev = 2, feesCalculation = FALSE)





range_bb <- seq(from=60, to = 360, by=60)
range_sdev <- seq(from=1, to= 3, by=2)
combinations <- expand.grid(BBlookback=range_bb, BBStdev=range_sdev)



save.image("~/Desktop/ordina in base alla data/dataframe merged.RData")


