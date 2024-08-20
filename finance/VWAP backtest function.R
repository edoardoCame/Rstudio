


library(xts)
library(DSTrading)
library(quantmod)
#kaufman adaptive moving average test 1


library(readr)
NZDCAD <- read_delim("~/Desktop/NZDCAD from 2008 to today.csv", 
                     delim = ";", escape_double = FALSE, col_names = TRUE, 
                     trim_ws = TRUE)
colnames(NZDCAD) <- c("Date", "Open", "High", "Low", "Close", "Volume")
NZDCAD$Volume <- NULL

xts <- xts(NZDCAD[, -1], order.by = as.POSIXct(NZDCAD$Date, format = "%Y-%m-%d %H:%M:%S"))


kama <- KAMA(xts, nER = 60, nFast = 60, nSlow = 240)

df <- data.frame(close=xts$Close, KAMA=kama, Returns=ROC(xts$Close))

#al contrario funziona di più?
signals <- ifelse(df$Close < df$VWAP, yes = 1, no = -1)

df$Signals <- lag.xts(signals, k = 1)
colnames(df) <- c("Close", "VWAP", "Returns", "Signals")

total.returns <- df$Returns * df$Signals

equity <- array(NA, dim = length(total.returns))
equity[1] <- 100
for (i in 2:length(equity)) {
  if (identical(df$Signals[i] , df$Signals[i-1])) {
    equity[i] <- equity[i-1] * total.returns[i] + equity[i-1]
  } else {
    
    equity[i] <- equity[i-1] * total.returns[i] + equity[i-1] - equity[i-1] * 0.0001
  }
  
}

plot(equity, type="l")


total.trades <- array(NA, dim=length(signals))
for (i in 1:length(signals)) {
  if (identical(signals[i], signals[i-1])) {
    total.trades[i] <- 0
  } else {
    total.trades[i] <- 1
  }
}

sum(total.trades)











#Funzione di ottimizzazione:
#---------------------------

#NZDCAD
library(readr)
NZDCAD <- read_delim("~/Desktop/NZDCAD from 2008 to today.csv", 
                     delim = ";", escape_double = FALSE, col_names = TRUE, 
                     trim_ws = TRUE)
colnames(NZDCAD) <- c("Date", "Open", "High", "Low", "Close", "Volume")
NZDCAD$Volume <- NULL

xts <- xts(NZDCAD[, -1], order.by = as.POSIXct(NZDCAD$Date, format = "%Y-%m-%d %H:%M:%S"))

#------------------------------------------------------------------------


#EURUSD
library(readr)
EURUSD <- read_delim("EURUSD_M30.csv", delim = "\t", 
                     escape_double = FALSE, trim_ws = TRUE)

#EURUSD
library(readr)
EURUSD <- read_delim("EURUSD_M1.csv", delim = "\t", 
                     escape_double = FALSE, trim_ws = TRUE)



xts <- xts(EURUSD[,-1], order.by = as.POSIXct(EURUSD$Time, format = "%Y-%m-%d %H:%M:%S"))


#------------------------------------------------------------------------


#QQQ

xts <- getSymbols.yahoo("QQQ", from="2001-01-01", to=Sys.Date(), auto.assign=FALSE)[,1:5]

colnames(xts) <- c("Open", "High", "Low", "Close", "Volume")

#------------------------------------------------------------------------





VWAPBacktest <- function(VWAPperiod=60,inverse=FALSE, plot.equity=FALSE, feeRate=0.01, longOnly=FALSE){
  
  
  vwap <- VWAP(xts$Close, volume = xts$Volume, n=VWAPperiod)
  vwap[1] <- xts$Close[1]
  vwap <- na.locf(vwap)
  
  df <- data.frame(close=xts$Close, VWAP=vwap, Returns=ROC(xts$Close))
  
  
  if (longOnly==TRUE) {
    
    if (inverse==TRUE) {
    signals <- ifelse(df$Close < df$VWAP, yes = 1, no = 0)
    } else {
      signals <- ifelse(df$Close > df$VWAP, yes = 1, no = 0)
    }
    
  } else {
    
    if (inverse==TRUE) {
      signals <- ifelse(df$Close < df$VWAP, yes = 1, no = -1)
    } else {
      signals <- ifelse(df$Close > df$VWAP, yes = 1, no = -1)
    }
    
  }
  
  df$Signals <- lag.xts(signals, k = 1)
  colnames(df) <- c("Close", "VWAP", "Returns", "Signals")
  
  total.returns <- df$Returns * df$Signals
  
  
  equity <- array(NA, dim = length(total.returns))
  equity[1] <- 100
  
  for (i in 2:length(equity)) {
    
    if (identical(df$Signals[i] , df$Signals[i-1])) {
      
      equity[i] <- equity[i-1] * total.returns[i] + equity[i-1]
      
    } else {
      
      equity[i] <- equity[i-1] * total.returns[i] + equity[i-1] - equity[i-1] * feeRate/100
    }
    
  }
  
  if (plot.equity==TRUE) {
    print(plot(equity, type="l", main= paste("lookback period:",VWAPperiod)))       
  }
  
  total.trades <- array(NA, dim=length(signals))
  for (i in 1:length(signals)) {
    if (identical(signals[i], signals[i-1])) {
      total.trades[i] <- 0
    } else {
      total.trades[i] <- 1
    }
  }
  
  number.of.trades <- sum(total.trades)/2
  
  
  print(cat("equity:",equity[length(equity)], "total trades:",number.of.trades,"lookback:",VWAPperiod,fill = TRUE))
  
  return(equity[length(equity)])
  
}



VWAPBacktest(plot.equity = TRUE, feeRate = 0.01, inverse = FALSE, VWAPperiod = 20, longOnly = TRUE)



range_lookback <- seq(from=20000, to= 100000, by=10000)

combinations <- data.frame(range_lookback)
combinations$equity <- mapply(FUN=VWAPBacktest, VWAPperiod=combinations$range_lookback, plot.equity=TRUE,
                              inverse=FALSE, feeRate=0.008)





