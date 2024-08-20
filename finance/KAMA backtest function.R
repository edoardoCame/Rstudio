


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
signals <- ifelse(df$Close < df$KAMA, yes = 1, no = -1)

df$Signals <- lag.xts(signals, k = 1)
colnames(df) <- c("Close", "KAMA", "Returns", "Signals")

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
                     escape_double = FALSE, trim_ws = TRUE)[,1:5]

xts <- xts(EURUSD[,-1], order.by = as.POSIXct(EURUSD$Time, format = "%Y-%m-%d %H:%M:%S"))


#------------------------------------------------------------------------


#QQQ

xts <- getSymbols.yahoo("QQQ", from="2001-01-01", to=Sys.Date(), auto.assign=FALSE)[,1:4]

colnames(xts) <- c("Open", "High", "Low", "Close")

#------------------------------------------------------------------------





KAMABacktest <- function(effRatio=60, nfast=60,nslow=240, inverse=FALSE, plot.equity=FALSE, feeRate=0.0001){
  
  
  kama <- KAMA(xts, nER = effRatio, nFast = nfast, nSlow = nslow)
  
  df <- data.frame(close=xts$Close, KAMA=kama, Returns=ROC(xts$Close))
  
  
  if (inverse==TRUE) {
    signals <- ifelse(df$Close < df$KAMA, yes = 1, no = -1)
  } else {
    signals <- ifelse(df$Close > df$KAMA, yes = 1, no = -1)
  }
  
  df$Signals <- lag.xts(signals, k = 1)
  colnames(df) <- c("Close", "KAMA", "Returns", "Signals")
  
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
    print(plot(equity, type="l", main= paste("eff ratio:", effRatio, "fast:", nfast, "slow:", nslow)))       
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
  
  
  print(cat("equity:",equity[length(equity)], "total trades",number.of.trades, "\n", "eff ratio:", effRatio, "fast:", nfast, "slow:", nslow))
  
  return(equity[length(equity)])
  
}

KAMABacktest(effRatio = 10, nfast = 2, nslow = 30, plot.equity = TRUE, inverse = FALSE, feeRate = 0)


#ottimizzazione del backtest
range_ef <- seq(from=20, to= 600, by=20)
range_nfast <- seq(from=20, to= 600, by=20)
range_nslow <- seq(from=60, to= 600, by=20)

combinations <- expand.grid(range_ef, range_nfast, range_nslow)


combinations$equity <- mapply(FUN = KAMABacktest, effRatio=combinations$Var1, nfast=combinations$Var2, nslow=combinations$Var3,
                              plot.equity=TRUE, inverse=FALSE, feeRate=0)







