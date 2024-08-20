library(quantmod)
library(TTR)
library(readxl)
library(tseries)


setwd("~/Desktop/EURGBP TESTS")

########################
#LO RENDO UNA FUNZIONE
########################

#devo comunque importare i dati prima


EURGBP <- read.csv("EURGBP 1 HOUR FROM 2008.csv", sep = "", row.names = NULL) #colonna 6




library(readr)
EURGBP <- read_delim("EURGBP_M5.csv", delim = "\t", 
                     escape_double = FALSE, trim_ws = TRUE)



library(readr)
EURGBP <- read_delim("eurgbp from 2015 to 2023.csv", 
                     delim = ";", escape_double = FALSE, col_names = FALSE, 
                     trim_ws = TRUE) #colonna 5




library(readr)
EURGBP <- read_delim("EURGBP from 2002.csv", 
                     delim = ";", escape_double = FALSE, col_names = FALSE, 
                     trim_ws = TRUE)




library(readr)
EURGBP <- read_delim("EURGBP 2023.csv", delim = ";", 
                     escape_double = FALSE, col_names = FALSE, 
                     trim_ws = TRUE)





#provo una walk forward manuale



eurgbp.backtest <- function(price_series,bblookback=20, bbStdev=2, leverage=1, feeRate=0.0001, logEquity=FALSE){
  
  
  returns <- ROC(price_series, n = 1)
  
  bands <- BBands(price_series, n = bblookback, sd = bbStdev, maType = "EMA")[,4]
  
  
  
  buy.signals <- ifelse(bands < 0, yes = 1, no = ifelse(bands >= 0.5, yes = 0, no = NA))
  
  short.signals <- ifelse(bands > 1, yes = -1, no = ifelse(bands <= 0.5, yes = 0, no= NA))
  
  
  buy.signals[1] <- 0
  short.signals[1] <- 0
  
  buy.signals <- na.locf(buy.signals, na.rm = FALSE)
  short.signals <- na.locf(short.signals, na.rm = FALSE)
  
  
  portfolioSignals <- buy.signals + short.signals
  
  
  
  df <- data.frame(returns, lag.xts(portfolioSignals, k = 1))
  
  
  equity <- array(NA, dim=length(df$daily.returns))
  equity[1] <- 100
  
  total.trades <- array(NA, dim = length(df$daily.returns))
  total.trades[1] <- 0
  
  
  for (i in 2:length(df$returns)) {
    
    if (identical(df$lag.xts.portfolioSignals..k...1.[i-1],  df$lag.xts.portfolioSignals..k...1.[i])) {
      
      total.trades[i] <- 0
      
      equity[i] <- equity[i-1] * df$returns[i] * df$lag.xts.portfolioSignals..k...1.[i]*leverage + equity[i-1] 
      
    } else {
      
      total.trades[i] <- 1
      
      equity[i] <- equity[i-1] * df$returns[i] * df$lag.xts.portfolioSignals..k...1.[i] *leverage + equity[i-1] - (equity[i-1] * feeRate/100 * leverage)
      
    }
    
    
  }
  
  
  #if (logEquity==TRUE) {
    
    
   # print(plot(log(equity), type="l", main = "eurgbp bollinger bands strategy"))
    
    
  #} else {
    
   # print(plot(equity, type="l", main = "eurgbp bollinger bands strategy"))
    
    
  #}
  
  
  
  print(cat("processing row:", bblookback, bbStdev, "\n"))
  print(equity[length(equity)])
  
  print(cat("total trades:",sum(total.trades) / 2))
  
  
  
  return(equity[length(equity)])
  
  
  
}


eurgbp.backtest(price_series = unclass(training_window$X5),bblookback = 60, bbStdev = 5, feeRate = 0.01)

#########################################################
#ottimizzazione del backtest



range_bb <- seq(from=180,to=1440,by=120)
range_stdev <- seq(from=3,to=6,by=1.5)
combinations <- expand.grid(range_bb, range_stdev)


library(future)

plan(strategy = 'multisession', workers = 12)



combinations$result <- future_mapply(eurgbp.backtest, bblookback=combinations$Var1, bbStdev=combinations$Var2,
                              leverage= 1, price_series = EURGBP[,5])




###

















































#versione con entrata multipla
eurgbp.backtest <- function(price_series,bblookback1=20, bbStdev1=2, bblookback2, bbStdev2, bblookback3, bbStdev3){
  
  returns1 <- ROC(price_series, n = 1)
  bands1 <- BBands(price_series, n = bblookback1, sd = bbStdev1, maType = "EMA")[,4]
  
  
  #first entry
  buy.signals1 <- ifelse(bands1 < 0, yes = 1, no = ifelse(bands1 >= 0.5, yes = 0, no = NA))
  short.signals1 <- ifelse(bands1 > 1, yes = -1, no = ifelse(bands1 <= 0.5, yes = 0, no= NA))
  
  buy.signals1[1] <- 0
  short.signals1[1] <- 0
  
  buy.signals1 <- na.locf(buy.signals1, na.rm = FALSE)
  short.signals1 <- na.locf(short.signals1, na.rm = FALSE)
  
  
  portfolioSignals1 <- buy.signals1 + short.signals1
  
  
  df1 <- data.frame(returns1, lag.xts(portfolioSignals1, k = 1))
  df1[1,2] <- 0
  strategy_returns1 <- df1[,1] * df1[,2]
  
  
  
  #second entry
  
  bands2 <- BBands(EURGBP[,5], n = bblookback2, sd = bbStdev2, maType = "EMA")[,4]
  buy.signals2 <- ifelse(bands2 < 0, yes = 1, no = ifelse(bands2 >= 0.5, yes = 0, no = NA))
  short.signals2 <- ifelse(bands2 > 1, yes = -1, no = ifelse(bands2 <= 0.5, yes = 0, no= NA))
  
  buy.signals2[1] <- 0
  short.signals2[1] <- 0
  
  buy.signals2 <- na.locf(buy.signals2, na.rm = FALSE)
  short.signals2 <- na.locf(short.signals2, na.rm = FALSE)
  
  
  portfolioSignals2 <- buy.signals2 + short.signals2
  
  
  df2 <- data.frame(returns1, lag.xts(portfolioSignals2, k = 1))
  df2[1,2] <- 0
  strategy_returns2 <- df2[,1] * df2[,2]
  
  
  #third entry
  bands3 <- BBands(EURGBP[,5], n = bblookback3, sd = bbStdev3, maType = "EMA")[,4]
  buy.signals3 <- ifelse(bands3 < 0, yes = 1, no = ifelse(bands3 >= 0.5, yes = 0, no = NA))
  short.signals3 <- ifelse(bands3 > 1, yes = -1, no = ifelse(bands3 <= 0.5, yes = 0, no= NA))
  
  buy.signals3[1] <- 0
  short.signals3[1] <- 0
  
  buy.signals3 <- na.locf(buy.signals3, na.rm = FALSE)
  short.signals3 <- na.locf(short.signals3, na.rm = FALSE)
  
  
  portfolioSignals3 <- buy.signals3 + short.signals3
  
  
  df3 <- data.frame(returns1, lag.xts(portfolioSignals3, k = 1))
  df3[1,2] <- 0
  strategy_returns3 <- df3[,1] * df3[,2]
  
  
  TOTAL_RETURNS <<- (strategy_returns1 + strategy_returns2 + strategy_returns3) / 3
  
  
  
  
  plot(cumprod(1+na.omit(TOTAL_RETURNS)), type= "l", main="Multiple entries backtest EURGBP")
  
  
  
}


#ho provato ad includerci le fees
eurgbp.backtest(price_series = EURGBP[,5],bblookback1 = 60, bbStdev1 = 5, bblookback2 = 60, 
                bbStdev2 = 6, bblookback3 = 60, bbStdev3 = 7)
                











