library(quantmod)
library(TTR)
library(readxl)
library(tseries)
library(future)
library(future.apply)




setwd("~/Desktop/EURUSD tests")

########################
#LO RENDO UNA FUNZIONE
########################

#devo comunque importare i dati prima

library(readr)
EURUSD <- read_delim("DAT_ASCII_EURUSD_M1_2023.csv", 
                     delim = ";", escape_double = FALSE, col_names = FALSE, 
                     trim_ws = TRUE)




eurusd.backtest <- function(bblookback=20, bbStdev=2, leverage=1, feeRate=0.0001, logEquity=FALSE, plotCharts=FALSE){
  
  
  returns <- ROC(EURUSD[,5], n = 1)
  
  bands <- BBands(EURUSD[,5], n = bblookback, sd = bbStdev, maType = "EMA")[,4]
  
  
  
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
  
  if (plotCharts==TRUE) {
    
  if (logEquity==TRUE) {
    
    
    print(plot(log(equity), type="l", main = "eurusd bollinger bands strategy"))
    
    
  } else {
    
    print(plot(equity, type="l", main = "eurusd bollinger bands strategy"))
    
    
  }
  
    
  }
  
  
  
  print(cat("processing row:", bblookback, bbStdev, "\n"))
  print(equity[length(equity)])
  
  print(cat("total trades:",sum(total.trades) / 2))
  
  
  
  return(equity[length(equity)])
  
  
  
}


#ho provato ad includerci le fees
eurusd.backtest(bblookback = 10, bbStdev = 5, leverage = 1, feeRate = 0.008, 
                logEquity = FALSE, plotCharts = TRUE)












#########################################################
#ottimizzazione del backtest

range_bb <- seq(from=60,to=1440,by=60)
range_stdev <- seq(from=1,to=8,by=1)
combinations <- expand.grid(range_bb, range_stdev)



library(future)
library(future.apply)
plan(strategy = "multisession", workers = 14)
combinations$result <- future_mapply(eurusd.backtest, bblookback=combinations$Var1, bbStdev=combinations$Var2,
                              leverage= 1, feeRate=0.01, plotCharts=FALSE)


plan("sequential")






#provo a plottare qualcosa con plotly
#FUNZIONA!
library(plotly)

plot_ly(data = combinations, x= ~combinations$Var1, y=~combinations$Var2, 
        z=~combinations$result, type="scatter3d")


plot_ly(y=~combinations$result, x=~combinations$Var1)


library(openxlsx)

write.xlsx(combinations, file = "combinazioni per EURGBP 1 min 2023.xlsx")


rm(list=ls())







#------------------------
#Bootstrapping di EURGBP
#------------------------

returns <- ROC(EURGBP[,5], n = 1)
returns[1] <- 0




#Calculate sample returns
#------------------------
sample_returns <- function(N, returns) {
  samples <- matrix(NA, nrow = length(returns), ncol = N)
  for (i in 1:N) {
    samples[, i] <- sample(returns, size = length(returns), replace = TRUE)
  }
  colnames(samples) <- paste0("Sample", 1:N)
  return(as.data.frame(samples))
}


returns_samples <- sample_returns(N = 50, returns = returns)


#Compute new random prices
#-------------------------
sample_prices <- apply(X = returns_samples, MARGIN = 2, FUN = function(x){(cumprod(1+x))})
sample_prices <- as.data.frame(sample_prices)




#plotto tutte le simulazioni
matplot(sample_prices, type = "l", xlab = "Time",
        ylab = "Sample Prices", main= paste(ncol(sample_prices), "samples"))


















rm(EURGBP)
gc()















sample.backtest <- function(price_series, bblookback=20, bbStdev=2, leverage=1){
  
  
  
  bands <- BBands(price_series, n = bblookback, sd = bbStdev, maType = "EMA")[,4]
  returns <- ROC(price_series)
  returns[1] <- 0
  
  buy.signals <- ifelse(bands < 0, yes = 1, no = ifelse(bands >= 0.5, yes = 0, no = NA))
  
  short.signals <- ifelse(bands > 1, yes = -1, no = ifelse(bands <= 0.5, yes = 0, no= NA))
  
  
  buy.signals[1] <- 0
  short.signals[1] <- 0
  
  buy.signals <- na.locf(buy.signals, na.rm = FALSE)
  short.signals <- na.locf(short.signals, na.rm = FALSE)
  
  
  portfolioSignals <- buy.signals + short.signals
  
  
  
  df <- data.frame(returns, lag.xts(portfolioSignals, k = 1))
  df[1,2] <- 0
  
  total_returns <- df[,1] * df[,2]
  
  equity <- cumprod(1+total_returns)
  
  print(plot(equity, type="l"))
  
  print(equity[length(equity)])
  
  return(equity[length(equity)])
  
  
  
}


plot(sample_prices$Sample10, type="l")

sample.backtest(price_series = sample_prices$Sample11, bblookback = 30, 
                bbStdev = 1)

combinations <- apply(X = sample_prices, MARGIN = 2, 
                      FUN = sample.backtest, bblookback=60, bbStdev=2)

combinations <- as.data.frame(combinations)

percent_positive <- sum(combinations$combinations > 0) / nrow(combinations)
print(percent_positive)




save.image("~/Desktop/EURGBP TESTS/sampling data.RData")



