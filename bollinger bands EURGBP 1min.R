library(quantmod)
library(TTR)
library(readxl)
library(tseries)
library(future)
library(future.apply)
library(roll)



setwd("~/Desktop/EURGBP TESTS")

########################
#LO RENDO UNA FUNZIONE
########################



library(readr)
EURGBP <- read_delim("EURGBP 2023.csv", delim = ";", 
                     escape_double = FALSE, col_names = FALSE, 
                     trim_ws = TRUE)




library(readr)
EURGBP <- read_delim("EURGBP 2024.csv", delim = ";", 
                     escape_double = FALSE, col_names = FALSE, 
                     trim_ws = TRUE)

EURGBPchart <- xts(EURGBP[,-1], order.by = EURGBP$X1)



#importo i dati di DukasCopy
#--------------------------------------------------------------------------------
library(readr)
EURGBP <- read_delim("~/Downloads/EURGBP1.csv", 
                     delim = "\t", escape_double = FALSE, 
                     col_names = FALSE, trim_ws = TRUE)



merged_data <- EURGBP
rm(EURGBP)
colnames(merged_data) <- c('Time', 'Open', 'High', 'Low', 'Close', 'Volume')
merged_data$Time <- as.POSIXlt(merged_data$Time, format = "%Y-%m-%d %H:%M:%S")
merged_data$Returns <- ROC(merged_data$Close)
#--------------------------------------------------------------------------------



#creo i filtri:
#--------------

rolling_sd <- roll_sd(merged_data$Close, width = 43200)
rolling_sd[1] <- 0
rolling_sd <- na.locf(rolling_sd)
merged_data$FilterSd <- ifelse(rolling_sd < 0.01, yes = TRUE, no = FALSE)
merged_data$FilterHour <- ifelse(merged_data$Time$hour >= 0 & merged_data$Time$hour < 12, yes = FALSE, no = TRUE)
filters <- merged_data$FilterHour & merged_data$FilterSd

#sum(filters) / length(filters)




eurgbp.backtest <- function(bblookback=20, bbStdev=2, leverage=1, feeRate=0.01, logEquity=FALSE, plotCharts=FALSE){
  
  
  returns <- merged_data$Returns #in caso sono i dataset sovrastimati di 100!!
  
  bands <- BBands(merged_data$Close, n = bblookback, sd = bbStdev, maType = "EMA")[,4]
  
  
  
  buy.signals <- ifelse(bands < 0, yes = 1, no = ifelse(bands >= 0.5, yes = 0, no = NA))
  
  short.signals <- ifelse(bands > 1, yes = -1, no = ifelse(bands <= 0.5, yes = 0, no= NA))
  
  
  buy.signals[1] <- 0
  short.signals[1] <- 0
  
  buy.signals <- na.locf(buy.signals, na.rm = FALSE)
  short.signals <- na.locf(short.signals, na.rm = FALSE)
  
  
  portfolioSignals <- (buy.signals + short.signals)
  
  
  
  df <- data.frame(returns, lag.xts(portfolioSignals, k = 1))
  
  
  equity <- array(NA, dim=length(df$daily.returns))
  equity[1] <- 100
  
  total.trades <- array(NA, dim = length(df$daily.returns))
  total.trades[1] <- 0
  
  
  for (i in 2:length(df$returns)) {
    
    if (filters[i]==TRUE) {
      
      if (identical(df$lag.xts.portfolioSignals..k...1.[i-1],  df$lag.xts.portfolioSignals..k...1.[i])) {
      
      total.trades[i] <- 0
      
      equity[i] <- equity[i-1] * df$returns[i] * df$lag.xts.portfolioSignals..k...1.[i]*leverage + equity[i-1] 
      
    } else {
      
      total.trades[i] <- 1
      
      equity[i] <- equity[i-1] * df$returns[i] * df$lag.xts.portfolioSignals..k...1.[i] *leverage + equity[i-1] - (equity[i-1] * feeRate/100 * leverage)
      
    }
      
    } else {
      
      equity[i] <- equity[i-1]
      total.trades[i] <- 0
    }
    
    
    
  }
  
  if (plotCharts==TRUE) {
    
    
  if (logEquity==TRUE) {
    
    
    print(plot(log(equity), type="l", main = "eurgbp bollinger bands strategy"))
    
    
    } else {
    
      print(plot(equity, type="l", main = "eurgbp bollinger bands strategy"))
    
    
    }
  
    }
  
  #risultato.equity <<- equity
  
  print(cat("processing row:", bblookback, bbStdev, "\n"))
  print(equity[length(equity)])
  
  print(cat("total trades:",sum(total.trades) / 2))
  
  
  
  return(equity[length(equity)])
  
  
  
}


#ho provato ad includerci le fees
eurgbp.backtest(bblookback = 60, bbStdev =5, leverage = 10, feeRate = 0.01, 
                logEquity = FALSE, plotCharts = TRUE)




#col filtro per la bassa volatilità guadagna mooolto di più....
#backtest dal 2007 ad oggi no fees sdev 1 con alta volatilità
#faceva il 1.47% di ciò che faceva col filtro per la bassa volatilità...
#differenza massiva












#########################################################
#ottimizzazione del backtest

range_bb <- seq(from=30,to=240,by=30)
range_stdev <- seq(from=3,to=6,by=1)
combinations <- expand.grid(range_bb, range_stdev)

library(future)
library(future.apply)


plan("multisession", workers = 4)
options(future.globals.maxSize= 891289600)

combinations$result <- future_mapply(eurgbp.backtest, bblookback=combinations$Var1, bbStdev=combinations$Var2,
                              leverage= 1, feeRate=0.01, logEquity=FALSE, plotCharts=FALSE)



plan('sequential')

#provo a plottare qualcosa con plotly
#FUNZIONA!
library(plotly)

plot_ly(data = combinations, x= ~combinations$Var1, y=~combinations$Var2, 
        z=~combinations$result, type="scatter3d")


plot_ly(y=~combinations$result, x=~combinations$Var1)


library(openxlsx)

write.xlsx(combinations, file = "combinazioni per EURGBP 1 min 2023.xlsx")


rm(list=ls())



save.image("~/Desktop/EURGBP TESTS/optimization results in days.RData")






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



