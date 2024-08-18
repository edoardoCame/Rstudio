library(quantmod)
library(PerformanceAnalytics)
library(tseries)

backtestMom.hedge <- function(startDate="2005-01-01",mom.period = 7,tic1,tic2){
ticker1 <- getSymbols.yahoo(tic1, from=startDate, to=Sys.Date(), env = globalenv(), auto.assign=FALSE)
ticker2 <- getSymbols.yahoo(tic2, from=startDate, to=Sys.Date(), env = globalenv(), auto.assign=FALSE)
hedge1 <- ticker1/ticker2
roc.indicator <- ROC(hedge1[,4], n = mom.period, na.pad = TRUE)
hedge1.returns <- dailyReturn(hedge1)

df <- data.frame(hedge1.returns, lag.xts(roc.indicator,k = 1))[(mom.period+2):length(roc.indicator),]
colnames(df) <- c("hedge1 returns", "ROC")

signals <- ifelse(df$ROC>0, df$`hedge1 returns`, df$`hedge1 returns`*-1)

equity <- array(NA, dim=length(signals))
equity[1] <- 100
for (i in 2:length(signals)) {
  equity[i] <- (equity[i-1] * signals[i]) + equity[i-1]
}

plot(equity, type="l")
}


backtestMom.hedge(startDate = "2018-01-02",tic1 = "QQQ", 
            tic2 = "SPY", mom.period = 7)
