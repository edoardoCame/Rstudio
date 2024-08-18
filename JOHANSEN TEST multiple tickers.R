
library(quantmod)
library(urca)
library(PerformanceAnalytics)
library(tseries)
library(readxl)




multiple_tickers_CA.JO <- function(start.date, tickers){
  
  
  getSymbols(Symbols = tickers, from=start.date) #download data
  
  
  get.Column <- function(z){
    x <- get(z)
    na.approx(x[,6])
  }
  
  symbollist <- na.approx(data.frame(lapply(tickers, get.Column)))
  

    
  jotest=ca.jo(x = symbollist,type="trace", K=2, ecdet="none", spec="longrun")
  eigenvalues <- jotest@V[,1]
  s <- sweep(x = symbollist, MARGIN = 2, STATS = eigenvalues, FUN = "*")
  y <- apply(s, MARGIN = 1, FUN = sum)
  print(plot(y, type="l", ylab="hedge ratio of tickers"))
  print(summary(jotest))
  adf.test(y, k=1)
}


multiple_tickers_CA.JO(start.date="2011-01-01", tickers = c("TXG.TO", "CXB.TO", 
                                                            "OGC.TO", "VGCX.TO"))     










