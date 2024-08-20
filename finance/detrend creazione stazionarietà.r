library(pracma)
library(tseries)
library(quantmod)

getDifferences <- function(tic, lperiod=1,start.date="2010-01-01", end.date=Sys.Date()){
  
  #uso i prezzi log per ridurre la varianza
  ticker1 <- log(na.approx(getSymbols.yahoo(tic, from=start.date, to=end.date, auto.assign=FALSE)))
  
  test1 <- na.omit(diff(ticker1[,4], lag = lperiod))
  print(plot(test1, type="l", main=tic, lwd=1))
  hurstexp(test1)
}

#interessante osservare come con lag alti ci siano livelli sotto i quali non scende mai
getDifferences(tic="EURGBP=X", start.date = "2005-01-01", lperiod = 1)



getDifferences(tic="ETH-BTC", start.date = "2018-01-01")

getDifferences(tic="EURCHF=X", start.date = "2023-01-01")

getDifferences(tic="QQQ")
