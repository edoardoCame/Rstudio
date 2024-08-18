library(quantmod)
library(PerformanceAnalytics)
library(tseries)
library(urca)
library(zoo)
library(roll)



##################################################
#FUNZIONE PER CALCOLARE L'HEDGE RATIO "OTTIMALE"##
##################################################

optimal.hedgeAdj <- function(start.date, end.date=Sys.Date(),tic1,tic2,timeframe="daily", title){
  ticker1 <- na.approx(getSymbols.yahoo(tic1, env = globalenv(), from=start.date, to=end.date, auto.assign=FALSE, periodicity = timeframe))
  ticker2 <- na.approx(getSymbols.yahoo(tic2, env = globalenv(), from=start.date, to=end.date, auto.assign=FALSE, periodicity = timeframe))
  
  lr <- lm(ticker2[,6] ~ ticker1[,6]) #usa i prezzi adjusted close!
  
  beta <- lr$coefficients[2]
  
  #zeta <- lr$residuals[,1] #questi sono i residuals che 
                           #sono identici all'hedge ratio di chan, NON usarli
  
  
  
  hedge.ratio <- (ticker2[,6] - beta * ticker1[,6])
  
  print(plot(hedge.ratio, type="l"))
  print(adf.test(hedge.ratio, k=1))
}


#viene identico al libro di chan 
optimal.hedgeAdj(start.date = "2006-04-26", end.date = "2012-04-09", tic1 = "EWA", tic2 = "EWC")

optimal.hedgeAdj(start.date = "2007-01-01", end.date = Sys.Date(), tic1 = "GLD", tic2 = "GDX")





optimal.hedgeNORMAL <- function(start.date, end.date=Sys.Date(),tic1,tic2,timeframe="daily", title){
  ticker1 <- na.approx(getSymbols.yahoo(tic1, env = globalenv(), from=start.date, to=end.date, auto.assign=FALSE, periodicity = timeframe))
  ticker2 <- na.approx(getSymbols.yahoo(tic2, env = globalenv(), from=start.date, to=end.date, auto.assign=FALSE, periodicity = timeframe))
  
  lr <- lm(ticker2[,4] ~ ticker1[,4]) #usa le close normali
  
  beta <- lr$coefficients[2]
  
  #zeta <- lr$residuals[,1] #questi sono i residuals che 
  #sono identici all'hedge ratio di chan, NON usarli
  
  
  
  hedge.ratio <- (ticker2[,4] - beta * ticker1[,4])
  
  print(plot(hedge.ratio, type="l"))
  print(adf.test(hedge.ratio, k=1))
}



optimal.hedgeNORMAL(start.date="2018-01-01", tic1="BTC-USD", tic2="ETH-USD")






#####################################################################






#test di Johansen

johansen.test.3tickers <- function(tic1,tic2,tic3,start.date="2005-01-01",end.date=Sys.Date(),timeframe="daily"){
  ticker1 <- na.approx(getSymbols.yahoo(tic1, env = globalenv(), from=start.date, to=end.date, auto.assign=FALSE, periodicity = timeframe))
  ticker2 <- na.approx(getSymbols.yahoo(tic2, env = globalenv(), from=start.date, to=end.date, auto.assign=FALSE, periodicity = timeframe))
  ticker3 <- na.approx(getSymbols.yahoo(tic3, env = globalenv(), from=start.date, to=end.date, auto.assign=FALSE, periodicity = timeframe))
  #ticker4 <- na.approx(getSymbols.yahoo(tic4, env = globalenv(), from=start.date, to=end.date, auto.assign=FALSE, periodicity = timeframe))
  
  
  df <- data.frame(ticker1[,4], ticker2[,4], ticker3[,4])
  test <- ca.jo(x = df, type = "trace", K = 2, spec = "transitory")
  return(summary(test))
}


johansen.test.3tickers(tic1 = "EWA", tic2 = "EWC", tic3 = "IGE",start.date = "2006-04-26", end.date = "2012-01-01")

#################################################
#ROLLING HEDGE RATIO TEST
#################################################
rolling.hedge <- function(tic1,tic2,start.date,end.date=Sys.Date(),lookback=20,bands=FALSE, title="spread"){
  
  #importo i ticker da Yahoo Finance
  ticker1 <- log(na.approx(getSymbols.yahoo(Symbols = tic1, from=start.date, to = end.date, auto.assign=FALSE)))
  ticker2 <- log(na.approx(getSymbols.yahoo(Symbols = tic2, from=start.date, to = end.date, auto.assign=FALSE)))
  
  #calcolo la regressione lineare ed estraggo il coefficiente per calcolare l'hedge ratio adattivo
  df <- roll_lm(x= ticker1[,4], y = ticker2[,4], width = lookback)
  hedge.dynamic <-(ticker2[,4] - df$coefficients[,2] * ticker1[,4])
  
  #riporto l'esponente di Hurst per indicare la stazionarietà dell'hedge ratio adattivo
  print(HurstIndex(hedge.dynamic))
  
  #plotto l'hedge ratio adattivo + le sue bande di bollinger se richiesto dall'utente
  print(plot(hedge.dynamic, type="l", main=title))
  if (bands==TRUE) {
    bbands <- BBands(hedge.dynamic, n=10, sd=2)
    lines(bbands[,1], col="red")
    lines(bbands[,3], col="red")
  }
}



rolling.hedge(tic1 = "BTC-USD", tic2 = "LTC-USD", start.date = "2018-01-01", lookback = 20)





rolling.hedge(tic1="GLD", tic2="USO", start.date = "2006-05-24", 
              end.date = "2012-04-09", lookback = 20, bands = FALSE)

rolling.hedge(tic1="GLD", tic2="GDX", start.date = "2007-01-01", 
              end.date = Sys.Date(), lookback = 20, title="Hedge Ratio fra EWA ed EWC")

rolling.hedge(tic1 = "EURUSD=X", tic2 = "GBPUSD=X", start.date = "2004-01-01")

rolling.hedge(tic1 = "AUDUSD=X", tic2 = "NZDUSD=X", start.date = "2008-01-01", lookback = 10)



rolling.hedge(tic1 = "QQQ", tic2 = "ONEQ", start.date = "2004-01-01", lookback = 10)

#johansen test###########################

johansen.test.2tickers <- function(tic1,tic2,start.date="2018-01-01",end.date=Sys.Date()){

  ticker1 <- na.approx(getSymbols.yahoo(Symbols = tic1, from=start.date, to = Sys.Date(), auto.assign=FALSE))
  ticker2 <- na.approx(getSymbols.yahoo(Symbols = tic2, from=start.date, to = Sys.Date(), auto.assign=FALSE))

  df <- data.frame(ticker1[,4], ticker2[,4])
  test <- ca.jo(x = df, type = "eigen", K = 2, spec = "transitory")
summary(test)
}


johansen.test.2tickers(tic1="EWA", tic2="EWC", start.date = "2007-01-01")

#########################################



#HALF LIFE OF MEAN REVERSION COMPUTATION#
y <- na.approx(getSymbols("EURUSD=X", auto.assign=F)[,4])
y.lag <- lag(y, 1)
halflifedf <- cbind(y,y.lag)[-1,] #remove the first column
regression.results <- lm(halflifedf[,1] ~ halflifedf[,2])
lambda <- summary(regression.results)$coefficients[2]
half.life <- -log(2)/lambda

