library(quantmod)
library(urca)
library(PerformanceAnalytics)
library(tseries)
library(readxl)


ticker1 <- getSymbols.yahoo("EWA", from="2006-04-26", to="2012-04-09", env=globalenv())
ticker2 <- getSymbols.yahoo("EWC", from="2006-04-26", to="2012-04-09", env=globalenv())
ticker3 <- getSymbols.yahoo("IGE", from="2006-04-26", to="2012-04-09", env=globalenv())


ewaAdj = unclass(EWA$EWA.Adjusted)
ewcAdj = unclass(EWC$EWC.Adjusted)
igeAdj = unclass(IGE$IGE.Adjusted)


jotest=ca.jo(data.frame(ewaAdj,ewcAdj,igeAdj), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)


#APPLICO GLI EIGENVECTORS AI TICKER PER OTTENERE LO SPREAD
eigenvalues <- jotest@V[,1]
m_tickers <- cbind(ewaAdj, ewcAdj, igeAdj)

#Utilizzo sweep, mi applica ad ogni riga di ogni colonna la funzione *
#utilizzando i valori che indico in STATS

s <- sweep(x = m_tickers, MARGIN = 2, STATS = eigenvalues, FUN = "*")

y <- apply(s, MARGIN = 1, FUN = sum)



plot(s, type="l")

adf.test(s, k = 1)

#questo è corretto e viene come l'articolo che replica le cose di chan su R
#notare che il lag K=2 è sbagliato perchè su matlab puoi mettere (come fa chan) un lag=1 quindi sballa
#i risultati, sarebbe utile capire come mettere un lag = 1 ma non penso sia possibile su R.






#TESTIAMOLO SU ETHBTC nel 1m timeframe:



BTCUSDT <- read_excel("candle_data_multi_1m.xlsx", sheet = "BTCUSDT")
ETHUSDT <- read_excel("candle_data_multi_1m.xlsx", sheet = "ETHUSDT")

df <- data.frame(BTCUSDT[,5], ETHUSDT[,5])
colnames(df) <- c("btc", "eth")


jotest=ca.jo(df, type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

z <- df$btc * 1 - df$eth * 21.72334
plot(z, type="l")

adf.test(z, k=1)

##########################
#test sul daily timeframe:


ticker1 <- getSymbols.yahoo("ETH-USD", from="2018-01-01", to=Sys.Date(), env=globalenv())
ticker2 <- getSymbols.yahoo("BTC-USD", from="2018-01-01", to=Sys.Date(), env=globalenv())

df <- data.frame(`ETH-USD`[,4], `BTC-USD`[,4])

jotest=ca.jo(df, type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

z <- df$ETH.USD.Close * 1 - df$BTC.USD.Close * 0.06907662 
plot(z, type="l")

adf.test(z)


##################################
#TEST SULLE 4 GOLDEN STOCKS
##################################


start.date <- "2011-01-01"

ticker1 <- na.approx(getSymbols("TXG.TO", from= start.date, to=Sys.Date(), auto.assign = F))
ticker2 <- na.approx(getSymbols("CXB.TO", from=start.date, to=Sys.Date(), auto.assign = F))
ticker3 <- na.approx(getSymbols("OGC.TO", from=start.date, to=Sys.Date(), auto.assign = F))
ticker4 <- na.approx(getSymbols("VGCX.TO", from=start.date, to=Sys.Date(), auto.assign = F))

tic1Adj = unclass(ticker1[,6])
tic2Adj = unclass(ticker2[,6])
tic3Adj = unclass(ticker3[,6])
tic4Adj = unclass(ticker4[,6])


jotest=ca.jo(data.frame(tic1Adj, tic2Adj, tic3Adj, tic4Adj), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

z <- tic1Adj * 1 + tic2Adj * -5.47783386 + tic3Adj * -1.40961207 + tic4Adj *  0.06668099  
plot(z, type="l", ylab = "Spread fra le 3 stocks")
adf.test(z, k = 1)




##################################################################################################



#qua sotto voglio costruire la funzione johansen che mi fa il test 
#indipendentemente dal numero di ticker



multiple_tickers_CA.JO <- function(start.date, tickers){
  
  
  getSymbols(Symbols = tickers, from=start.date)
  
  
  #lui con questa funzione prende i ticker 1 ad 1 ed estrae la 6a colonna
  get.Column <- function(z){
    x <- get(z)
    x[,6]
  }
  
  
  symbollist <- na.approx(data.frame(lapply(tickers, get.Column)))
  
  # <<- symbollist #uso la doppia freccia per assegnare al globalenv il dataframe
  
  jotest=ca.jo(x = symbollist,type="trace", K=2, ecdet="none", spec="longrun")
  eigenvalues <- jotest@V[,1]
  s <- sweep(x = symbollist, MARGIN = 2, STATS = eigenvalues, FUN = "*")
  y <- apply(s, MARGIN = 1, FUN = sum)
  print(plot(y, type="l", ylab="spread fra ticker"))
  summary(jotest)
  adf.test(y, k=1)
}


multiple_tickers_CA.JO(start.date="2011-01-01", tickers = c("TXG.TO", "CXB.TO", "OGC.TO", "VGCX.TO"))


multiple_tickers_CA.JO(start.date="2011-01-01", tickers = c("TXG.TO", "OGC.TO", "VGCX.TO"))

multiple_tickers_CA.JO(start.date = "2008-01-01", tickers = c("EURUSD=X", "GBPUSD=X"))



multiple_tickers_CA.JO(start.date = "2004-01-01", tickers = c("EURUSD=X", "GBPUSD=X"))







