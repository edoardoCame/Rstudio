library(quantmod)



#######################
### LINEAR REGRESSION ###
#######################

scatterPlot <- function(tic1, tic2, start.date, end.date=Sys.Date()){
  ticker1 <- na.approx(unclass(getSymbols.yahoo(tic1, from=start.date, to=end.date, auto.assign=FALSE)))
  ticker2 <- na.approx(unclass(getSymbols.yahoo(tic2, from=start.date, to=end.date, auto.assign=FALSE)))
  
  print(plot(x=ticker1[,4], y=ticker2[,4]))
  
  summary(lm(ticker1[,4] ~ ticker2[,4]))
}

scatterPlot(tic1="EURUSD=X", tic2 = "GBPUSD=X", start.date = "2008-01-01")

scatterPlot(tic1="BCH-USD", tic2="BTC-USD", start.date = "2023-01-01")


