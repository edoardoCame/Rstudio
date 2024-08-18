
library(PerformanceAnalytics)
library(quantmod)



simbolo <- getSymbols.yahoo(Symbols = 'XLP', env = .GlobalEnv, 
                            from = '1999-01-01', auto.assign=FALSE)

candela.positiva <- ifelse(Cl(simbolo)>Op(simbolo), sqrt((Hi(simbolo)-Cl(simbolo))^2) / sqrt((Lo(simbolo)-Op(simbolo))^2), 0 )
candela.negativa <- ifelse(Cl(simbolo)<Op(simbolo), sqrt((Hi(simbolo)-Op(simbolo))^2) / sqrt((Lo(simbolo)-Cl(simbolo))^2), 0 )
ritorni.osservati <- candela.positiva + candela.negativa
ritorni <- dailyReturn(Cl(simbolo))

long <- ifelse(lag(ritorni.osservati, 1) > 1 , ritorni, 0)

in_out <- ifelse(lag(ritorni.osservati, 1) > 1 , 1, 0)
nTrades <- ifelse(lag(in_out) == in_out, yes = 0, no = 1)
sum(na.omit(nTrades))

charts.RollingPerformance(long, width = 30)





charts.PerformanceSummary(long)
maxDrawdown(long)



charts.PerformanceSummary(ritorni)
maxDrawdown(ritorni)



