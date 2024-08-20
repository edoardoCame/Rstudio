library(quantmod)
library(dplyr)
library(PerformanceAnalytics)
library(roll)


PFE <- getSymbols.yahoo(Symbols = 'PFE', env = .GlobalEnv, 
                        from = '2000-01-01', auto.assign=FALSE)[,1:4]



PFE$Returns <- dailyReturn(PFE)


pfe_diff <- diff(PFE$PFE.Close)

upper_bb <- BBands(pfe_diff, n = 20, sd = 2)[,3]

PFE$Filter <- ifelse(lag.xts(PFE$PFE.Open, k = 1) < lag.xts(PFE$PFE.Open, k = 2), 
                     yes = PFE$Returns, no = 0)

charts.PerformanceSummary(PFE$Filter)


charts.PerformanceSummary(PFE$Returns)

pacf(PFE$Returns)


hist(PFE$Returns, breaks = 100)

#rolling sd sui ritorni
#-----------------------


rolling_sd <- roll_sd(PFE$Returns, width = 30)
plot(rolling_sd, type= 'l')

filter <- ifelse(lag(rolling_sd, n = 1) > lag(rolling_sd, n = 2), yes = PFE$Returns, no = 0)

charts.PerformanceSummary(filter)







