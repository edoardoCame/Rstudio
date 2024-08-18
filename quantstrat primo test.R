

library(quantstrat)
library(PerformanceAnalytics)
library(tseries)
library(pracma)
library(zoo)
library(roll)
library(urca)
library(quantmod)
library(readr)
library(tseries)
library(parallel)
library(readr)



merged_data <- read_delim("Desktop/merge data/merged_data.csv", 
                          delim = ";", escape_double = FALSE, col_names = FALSE, 
                          trim_ws = TRUE)

library(readr)
EURGBP <- read_delim("~/Desktop/EURGBP TESTS/EURGBP 2023.csv", 
                     delim = ";", escape_double = FALSE, col_names = FALSE, 
                     trim_ws = TRUE)

colnames(EURGBP) <- c("Time", "Open", 'High', 'Low', 'Close', 'Volume')

EURGBP <- xts(EURGBP[,-1], order.by = EURGBP$Time)



#setup the strategy
#-----------------------
initdate <-"2023-01-01"
from <-"2023-01-01"
to <- Sys.Date()

# Set system environment timezone:
Sys.setenv(TZ ="UTC")
# Set currency (we’ll use USD for now):
currency("USD")

tradesize <- 100
initeq <- 100

strategy.st <- portfolio.st <- account.st <-"firststrat"

.blotter <- new.env()
.strategy <- new.env()

initPortf(portfolio.st, symbols = "EURGBP", initDate = initdate)

initAcct(name = account.st, portfolios = portfolio.st, initDate = initdate, 
         currency = "USD", initEq = initeq)
initOrders(portfolio.st, initDate = initdate)

data("stratBBands")

strategy(stratBBands$name, stratBBands, store = TRUE)



out <- applyStrategy("stratBBands", portfolios = portfolio.st, mktdata = EURGBP, 
                     parameters = list(n=180, sd=5))


updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)

chart.Posn(portfolio.st)























