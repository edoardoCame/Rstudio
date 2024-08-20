library(quantmod)

vettore1 <- round(runif(n=200, min=1, max=10), digits = 0)
vettore2 <- round(runif(n=200, min=1, max=10), digits = 0)

lr <- lm(vettore2 ~ vettore1) 

beta <- lr$coefficients[2]



vettore3 <- vettore2 - vettore1 * beta

plot(vettore3, type="l")



df <- data.frame(vettore1, vettore2, vettore3, betaV1=beta)
dfReturns <- data.frame(returnsV1=ROC(vettore1), returnsV2=ROC(vettore2), returnsV3=ROC(vettore3))


#beta = 0.1113172


#I RITORNI DIVERGONO! calcolare il ROC dello spread NON è la stessa cosa di calcolare
#i ritorni sui ticker singoli!