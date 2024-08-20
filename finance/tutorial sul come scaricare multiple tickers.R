



#tutorial su come scaricare e unire in un dataframe più tickers contemporaneamente


start.date <- "2015-01-01"
tickers <- c("TXG.TO", "CXB.TO", "OGC.TO", "VGCX.TO")

na.approx(getSymbols(Symbols = tickers, from=start.date))


#lui con questa funzione prende i ticker 1 ad 1 ed estrae la 6a colonna
get.Column <- function(z){
  x <- get(z)
  x[,6]
}

#applichiamo tramite lista la funzione get column a tutti i ticker (i singoli simboli 
#verranno sostituiti a "z" e ad ognuno verrà estratta la 6a colonna che verrà riportata nella lista).

symbollist <- data.frame(lapply(tickers, get.Column))


#lapply returns a list of the same length as X, each 
#element of which is the result of applying FUN to the corresponding element of X.

