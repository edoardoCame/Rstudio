


#Esercizi lezione 2 luglio


#Esercizio 1:
#crea una funzione che crea una random walk
#1. Crea una funzione che genera una random walk
#2. Applica la funzione
#3. Fai un plot del tutto








#Esercizio 2: crea una media mobile su un vettore generato dalla random_walk
#1. usa la funzione random_walk creata in precedenza
#2. Crea una funzione che calcoli la media mobile di un vettore
#3. Applica la funzione al vettore generato
#4. Fai un plot del tutto



#Esercizio 3:
#Scrivi una funzione somma_pari che utilizzi un ciclo for 
#per sommare tutti i numeri pari da 1 a 100 e restituisca il risultato. 
#Usa la struttura if per verificare se un numero è pari.

somma_pari <- function() {
  # il tuo codice qui
}

somma_pari()


#Esercizio 4:
#Scrivi una funzione fattoriale che accetti un numero intero n 
#e utilizzi un ciclo for per calcolare il fattoriale di n. La funzione
#deve restituire il risultato.

fattoriale <- function(n) {
  # il tuo codice qui
}

fattoriale(5)  # Dovrebbe restituire 120


#Esercizio 5:
#Scrivi una funzione classifica_numero che prenda un numero come input e
#restituisca "Positivo", "Negativo" o "Zero" usando la funzione ifelse.

classifica_numero <- function(x) {
  # il tuo codice qui
}

classifica_numero(5)    # Dovrebbe restituire "Positivo"
classifica_numero(-3)   # Dovrebbe restituire "Negativo"
classifica_numero(0)    # Dovrebbe restituire "Zero"


#Esercizio 6:
#Scrivi una funzione trova_massimo che prenda un vettore numerico 
#come input e utilizzi un ciclo for per trovare il valore massimo nel vettore.


trova_massimo <- function(vettore) {
  # il tuo codice qui
}

trova_massimo(c(3, 5, 7, 2, 9, 1))  



#Esercizio 7:
#Scrivi una funzione filtra_e_trasforma che accetti
#un vettore di numeri come input e segua i seguenti passaggi:

#1) Filtra i numeri pari dal vettore.
#2) Moltiplica ciascun numero pari per 2.
#3) Restituisci un nuovo vettore contenente i numeri trasformati.





















#SOLUZIONI:
#-----------------------------------------------




#Esercizio 1:
random_walk <- function(n){
  walk <- array(100, n)
  for(i in 2:n){
    walk[i] <- walk[i-1] + rnorm(1)
  }
  return(walk)
}

price <- random_walk(100)
plot(price,type='l')




#Esercizio 2:


#1. usa il vettore della random walk
vettore <- random_walk(200)


#2. Crea una funzione che calcoli la media mobile di un vettore
media_mobile <- function(vettore, window = 5){
  n <- length(vettore)
  media <- numeric(n)
  
  for(i in 1:n){
    
    if(i < window){
      
      media[i] <- mean(vettore[1:i])
      
    } else {
      
      media[i] <- mean(vettore[ (i-window+1) : i ] )
    }
  }
  return(media)
}


sma <- media_mobile(vettore, window = 5)
plot(vettore, type='l', col='red')
lines(sma, type='l', col='blue')



vettore <- 1:20 #media mobile 5
print(vettore)


vettore[3:5]




#Esercizio 3:
somma_pari <- function() {
  somma <- 0
  for (i in 1:100) {
    if (i %% 2 == 0) {
      somma <- somma + i
    }
  }
  return(somma)
}

somma_pari() 




#Esercizio 4:
fattoriale <- function(n) {
  risultato <- 1
  for (i in 1:n) {
    risultato <- risultato * i
  }
  return(risultato)
}

fattoriale(5)  


#Esercizio 5:
classifica_numero <- function(x) {
  ifelse(x > 0, "Positivo", ifelse(x < 0, "Negativo", "Zero"))
}

classifica_numero(0)
classifica_numero(5)

#Esercizio 6:
trova_massimo <- function(vettore) {
  massimo <- vettore[1]
  for (i in 2:length(vettore)) {
    if (vettore[i] > massimo) {
      massimo <- vettore[i]
    }
  }
  return(massimo)
}

trova_massimo(c(3, 5, 7, 2, 9, 1))



#Esercizio 7:

filtra_e_trasforma <- function(vettore) {
  # Filtro dei numeri pari
  numeri_pari <- vettore[vettore %% 2 == 0]
  
  # Trasformazione: moltiplicazione per 2
  numeri_trasformati <- numeri_pari * 2
  
  return(numeri_trasformati)
}

# Esempio di utilizzo
filtra_e_trasforma(c(1, 2, 3, 4, 5, 6))  # Dovrebbe restituire c(4, 8, 12)






