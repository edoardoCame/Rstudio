



#ESERICZI IFELSE:


#Es 1:

#Scrivi un codice R che utilizza ifelse per determinare se 
#l'utente è minorenne (sotto i 18 anni) o maggiorenne (18 anni o più). 
#Stampa un messaggio appropriato per ciascun caso.

eta <- 20

#continua tu



#Es 2:

#Crea un vettore di voti (numeri interi) e utilizza ifelse per 
#determinare se ciascun voto è sufficiente (>= 6) o insufficiente (< 6). 
#Stampa un vettore con i risultati ("Sufficiente" o "Insufficiente").



#Es 3:


#Scrivi una funzione in R che prende un vettore di numeri come 
#input e utilizza ifelse per determinare se ciascun numero è pari 
#o dispari. La funzione deve restituire un vettore con i risultati 
#("Pari" o "Dispari").


#Ricordati come si definisce se qualcosa è pari o dispari con i %%...
#Controlla il file dell'ultima lezione!


















#SOLUZIONI:

#Es 1:
eta <- 20 #o valore a piacimento
risultato <- ifelse(eta < 18, "Sei minorenne.", "Sei maggiorenne.")

# Stampa il risultato
print(risultato)





#Es 2:

# Vettore di voti degli studenti
voti <- c(4, 7, 5, 8, 6, 3, 9, 5)

# Determina se i voti sono sufficienti o insufficienti
risultati <- ifelse(voti >= 6, "Sufficiente", "Insufficiente")

# Stampa i risultati
print(risultati)



#Oppure con un ciclo for:

risultati <- array(NA, length(voti))

for (i in 1:length(voti)) {
  
  if (voti[i] >= 6) {
    risultati[i] <- "Sufficiente"
  } else {
    risultati[i] <- "Insufficiente"
  }
}


print(risultati)










fibonacci <- array(NA, 20)
fibonacci[1] <- 0
fibonacci[2] <- 1


for (i in 3:20) {
  fibonacci[i] <- fibonacci[i - 1] + fibonacci[i - 2]
}

plot(fibonacci, type = "l", main = "Successione di Fibonacci", 
     xlab = "n", ylab = "Fibonacci(n)")





#Prima iterazione:
#i = 3
fibonacci[3] <- fibonacci[3-1] + fibonacci[3-2]

#Seconda iterazione:
#i = 4
fibonacci[4] <- fibonacci[4-1] + fibonacci[4-2]


#Terza iterazione:
#i = 5
fibonacci[5] <- fibonacci[5-1] + fibonacci[5-2]


#...

#Ottava iterazione:
#i = 10
fibonacci[10] <- fibonacci[10-1] + fibonacci[10-2]

#

































#Es 3:

# Funzione per determinare se i numeri sono pari o dispari
parita <- function(numeri) {
  ifelse(numeri %% 2 == 0, "Pari", "Dispari")
}

# Vettore di numeri
numeri <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

# Applicazione della funzione e stampa dei risultati
risultati <- parita(numeri)
print(risultati)



