

#Esercizi lezione 20 agosto

# Esercizio 1: Ciclo `for` per calcolare la somma dei primi 10 numeri interi
somma <- 0
for (i in 1:10) {
  somma <- somma + i
}
print(paste("Somma dei primi 10 numeri interi:", somma))




# Esercizio 2: Funzione per calcolare il fattoriale
fattoriale <- function(n) {
  risultato <- 1
  for (i in 1:n) {
    risultato <- risultato * i
  }
  return(risultato)
}

print(paste("Fattoriale di 5:", fattoriale(5)))







# Esercizio 3: Ciclo `for` per creare una sequenza di Fibonacci
fibonacci <- numeric(10)
fibonacci[1] <- 0
fibonacci[2] <- 1

for (i in 3:10) {
  fibonacci[i] <- fibonacci[i-1] + fibonacci[i-2]
}

print("Primi 10 numeri della sequenza di Fibonacci:")
print(fibonacci)











# Esercizio 5: Uso di `apply` per calcolare la somma delle righe
matrice <- matrix(1:9, nrow=3)
somma_righe <- apply(matrice, 1, sum)
print("Somma delle righe della matrice 3x3:")
print(somma_righe)



















# Esercizio 6: Uso di `lapply` per calcolare la lunghezza delle parole
parole <- list("R", "programming", "language")
lunghezze <- lapply(parole, nchar)
print("Lunghezza delle parole:")
print(lunghezze)








# Esercizio 7: Ciclo `for` per calcolare il prodotto degli elementi di un vettore
vettore <- c(2, 3, 4)
prodotto <- 1
for (i in vettore) {
  prodotto <- prodotto * i
}
print(paste("Prodotto degli elementi del vettore:", prodotto))






# Esercizio 8: Funzione per calcolare la media di una lista di numeri
media <- function(numeri) {
  return(sum(numeri) / length(numeri))
}


print(paste("Media della lista di numeri:", media(c(5, 10, 15))))





# Esercizio 9: Uso di `sapply` per arrotondare un vettore di numeri
numeri <- c(3.14159, 2.71828, 1.61803)
arrotondati <- sapply(numeri, round, 2)
print("Numeri arrotondati a 2 cifre decimali:")
print(arrotondati)








# Esercizio 10: Ciclo `for` per contare i numeri pari in un vettore
vettore <- c(1, 4, 7, 8, 10)
conta_pari <- 0
for (i in vettore) {
  if (i %% 2 == 0) {
    conta_pari <- conta_pari + 1
  }
}



print(paste("Numero di elementi pari nel vettore:", conta_pari))





# Esercizio 1: Uso di `apply` per calcolare la media delle colonne
matrice <- matrix(1:9, nrow=3)
media_colonne <- apply(matrice, 2, mean)
print("Media delle colonne della matrice:")
print(media_colonne)

# Esercizio 2: Uso di `lapply` per trovare il massimo di ciascun vettore in una lista
liste_vettori <- list(c(1, 5, 3), c(10, 2, 8), c(7, 4, 9))
massimi <- lapply(liste_vettori, max)
print("Massimi di ciascun vettore nella lista:")
print(massimi)

# Esercizio 3: Uso di `vapply` per calcolare la lunghezza delle stringhe
parole <- c("R", "è", "un", "linguaggio", "potente")
lunghezze <- vapply(parole, nchar, FUN.VALUE = numeric(1))
print("Lunghezza delle stringhe:")
print(lunghezze)

# Esercizio 4: Uso di `apply` per calcolare il prodotto delle righe
matrice <- matrix(1:9, nrow=3)
prodotto_righe <- apply(matrice, 1, prod)
print("Prodotto delle righe della matrice:")
print(prodotto_righe)

# Esercizio 5: Uso di `lapply` per elevare al quadrato ciascun elemento di una lista di numeri
numeri <- list(2, 3, 4, 5)
quadrati <- lapply(numeri, function(x) x^2)
print("Quadrato di ciascun numero nella lista:")
print(quadrati)

# Esercizio 6: Uso di `vapply` per calcolare il numero di caratteri in ciascun nome
nomi <- c("Anna", "Giovanni", "Luigi", "Paolo")
numero_caratteri <- vapply(nomi, nchar, FUN.VALUE = numeric(1))
print("Numero di caratteri in ciascun nome:")
print(numero_caratteri)

# Esercizio 7: Uso di `apply` per sommare gli elementi diagonali di una matrice
matrice <- matrix(1:16, nrow=4)
somma_diagonale <- apply(matrice, 2, function(x) sum(diag(x)))
print("Somma degli elementi diagonali:")
print(sum(diag(matrice)))

# Esercizio 8: Uso di `lapply` per normalizzare ciascun vettore in una lista
liste_vettori <- list(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9))
normalizzati <- lapply(liste_vettori, function(x) x / sum(x))
print("Vettori normalizzati:")
print(normalizzati)

# Esercizio 9: Uso di `vapply` per arrotondare un vettore di numeri a 3 cifre decimali
numeri <- c(3.14159, 2.71828, 1.61803)
arrotondati <- vapply(numeri, round, FUN.VALUE = numeric(1), digits = 3)
print("Numeri arrotondati a 3 cifre decimali:")
print(arrotondati)

# Esercizio 10: Uso di `apply` per calcolare la varianza delle colonne di una matrice
matrice <- matrix(rnorm(20), nrow=5)
varianza_colonne <- apply(matrice, 2, var)
print("Varianza delle colonne della matrice:")
print(varianza_colonne)




classifica_numeri <- function(vettore){
  
  classifica <- array(NA, length(vettore))
  for (i in 1:length(vettore)){
    if (vettore[i] > 0){
      classifica[i] <- "Positivo"
    } else if (vettore[i] < 0){
      classifica[i] <- "Negativo"
    } else {
      classifica[i] <- "Zero"
    }
    
  }
  return(classifica)
}

classifica_numeri(vettore = c(1, -2, 0, 3, -4))











