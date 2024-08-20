

#Esercizi cicli for per 27 luglio

#Es1
#Consegna: Scrivi un ciclo for che stampa la tabellina del 5,
#ossia i primi 10 multipli di 5. (ovvero 5*1, 5*2, 5*3 ecc...)


for (... in ...) { #che valori deve assumere la i?
  print(5 * ...)
}







#Es2
#Consegna: Data una lista di numeri 
#numeri <- c(3, -1, -4, 2, 7, -6, 0, -3), scrivi un ciclo for 
#che conta quanti numeri negativi ci sono nella lista e stampa 
#il risultato.


#Completa gli spazi rimanenti: ricordati, che valore assume la i?
numeri <- c(3, -1, -4, 2, 7, -6, 0, -3)
conta_negativi <- 0

for (... in ...) {
  if (... < 0) {
    conta_negativi <- ... + 1
  }
}





#Es3
#Consegna: Scrivi un ciclo for che calcola e stampa le potenze di 2 
#dalla potenza 0 fino alla potenza 10, (cioè 2^0, 2^1, 2^2, 2^3, ecc...)



for (... in ...) { #che valori deve assumere la i?
  print(2^...)
}





#Es 4
#Consegna: Scrivi un ciclo for che stampa tutti i 
#numeri dispari da 1 a 20.

for (... in ...) { #che valori deve assumere la i?
  if (... %% 2 != 0) { #il != significa NON uguale, è l'opposto di ==
    print(...)
  }
}


#Es5
#Consegna: Dato un vettore di numeri: numeri <- c(2, 3, 5, 7, 11),
#scrivi un ciclo for che calcola il prodotto di tutti gli elementi
#del vettore e stampa il risultato.

numeri <- c(2, 3, 5, 7, 11)

prodotto <- ... #da dove deve partire la produttoria?

for (... in ...) {
  prodotto <- prodotto * ...
}



#Es 6
#Scrivi un codice in R che prende un vettore contenente
#alcuni valori numerici e alcuni valori NA. Utilizza ifelse
#per sostituire i valori NA con la media dei valori non-NA 
#presenti nel vettore. Stampa il vettore risultante.

valori <- c(2, 4, NA, 7, 5, NA, 10)

media_valori <- mean(valori, na.rm = TRUE) #ti ricordi cos'è na.rm?
                                           #guarda nell'help

valori_sostituiti <- ifelse(is.na(...), ..., ...) #is.na è un comando:
                                       #restituisce TRUE se il valore è NA
                                       #restituisce FALSE se il valore non è NA
                                       #guarda tu nell'help

print(valori_sostituiti)


#Es 7
#Scrivi una funzione in R chiamata calcola_area_cerchio che prenda come input il 
#raggio di un cerchio e restituisca l'area del cerchio. Utilizza la formula:
#area = pigreco * raggio^2. Stampa l'area di un cerchio con raggio 5.


calcola_area_cerchio <- function(...) { #cosa accetta in input?
  ...           #cosa deve restituirmi?
}


area <- calcola_area_cerchio(... = 5) #uso la funzione





#----------------------------------------------
#SOLUZIONI
#Es 1
for (i in 1:10) {
  print(5 * i)
}


#Es 2
numeri <- c(3, -1, -4, 2, 7, -6, 0, -3)
conta_negativi <- 0

for (numero in numeri) {
  if (numero < 0) {
    conta_negativi <- conta_negativi + 1
  }
}

print(conta_negativi)


#Es 3
for (i in 0:10) {
  print(2^i)
}



#Es 4
for (i in 1:20) {
  if (i %% 2 != 0) {
    print(i)
  }
}


#Es 5
numeri <- c(2, 3, 5, 7, 11)
prodotto <- 1

for (numero in numeri) {
  prodotto <- prodotto * numero
}

print(prodotto)


#Es 6
valori_sostituiti <- ifelse(is.na(valori), media_valori, valori)
print(valori_sostituiti)

#Es 7
calcola_area_cerchio <- function(raggio) {
  area <- pi * raggio^2
  return(area)
}

# Test della funzione
raggio_test <- 5
area <- calcola_area_cerchio(raggio_test)

# Stampa dell'area
print(area)





