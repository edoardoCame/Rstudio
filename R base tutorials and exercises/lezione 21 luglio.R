






numeri <- c(1,5,2,3,6,7,10)
lunghezza <- length(numeri)
somma <- 0

for (i in 1:length(numeri)) {
  
  somma <- somma + numeri[i]
  print(somma)
}

print(somma)
sum(numeri)



#logica manuale
somma <- somma + numeri[1]
somma <- somma + numeri[2]
somma <- somma + numeri[3]
somma <- somma + numeri[4]

#









vettore_nomi <- c("Anna", "Marco", "Luca", "Giulia", "Francesco")


i <- "Anna" #prima iterazione
i <- "Marco" #seconda iterazione
i <- "Luca" #terza iterazione
i <- "Giulia" #ecc
i <- "Francesco"

print(paste("è nella lista:",i))

for (i in vettore_nomi) {
  print(paste(i, "è nella lista"))
}

#














#Lezione 21 luglio

#Esercizio 1
normdens<-function(x, m=0,s=1){
  # Nota: pi in R è la costante pi-greco 3.14....
  # Lo si può verificare digitando pi nella console
  1 / sqrt(2*pi*s^2) * exp( -(x-m)^2 / (2*s^2) )
} #creo la funzione matematica


x<-seq(-5, 10, by=0.1) #creo un vettore di valori da -5 a 10 con passo 0.1

y<-array(data = NA, dim = length(x)) #creo un vettore di NA con lunghezza 
                                     #pari a quella di x


#Creo un ciclo for per usare la sequenza x per calcolare
#i valori della funzione normdens.
#Praticamente voglio che ad ogni iterazione, la funzione normdens
#utilizzi un valore di x per calcolare il valore corrispondente di y
#e lo salvi nella posizione corrispondente di y.
for(i in 1:length(y)){
  y[i] <- normdens(x[i], m=2, s=1)
}

#Disegno il grafico
plot(x = x, y = y, type='l', col='lightblue', lwd=2, main='')
grid(lwd=2)






#--------------------------------------------------------------
#ESERCIZI SUI CICLI FOR:

#Esempio di ciclo for: per ogni iterazione in un vettore da 1 a 10,
#calcolare il quadrato del valore corrispondente e printarlo.

for(i in 1:10) { # Head of for-loop
  
  x1 <- i^2     # Code block
  print(x1)     # Print results
}


#Altro esempio:
x2 <- c("Max", "Tina", "Lindsey", "Anton", "Sharon")       
for(i in x2) {                                             
  print(paste("Hello", i, "how are you?"))  #ricordiamoci il comando paste()               
}

#Cosa succede eseguendo questo codice? la i dentro il comando paste() nell'
#ultima riga assume il valore (in questo caso assume il testo) di ogni elemento
#del vettore x2.


#Altro esempio dove va riempito l'array:
x3 <- array(data = NA, dim = 10) #creo un vettore di NA con lunghezza 10

for(i in 1:10) {                                           
  
  x3[i] <- i^2 #in OGNI posizione di x3, RIEMPIO x3 con i^2. Cos'è i
               #in questo caso? è l'indice dell'array x3, e assume valori
               #da 1 a 10. (siccome l'ho specificato io nella prima riga del ciclo
               #for, deve andare da 1 a 10, (1:10)).
}



#--------------------------------------------------
#ESERCIZI PER STUDIO INDIVIDUALE:


#Esercizi introduttivi:

#Es 1:
#Consegna: Scrivi un ciclo for che stampa i numeri da 1 a 10, ciascuno su una nuova riga.








#Es 2:
#Consegna: Scrivi un ciclo for che calcola la somma dei numeri pari da 1 a 20 e stampa il risultato.







#Es 3:
#Consegna: Data la seguente lista di nomi: nomi <- c("Anna", "Marco", "Luca", "Giulia", "Francesco"), 
#scrivi un ciclo for che stampa ciascun nome seguito dal messaggio "è nella lista".







#Es 4:
#Consegna: Scrivi un ciclo for che calcola il fattoriale di un numero intero
#positivo dato (ad esempio, 5). Il fattoriale di un numero n è il prodotto di tutti i numeri
#interi positivi da 1 a n. Ad esempio, il fattoriale di 5 è 5*4*3*2*1 = 120.











#Esercizi intermedi:

#Es 5:
#Consegna: Scrivi un ciclo for che calcola la media dei valori in un vettore di numeri. 
#Ad esempio, dato il vettore numeri <- c(4, 8, 15, 16, 23, 42), 
#calcola la media dei suoi elementi.








#Es 6:
#Crea una sequenza di fibonacci:
#Creare una sequenza di Fibonacci significa generare una serie di numeri in cui ogni 
#numero è la somma dei due numeri precedenti, a partire da due numeri iniziali definiti. 
#Tradizionalmente, la sequenza di Fibonacci inizia con 0 e 1.

#per costruire una sequenza di Fibonacci, si inizia con i due numeri iniziali (0 e 1) 
#e si continua aggiungendo ogni volta i due numeri più recenti della sequenza per ottenere il numero successivo.

#Esempio:
#0, 1, (0+1)=1, (1+1)=2, (1+2)=3, (2+3)=5, 8, 13, 21, 34, ...





















#-------------------------------
#SOLUZIONI AGLI ESERCIZI:


#Es 1:
for (i in 1:10) {
  print(i)
}


#Es 2:
somma_pari <- 0
for (i in 1:20) {
  if (i %% 2 == 0) { #se il resto della divisione di i per 2 è 0
    somma_pari <- somma_pari + i
  }
}
print(somma_pari)



#Es 3:
nomi <- c("Anna", "Marco", "Luca", "Giulia", "Francesco")
for (nome in nomi) {
  print(paste(nome, "è nella lista"))
}






#Es 4:
numero <- 5
fattoriale <- 1

for (i in 1:numero) {
  fattoriale <- fattoriale * i
  print(fattoriale)
}


#logica manuale:
fattoriale <- 1 * 1 #prima iterazione
print(fattoriale)

fattoriale <- 1 * 2 #seconda iterazione
print(fattoriale)

fattoriale <- 2 * 3 #terza iterazione
print(fattoriale)

fattoriale <- 6 * 4 #quarta iterazione
print(fattoriale)

fattoriale <- 24 * 5 #quinta iterazione
print(fattoriale)





#Es 5:
numeri <- c(4, 8, 15, 16, 23, 42)
somma <- 0
n <- length(numeri)


for (i in 1:n) {
  somma <- somma + numeri[i]
}

media <- somma / n
print(media)


#Es 6:
fibonacci <- array(NA, dim = 10)
fibonacci[1] <- 0
fibonacci[2] <- 1

for (i in 3:10) {
  fibonacci[i] <- fibonacci[i-1] + fibonacci[i-2]
}

print(fibonacci)






















