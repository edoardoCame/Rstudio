
# ==============================================
# 1. Quiz di sintassi R: Vero o Falso
# ==============================================
# Q1) In R, le variabili possono iniziare con un numero. Vero o Falso? __
# Q2) L'operatore <- è utilizzato per l'assegnazione di valori. Vero o Falso? __
# Q3) Il commento in R inizia con // . Vero o Falso? __
# Q4) La funzione print() mostra valori sulla console. Vero o Falso? __
# Q5) Una lista in R può contenere elementi di tipo diverso. Vero o Falso? __
# Q6) Una matrice in R può avere colonne di lunghezza diversa. Vero o Falso? __
# Q7) Il simbolo %*% serve per il prodotto matrice-matrice. Vero o Falso? __
# Q8) L'operatore && valuta vettori elemento per elemento. Vero o Falso? __
# Q9) Una data.frame è una lista di vettori della stessa lunghezza. Vero o Falso? __
# Q10) La funzione seq() genera sequenze di numeri. Vero o Falso? __

# ==================================================
# 2. Esercizi pratici: metti in pratica la sintassi R
# ==================================================
# 1) Crea un vettore numerico chiamato numeri contenente i numeri da 1 a 10.
# 2) Calcola la somma dei valori nel vettore numeri.
# 3) Crea una lista chiamata info con elementi nome = "Mario", eta = 30, voti = c(8, 9, 7).
# 4) Estrai l'elemento eta dalla lista info.
# 5) Definisci una funzione sommaQuadrati(x, y) che restituisce x^2 + y^2.
# 6) Esegui sommaQuadrati(3, 4) e salva il risultato in res.
# 7) Crea un vettore character giorni contenente i nomi dei giorni della settimana.
# 8) Seleziona il terzo elemento del vettore giorni.
# 9) Crea un factor chiamato votazioni a partire da c(6,7,8,9,10).
# 10) Visualizza la struttura (str) del factor votazioni.
# 11) Crea una matrice 3x3 chiamata mat con i numeri da 1 a 9.
# 12) Calcola la trasposizione della matrice mat.
# 13) Crea un data.frame df con colonne id = 1:5 e score = c(10,20,30,40,50).
# 14) Aggiungi al data.frame df una colonna is_even che vale TRUE se score è pari.
# 15) Filtra df per mantenere solo le righe con is_even == TRUE.
# 16) Crea un vettore logico gt5 che indica se gli elementi di numeri sono > 5.
# 17) Usa un blocco if-else per stampare "Grande" se x > 5, altrimenti "Piccolo" (x=7).
# 18) Usa la funzione length() sul vettore numeri per ottenere il numero di elementi.
# 19) Visualizza i nomi delle colonne del data.frame df usando names() o colnames().
# 20) Usa lapply su una lista list("a","bc","def") per ottenere la lunghezza di ogni elemento.
# 21) Usa sapply su un vettore 1:5 per calcolare la radice quadrata di ogni elemento.
# 22) Imposta set.seed(123) e crea un vettore rnd di 100 numeri casuali da distribuzione normale.
# 23) Calcola la media e la deviazione standard di rnd.
# 24) Ordina rnd in ordine crescente.
# 25) Rimuovi i duplicati da un vettore v <- c(1,2,2,3,3,4).
# 26) Crea una matrice 2x5 con fill by row usando matrix(1:10, nrow=2, byrow=TRUE).
# 27) Usa apply su mat (3x3) per sommare le righe.
# 28) Crea due data.frame d1 e d2 e uniscili con merge su una colonna comune.
# 29) Usa read.csv("data.csv") per importare dati (supponi che il file esista).
# 30) Usa write.csv(df, "output.csv") per esportare il data.frame df.

# ==============================================
# Soluzioni
# ==============================================
# 1. Quiz di sintassi R: Risposte Vero/Falso
# Q1) Falso
# Q2) Vero
# Q3) Falso
# Q4) Vero
# Q5) Vero
# Q6) Falso
# Q7) Vero
# Q8) Falso
# Q9) Vero
# Q10) Vero

# 2. Esercizi pratici: soluzioni
# 1)
numeri <- 1:10
# 2)
sum(numeri)
# 3)
info <- list(nome = "Mario", eta = 30, voti = c(8, 9, 7))
# 4)
info$eta
# 5)
sommaQuadrati <- function(x, y) { x^2 + y^2 }
# 6)
res <- sommaQuadrati(3, 4)
# 7)
giorni <- c("lunedì", "martedì", "mercoledì", "giovedì", "venerdì", "sabato", "domenica")
# 8)
giorni[3]
# 9)
votazioni <- factor(c(6, 7, 8, 9, 10))
# 10)
str(votazioni)
# 11)
mat <- matrix(1:9, nrow = 3, ncol = 3)
# 12)
t(mat)
# 13)
df <- data.frame(id = 1:5, score = c(10,20,30,40,50))
# 14)
df$is_even <- df$score %% 2 == 0
# 15)
df_even <- df[df$is_even, ]
# 16)
gt5 <- numeri > 5
# 17)
x <- 7
if (x > 5) print("Grande") else print("Piccolo")
# 18)
length(numeri)
# 19)
names(df)  # oppure colnames(df)
# 20)
lapply(list("a", "bc", "def"), nchar)
# 21)
sapply(1:5, sqrt)
# 22)
set.seed(123)
rnd <- rnorm(100)
# 23)
mean(rnd)
sd(rnd)
# 24)
sort(rnd)
# 25)
v <- c(1,2,2,3,3,4)
unique(v)
# 26)
mat2 <- matrix(1:10, nrow = 2, byrow = TRUE)
# 27)
apply(mat, 1, sum)
# 28)
d1 <- data.frame(id = 1:3, value = c("a","b","c"))
d2 <- data.frame(id = c(2,3,4), score = c(10,20,30))
merge(d1, d2, by = "id")
# 29)
data <- read.csv("data.csv")
# 30)
write.csv(df, "output.csv")
