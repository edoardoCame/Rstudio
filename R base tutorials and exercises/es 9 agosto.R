

#Esercizi per 9 agosto



#Es 1:
# Lista di vettori di lunghezza variabile
lst_var <- list(a = c(1, 2, 3), b = c(4, 5, 6, 7), c = c(8, 9))

# Usa vapply per calcolare la varianza di ogni vettore:



#Es 2:
# Lista di vettori
lst <- list(a = 1:5, b = 6:10, c = 11:15)

# Usa lapply per calcolare la media di ogni vettore


#Es 3:
# Lista di vettori
lst <- list(a = 1:5, b = 6:10, c = 11:15)

# Usa vapply per calcolare il massimo e il minimo di ogni vettore


#Es 4:
# Matrice di esempio
mat <- matrix(1:20, nrow = 4, ncol = 5)

# Usa apply per calcolare il prodotto e la somma di ogni riga







#------------------------------------------------------



#SOLUZIONI:

#es 1:
vapply(lst_var, var, numeric(1))

#Es 2:
lapply(lst, mean)


#Es 3:
vapply(lst, function(x){ c(max = max(x), min = min(x), mean= mean(x))}, numeric(3)) #il numeric (2)
#serve per specificare che il risultato è un vettore numerico di lunghezza 2            


#Es 4:
apply(mat, 1, function(x){ c(prod = prod(x), sum = sum(x))})




