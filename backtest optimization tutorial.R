library(tidyr)





# Definisci la tua funzione
my_function <- function(x, y, z) {
  return(x + y + z)
}

# Inizializza un vettore per i valori di x, y e z
values <- seq(1, 10, by = 2)

# Utilizza un ciclo for per iterare attraverso i valori e chiamare la tua funzione
for (i in values) {
  result <- my_function(i, i, i)
  cat("Input:", i, i, i, "  Result:", result, "\n")
}



range_x <- seq(1, 10, by = 2)
range_y <- seq(1, 10, by = 2)
range_z <- seq(1, 10, by = 2)


combinations <- expand.grid(range_x, range_y, range_z)


combinations$result <- mapply(my_function, x=combinations$Var1, y=combinations$Var2, z=combinations$Var3)


rm(list=ls())
