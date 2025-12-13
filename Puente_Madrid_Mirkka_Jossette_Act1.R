#Remover todas las variables creadas en environment so far
rm(list=ls())
setwd("C:/Users/User/Desktop/UNIR/Estadistica_R/Statistics_and_R")

#Install libraries
#install.packages(c("ggplot2", "dplyr", "BiocManager"))
#install.packages("BiocManager")
#BiocManager::install("dplyr")
#BiocManager::install("BiocManager")
#BiocManager::install("ggplot2")
#BiocManager::install("tidyr")

# Cargar libraries
library(BiocManager)
library(ggplot2)
library(dplyr)
library(tidyr)

### -------------- EJERCICIOS ----------------- ###
#7.	Crea las siguientes variables. 
#	Vector10. Esta variable debe ser un vector que almacene los diez primeros números enteros. 
# Indica 3 formas distintas de definir esta variable, explorando las funciones seq() y rep(). 

# Definir un vector que contenga los 10 primeros números enteros usando función c()
vector10_1 <- c(1:10)

# Definir un vector que contenga los 10 primeros números enteros usando función seq()
vector10_2 <- seq(1, 10)

# Definir un vector que contenga los 10 primeros números enteros usando función rep()
vector10_3 <- rep(1:10, times = 1)


# Vector3. Esta variable debe ser un vector que almacene una variable de tipo carácter, 
# una de tipo numérico y una de tipo Booleano. ¿De qué clases son los valores almacenados? 
# Ayuda: explorar la función class(). 

# Definir el vector
vector3 <- c("a", 36, FALSE)
# Clase de la variable
class(vector3) #clase "character"
# Clase de los valores
class(vector3[[1]]) #clase "character"
class(vector3[[2]]) #clase "character"
class(vector3[[3]]) #clase "character"

#	Lista3. Esta variable debe ser una lista que almacene una variable de tipo carácter, 
# una de tipo numérico y una de tipo Booleano. ¿De qué clases son los valores almacenados? 
# Ayuda: explorar la función class().

# Definir la lista
lista3 <- list("b", 45, TRUE)
# Clase de la variable 
class(lista3) # clase "list"
# Clase de los valores 
class(lista3[[1]]) # clase "character"
class(lista3[[2]]) # clase "numeric"
class(lista3[[3]]) # clase "logical"

# Matriz_Resultado. Esta matriz debe almacenar el resultado de sumar la matrizA y la matrizB 
# (ambas son matrices de 4 filas x 3 columnas).  

# Definir la matrices
matrizA <- matrix(data = seq(1, 12), nrow = 4, ncol = 3)
matrizB <- matrix(data = seq(1, 12), nrow = 4, ncol = 3)

# Resultado de sumar las matrices
matriz_Resultado <- matrizA + matrizB

# 8. Crea el vector vector_random que almacene 50 números aleatorios. 
# Ayuda: para obtener valores random puedes usar las funciones runif(), rnorm(), rexp(), sample().

# Definir vector con numeros random redondeados a dos decimales 
set.seed(345) # seed para que los resultados sean replicables
vector_random <- round(runif(50, min = -50, max = 50), 2)

# Encuentra el valor máximo y mínimo del vector. 
# Minimo
set.seed(345)
minimo <- min(vector_random) # resultado = -49.51
# Maximo
set.seed(345)
maximo <- max(vector_random) # resultado = 47.77

#	Calcula la media y la mediana.
# Media
set.seed(345)
media <- mean(vector_random) # resultado = 2.8804
# Mediana
set.seed(345)
mediana <- median(vector_random) # resultado = 6.455
 
#	Determina la desviación estándar.
set.seed(345)
desviacion_estandar <- sd(vector_random) # resultado = 30.72207

#	Ordena los valores del vector en orden ascendente y descendente.
# Orden ascendente
set.seed(345)
ascendente <- sort(vector_random)
# Orden descendente
set.seed(345)
descendente <- sort(vector_random, decreasing = TRUE)

#	Calcula la suma y el producto de todos los elementos del vector.
# Suma
set.seed(345)
suma <- sum(vector_random) #resultado = 144.02
# Producto
set.seed(345)
producto <- prod(vector_random) #resultado = -7.337073e+66

# 10.	Desarrolla, utilizando sentecias if – else if - else, un bloque de código que lea un 
# nucleótido ingresado por el usuario y determine si es Adenina (A), Timina (T), Citosina (C) 
# o Guanina (G). Si el nucleótido ingresado no es válido, debe informar al usuario del error.

# readline funciona para leer una linea desde la consola
nucleotido <- toupper(readline("Introduce un nucleótido: "))

if(nucleotido == "A"){ # Primer caso
  print("Adenina")
} else if(nucleotido == "T"){ # Segundo caso
  print("Timina")
} else if(nucleotido == "C"){ # Tercer caso
  print("Citosina")
} else if(nucleotido == "G"){ # Cuarto caso
  print("Guanina")
} else {
  print("El nucleótido ingresado no es válido") # Respuesta default
}

# 11.	Desarrolla, utilizando bucles for o while, un bloque de código que calcule la suma 
# de todos los números comprendidos entre 50 y 100 (ambos inclusive).

x <- 0  # variable global para guardar el resultado final
for(i in 50:100){
  x <- x + i  # suma de los numeros
}
print(paste("Suma:", x))

# 12.	Desarrolla, utilizando bucles for o while, un bloque de código que calcule la suma 
# y el promedio de todos los números pares comprendidos entre 1 y 50 (ambos inclusive). 
y <- 0 # variable global para guardar el resultado de la suma final
j <- 1 # primer valor de j para el while loop
par <- 0 # cuenta el numero de numeros pares
while (1 <= j && j <= 50) {
  if(j %% 2 == 0){  # es numero par?
    y <- y + j
    par <- par + 1  
  }
  j <- j + 1 # evita que el while loop se haga infinito
}
promedio <- y / par  # suma/numero de pares
print(paste("suma:", y))
print(paste("promedio:", promedio))

# 13.	Crea y utiliza una función llamada Deteccion_Nucleotido que realice el bloque de 
# código del Ejercicio 10. (*) Nota: Si no has podido resolver dicho ejercicio, y solo 
# en este caso, crea y utiliza la función que prefieras que demuestre que sabes trabajar 
# con funciones (ejemplo: Multiplicación o división de números dados por el usuario). 

Deteccion_Nucleotido <- function(nucleotido){
  
  nt <- toupper(nucleotido) # cambia la letra a mayuscula
  
  if(nt == "A"){ # Primer caso
    print("Adenina")
  } else if(nt == "T"){ # Segundo caso
    print("Timina")
  } else if(nt == "C"){ # Tercer caso
    print("Citosina")
  } else if(nt == "G"){ # Cuarto caso
    print("Guanina")
  } else {
    print("El nucleótido ingresado no es válido") # Respuesta default
  }
}
# Testeando la funcion
Deteccion_Nucleotido("T")
Deteccion_Nucleotido("A")
Deteccion_Nucleotido("G")
Deteccion_Nucleotido("C")
Deteccion_Nucleotido("a")
Deteccion_Nucleotido("s")

