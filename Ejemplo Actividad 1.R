## ----------------------------------------------------------
## Creación de una base de datos simulada con variables
## antropométricas, bioquímicas y de expresión génica
## ----------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(pheatmap)

set.seed(123)  ## Para reproducibilidad

N <- 200  ## tamaño muestral

data_sim <- tibble(
  sexo = sample(c("Hombre", "Mujer"), N, replace = TRUE),
  
  ## Variables antropométricas
  peso = rnorm(N, mean = 70, sd = 12),
  estatura = rnorm(N, mean = 1.70, sd = 0.10),
  porcentaje_gc = rnorm(N, mean = 25, sd = 8),  ## % grasa corporal
  
  ## Variables bioquímicas (con distr. no normal)
  glucosa = rlnorm(N, meanlog = log(85), sdlog = 0.25),
  colesterol = rnorm(N, mean = 190, sd = 35),
  trigliceridos = rlnorm(N, meanlog = log(120), sdlog = 0.40) ,
  
  ## Expresión génica simulada: 10 genes × N individuos
  GEN1 = rnorm(N, 0, 1),
  GEN2 = rnorm(N, 0, 1),
  GEN3 = rnorm(N, 0, 1),
  GEN4 = rnorm(N, 0, 1),
  GEN5 = rnorm(N, 0, 1),
  GEN6 = rnorm(N, 0, 1),
  GEN7 = rnorm(N, 0, 1),
  GEN8 = rnorm(N, 0, 1),
  GEN9 = rnorm(N, 0, 1),
  GEN10 = rnorm(N, 0, 1)
)

head(data_sim)

## ----------------------------------------------------------
## BOXPLOTS para peso, estatura y %GC en función del sexo
## ----------------------------------------------------------

g1 <- ggplot(data_sim, aes(x = sexo, y = peso, fill = sexo)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Boxplot de peso según sexo") +
  theme_minimal()

g2 <- ggplot(data_sim, aes(x = sexo, y = estatura, fill = sexo)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Boxplot de estatura según sexo") +
  theme_minimal()

g3 <- ggplot(data_sim, aes(x = sexo, y = porcentaje_gc, fill = sexo)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Boxplot de % grasa corporal según sexo") +
  theme_minimal()

## Unir los 3 gráficos con patchwork
(g1 | g2 | g3)

## Los boxplots permiten comparar rápidamente la mediana y la 
## dispersión entre "Hombre" y "Mujer".
## También permiten identificar valores extremos ("outliers").
##
## En variables antropométricas es habitual observar:
## - Peso más alto en hombres
## - %GC más alto en mujeres
## - Estatura mayor en hombres

## ----------------------------------------------------------
## Histogramas bioquímicos: glucosa, colesterol, triglicéridos
## ----------------------------------------------------------

h1 <- ggplot(data_sim, aes(glucosa)) +
  geom_histogram(bins = 25, fill = "steelblue", alpha = 0.7) +
  labs(title = "Histograma de glucosa") +
  theme_minimal()

h2 <- ggplot(data_sim, aes(colesterol)) +
  geom_histogram(bins = 25, fill = "darkorange", alpha = 0.7) +
  labs(title = "Histograma de colesterol") +
  theme_minimal()

h3 <- ggplot(data_sim, aes(trigliceridos)) +
  geom_histogram(bins = 25, fill = "darkgreen", alpha = 0.7) +
  labs(title = "Histograma de triglicéridos") +
  theme_minimal()

(h1 | h2 | h3)

## La glucosa y los triglicéridos suelen presentar distribuciones 
## asimétricas (positivamente sesgadas). Por eso hemos usado 
## una distribución log-normal simulada.
##
## El colesterol total, sin embargo, suele aproximarse más 
## a una distribución normal.
##
## Estos histogramas permiten al alumno identificar si es adecuado 
## usar pruebas paramétricas (t-test) o no paramétricas (Wilcoxon).


## ----------------------------------------------------------
## Heatmap de expresión génica simulada
## ----------------------------------------------------------

matriz_genes <- as.matrix(data_sim %>% select(starts_with("GEN")))

## Crear nombres de individuos
rownames(matriz_genes) <- paste0("ID_", 1:nrow(matriz_genes))

## Heatmap usando pheatmap
pheatmap(
  matriz_genes,
  scale = "row",   ## centramos por gen
  clustering_method = "ward.D2",
  show_rownames = FALSE,
  main = "Heatmap de expresión génica simulada"
)

## En un heatmap de expresión génica solemos:
## - Escalar por fila (cada gen) para ver patrones relativos
## - Aplicar clustering jerárquico para agrupar genes o individuos
## - Interpretar colores como "sobreexpresión" o "infraexpresión"
##
## En esta simulación, los genes siguen distribuciones normales, 
## pero aun así podemos visualizar zonas donde los valores se 
## agrupan creando patrones aparentes.
