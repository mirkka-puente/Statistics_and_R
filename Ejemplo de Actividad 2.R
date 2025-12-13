############################################################
# ACTIVIDAD 2 – Versión con variables NUTRICIONALES
# tipo_dieta + nivel_actividad
# gtsummary + media/SD o mediana/IQR según normalidad
############################################################

rm(list = ls())
set.seed(123)

#install.packages(c("gtsummary", "nortest"))
library(dplyr)
library(gtsummary)
library(nortest)

#-----------------------------------------------------------
# 0) SIMULACIÓN DEL DATASET
#-----------------------------------------------------------

n <- 65

tipo_dieta <- sample(
  c("Omnívora", "Vegetariana", "Vegana", "unknown"),
  n, replace=TRUE, prob=c(.40,.35,.20,.05)
)

nivel_actividad <- sample(
  c("Sedentario","Moderado","Activo","unknown"),
  n, replace=TRUE, prob=c(.30,.45,.20,.05)
)

edad <- round(rnorm(n, 60, 10))

genes <- c("AQ_actb", paste0("AQ_gene",1:38), "AQ_casp3")

mat_genes <- matrix(NA, n, length(genes))
colnames(mat_genes) <- genes

for(j in seq_along(genes)){
  if(j <= 20) mat_genes[,j] <- rnorm(n, 8+j/10, 1+j/50)
  else        mat_genes[,j] <- rlnorm(n, 2+j/30, 0.4)
}

df <- data.frame(tipo_dieta, nivel_actividad, edad, mat_genes)

# Introducir algunos NA
set.seed(444)
idx <- sample(1:(n * length(genes)), 10)
gmat <- as.matrix(df[, genes]); gmat[idx] <- NA
df[, genes] <- gmat

#-----------------------------------------------------------
# 1) ELIMINAR UNKNOWN
#-----------------------------------------------------------

df <- df %>% 
  filter(tipo_dieta != "unknown", nivel_actividad != "unknown")

df$tipo_dieta     <- droplevels(df$tipo_dieta)
df$nivel_actividad <- droplevels(df$nivel_actividad)

#-----------------------------------------------------------
# 2) NORMALIDAD DE LOS GENES
#-----------------------------------------------------------

normalidad <- sapply(genes, function(g){
  x <- df[[g]]; x <- x[!is.na(x)]
  if(length(x) > 50){
    p <- ad.test(x)$p.value
  } else {
    p <- shapiro.test(x)$p.value
  }
  return(p >= 0.05)   # TRUE = normal
})

stat_list <- lapply(normalidad, function(is_normal){
  if(is_normal) "{mean} ({sd})"
  else          "{median} ({p25}–{p75})"
})
names(stat_list) <- genes

#-----------------------------------------------------------
# 3) TABLA MODELO 2 (tipo_dieta → nivel_actividad)
#-----------------------------------------------------------

tabla_modelo2 <- df %>%
  select(tipo_dieta, nivel_actividad, all_of(genes)) %>%
  tbl_strata(
    strata = tipo_dieta,
    .tbl_fun = ~ .x %>%
      tbl_summary(
        by = nivel_actividad,
        statistic = stat_list,
        digits = all_continuous() ~ 1
      ) %>%
      add_p(
        test = all_continuous() ~ "kruskal.test",
        pvalue_fun = ~ style_pvalue(.x, digits = 3)
      )
  )

tabla_modelo2


#-----------------------------------------------------------
# 4) TABLA MODELO 3 (edad categorizada)
#-----------------------------------------------------------

p50 <- median(df$edad)

df <- df %>%
  mutate(edad_cat = ifelse(edad < p50, "< P50", "≥ P50") %>% factor())

tabla_modelo3 <- df %>%
  select(edad_cat, all_of(genes)) %>%
  tbl_summary(
    by = edad_cat,
    statistic = stat_list,
    digits = all_continuous() ~ 1
  ) %>%
  add_p(
    test = all_continuous() ~ "wilcox.test",
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  )

tabla_modelo3


#-----------------------------------------------------------
# 5) TABLA MODELO 1 (normalidad)
#-----------------------------------------------------------

tabla_modelo1 <- data.frame(
  Variable = genes,
  Test_utilizado = ifelse(nrow(df) > 50, "Anderson-Darling","Shapiro-Wilk"),
  Valor_p = sapply(genes, function(g){
    x <- df[[g]]; x <- x[!is.na(x)]
    if(length(x) > 50) ad.test(x)$p.value else shapiro.test(x)$p.value
  }),
  Interpretación = ifelse(normalidad, "Compatible con normalidad", "No normal")
)

head(tabla_modelo1)
