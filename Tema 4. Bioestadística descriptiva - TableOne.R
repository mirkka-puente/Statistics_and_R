############################################################
### PRÁCTICA: DESCRIPCIÓN DE UNA MUESTRA CON TABLEONE    ###
### Contexto: Pacientes en una consulta de riesgo CV     ###
############################################################

### Instalación (solo la primera vez)
# install.packages("tableone")
# install.packages("tidyverse")   ### incluye dplyr y ggplot2

### Cargar librerías
library(tidyverse)   ### dplyr (manipulación) + ggplot2 (gráficos)
library(tableone)    ### tablas descriptivas tipo Table 1


############################################################
### 1. SIMULACIÓN DE UNA BASE DE DATOS BIOMÉDICA        ###
############################################################

set.seed(1234)  ### Semilla para reproducibilidad

### Número de pacientes
n <- 300

### Simulación de variables en data.frame
datos <- data.frame(
  id   = 1:n,
  
  ### Sexo biológico
  sexo = sample(c("Mujer", "Hombre"), size = n, replace = TRUE, prob = c(0.55, 0.45))
)

### Añadimos el resto de variables usando dplyr y convertimos de nuevo a data.frame
datos <- datos %>%
  mutate(
    ### Edad en años (ligeras diferencias por sexo)
    edad = if_else(
      sexo == "Mujer",
      rnorm(n, mean = 55, sd = 12),
      rnorm(n, mean = 57, sd = 11)
    ) %>% round(1),
    
    ### IMC (kg/m^2)
    imc = rnorm(n, mean = 27, sd = 4) %>% pmax(16) %>% pmin(45) %>% round(1),
    
    ### Fumador actual
    fumador = rbinom(n, size = 1, prob = 0.3),
    
    ### Hipertensión arterial
    hta = rbinom(n, size = 1, prob = 0.4),
    
    ### Colesterol total (mg/dL)
    colesterol = rnorm(n, mean = 205, sd = 35) %>% round(0),
    
    ### Creatinina sérica (mg/dL), distribución asimétrica
    creatinina = rlnorm(n, meanlog = log(0.9), sdlog = 0.4) %>%
      pmin(3.5) %>%
      round(2)
  )

### Conversión de variables a factor y vuelta a data.frame
datos <- datos %>%
  mutate(
    fumador = factor(fumador,
                     levels = c(0, 1),
                     labels = c("No fumador", "Fumador actual")),
    hta     = factor(hta,
                     levels = c(0, 1),
                     labels = c("No HTA", "HTA"))
  )

datos <- as.data.frame(datos)

### Vista rápida
head(datos)
str(datos)

### Guardar la base simulada
write.csv(datos, "datos_riesgo_cv_simulados.csv", row.names = FALSE)


############################################################
### 2. DESCRIPCIÓN BÁSICA Y GRÁFICOS CON DPLYR + GGPLOT2 ###
############################################################

### Resumen descriptivo por sexo
resumen_por_sexo <- datos %>%
  group_by(sexo) %>%
  summarise(
    n          = n(),
    edad_media = mean(edad),
    edad_sd    = sd(edad),
    imc_media  = mean(imc),
    imc_sd     = sd(imc),
    col_med    = mean(colesterol),
    col_sd     = sd(colesterol),
    creat_med  = median(creatinina),
    creat_p25  = quantile(creatinina, 0.25),
    creat_p75  = quantile(creatinina, 0.75),
    .groups    = "drop"
  )

resumen_por_sexo

### Boxplot de IMC por sexo
ggplot(datos, aes(x = sexo, y = imc, fill = sexo)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Distribución de IMC por sexo",
    x     = "Sexo",
    y     = "IMC (kg/m^2)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

### Histograma de creatinina por sexo
ggplot(datos, aes(x = creatinina, fill = sexo)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 20) +
  labs(
    title = "Distribución de creatinina por sexo",
    x     = "Creatinina (mg/dL)",
    y     = "Frecuencia"
  ) +
  theme_minimal()


############################################################
### 3. TABLAS DESCRIPTIVAS CON TABLEONE                  ###
############################################################

### Definición de variables
vars_continuas   <- c("edad", "imc", "colesterol", "creatinina")
vars_categoricas <- c("fumador", "hta")
vars             <- c(vars_continuas, vars_categoricas)
factorVars       <- vars_categoricas

### 3.1. TableOne con medias (DE) por defecto
tabla_media <- CreateTableOne(
  vars       = vars,
  strata     = "sexo",
  data       = datos,
  factorVars = factorVars
)

### Imprimir en consola
print(tabla_media,
      showAllLevels = TRUE,
      quote         = FALSE,
      noSpaces      = TRUE,
      test          = TRUE)

### Exportar a CSV la tabla con medias (DE)
tabla_media_mat <- print(
  tabla_media,
  showAllLevels = TRUE,
  quote         = FALSE,
  noSpaces      = TRUE,
  test          = TRUE,
  printToggle   = FALSE
)

tabla_media_df <- as.data.frame(tabla_media_mat)
write.csv(tabla_media_df, "tabla_tableone_medias_por_sexo.csv", row.names = TRUE)


### 3.2. TableOne usando mediana (IQR) para creatinina
nonnormal_vars <- c("creatinina")

tabla_mediana <- CreateTableOne(
  vars       = vars,
  strata     = "sexo",
  data       = datos,
  factorVars = factorVars
)

### Imprimir en consola con mediana (IQR) para creatinina
print(tabla_mediana,
      showAllLevels = TRUE,
      quote         = FALSE,
      noSpaces      = TRUE,
      test          = TRUE,
      nonnormal     = nonnormal_vars)

### Exportar a CSV la tabla con mediana (IQR) para creatinina
tabla_mediana_mat <- print(
  tabla_mediana,
  showAllLevels = TRUE,
  quote         = FALSE,
  noSpaces      = TRUE,
  test          = TRUE,
  nonnormal     = nonnormal_vars,
  printToggle   = FALSE
)

tabla_mediana_df <- as.data.frame(tabla_mediana_mat)
write.csv(tabla_mediana_df,
          "tabla_tableone_medias_y_mediana_creatinina_por_sexo.csv",
          row.names = TRUE)


### 3.3. Exportar solo variables continuas (opcional)
tabla_continuas_mat <- print(
  tabla_mediana,
  vars        = vars_continuas,
  showAllLevels = FALSE,
  quote       = FALSE,
  noSpaces    = TRUE,
  test        = TRUE,
  nonnormal   = nonnormal_vars,
  printToggle = FALSE
)

tabla_continuas_df <- as.data.frame(tabla_continuas_mat)
write.csv(tabla_continuas_df,
          "tabla_tableone_continuas_por_sexo.csv",
          row.names = TRUE)

