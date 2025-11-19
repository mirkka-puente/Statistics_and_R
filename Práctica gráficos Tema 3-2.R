rm(list=ls())
setwd("C:/Users/Rober/OneDrive/Desktop/UNIR/2025_2026/BIOESTADISTICA/Roberto")


f <- list.files(pattern="^Dataset.*genes\\.csv$", ignore.case=TRUE)[1]
df <- read.csv(f, fileEncoding="UTF-8-BOM", check.names=FALSE)

library(ggplot2)
library(dplyr)
library(pheatmap)



#### ---- Histogramas ---- ####
ggplot(df, aes(x = edad, fill = sexo)) +
  geom_histogram(color = "black", alpha = 0.4, position = "identity", bins = 10) +
  labs(title = "Distribución de la Edad", x = "Edad (años)", y = "Frecuencia") +
  scale_x_continuous(breaks = seq(20, 100, by = 2)) + # Etiquetas cada 2 años
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



#### ---- Diagramas de barras ---- ####
## poner el SE (mirar foro)
ggplot(df, aes(x = sexo)) +
  geom_bar(fill = c("lightpink", "lightgreen"), color = "black") +
  labs(title = "Distribución por Sexo", x = "Sexo", y = "Conteo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



#### ---- Box plots ---- ####
ggplot(df, aes(x = tratamiento, y = glucosa, fill = sexo)) +
  geom_boxplot(outlier.color = "black", outlier.size = 2, width = 0.7) +
  labs(title = "Distribución de Glucosa por Sexo", x = "Sexo", y = "Nivel de Glucosa (mg/dL)") +
  scale_fill_manual(values = c("lightpink", "lightgreen")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


#### ---- Scatter plots ---- ####
# Simple Scatter Plot
ggplot(df, aes(x = glucosa, y = chol)) +
  geom_point(color = "lightblue", alpha = 0.7, size = 3) +
  labs(title = "Relación entre Glucosa y Colesterol", x = "Glucosa (mg/dL)", y = "Colesterol (mg/dL)") +
  geom_smooth(method = "lm") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



# Scatter Plot by sexo
ggplot(df, aes(x = glucosa, y = chol, color = sexo)) +
  geom_point(alpha = 0.7, size = 3) +
  labs(title = "Relación entre Glucosa y Colesterol por Sexo", x = "Glucosa (mg/dL)", y = "Colesterol (mg/dL)") +
  scale_color_manual(values = c("lightpink", "lightgreen")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  guides(color = guide_legend(title = "Sexo"))


#### ---- Pie charts ---- ####  no hacer
# Crear tabla de frecuencias
tumor_counts<- df %>%
  count(extension) %>%
  mutate(perc = n / sum(n) * 100, label = paste0(round(perc, 1), "%"))

# Graficar
ggplot(tumor_counts, aes(x = "", y = perc, fill = extension)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Distribución de Tipos de Tumor") +
  theme_void() +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))
  
  
#### ---- Heatmaps ---- ####
# Seleccionar solo las columnas de expresión génica
bioquimica <- df %>% select("glucosa":"neutrofilos", "chol":"cpk", starts_with("AQ_"))
corr_matrix <- cor(bioquimica, method = "spearman")
corr_matrix

rows <- 17:nrow(corr_matrix)
cols <- colnames(corr_matrix)[1:16]
corr_matrix_sel <- corr_matrix[rows, cols, drop = FALSE]

# Graficar heatmap
pheatmap(corr_matrix_sel, 
         main = "Heatmap",
         cluster_cols = FALSE,
         cluster_rows = FALSE,
         color = colorRampPalette(c("blue", "white", "red"))(100))

set.seed(1995)
pheatmap(corr_matrix_sel, 
         main = "Heatmap",
         cluster_cols = TRUE,
         cluster_rows = TRUE,
         color = colorRampPalette(c("blue", "white", "red"))(100))


set.seed(1995)
pheatmap(bioquimica, 
         main = "Heatmap",
         cluster_cols = TRUE,
         cluster_rows = TRUE,
         color = colorRampPalette(c("blue", "white", "red"))(100))


bioquimica <- df %>% select("glucosa":"neutrofilos", "chol":"cpk", starts_with("AQ_"))
bioquimica <- bioquimica %>% select(-"igg", -starts_with("AQ_"))
set.seed(1995)
pheatmap(bioquimica, 
         main = "Heatmap",
         fontsize_row = 8,
         cluster_cols = TRUE,
         cluster_rows = TRUE,
         color = colorRampPalette(c("blue", "white", "red"))(100))




# Estratificamos por tratamiento
bioquimica_a <- df %>% 
  filter(tratamiento=="placebo") %>%
  select("glucosa":"neutrofilos", "chol":"cpk", starts_with("AQ_"))

corr_matrix_a <- cor(bioquimica_a, method = "spearman")
corr_matrix_a

dim(corr_matrix_a)
corr_matrix_sel_a <- corr_matrix_a[17:46, 1:16]
corr_matrix_sel_a


bioquimica_b <- df %>% 
  filter(tratamiento=="farmaco") %>%
  select("glucosa":"neutrofilos", "chol":"cpk", starts_with("AQ_"))

corr_matrix_b <- cor(bioquimica_b, method = "spearman")
corr_matrix_b

dim(corr_matrix_b)
corr_matrix_sel_b <- corr_matrix_b[17:46, 1:16]
corr_matrix_sel_b



# Instalación (solo la primera vez)
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) BiocManager::install("ComplexHeatmap")
if (!requireNamespace("circlize", quietly = TRUE)) install.packages("circlize")

# Carga
library(ComplexHeatmap)
library(grid)        # para gpar()
library(circlize)    # para colorRamp2


set.seed(1995)
Heatmap(corr_matrix_sel_a)
Heatmap(corr_matrix_sel_a,
        row_km = 2,
        column_km = 2,
        row_names_gp = gpar(fontsize = 8),
        color = colorRampPalette(c("blue", "white", "red"))(100))


set.seed(1995)
Heatmap(corr_matrix_sel_b,
        row_km = 0,
        column_km = 2,
        row_names_gp = gpar(fontsize = 8),
        color = colorRampPalette(c("blue", "white", "red"))(100))






# Otras librería
library(ggstatsplot)

ggbarstats(df, sexo, tratamiento)
ggbetweenstats(df, x=trat, y=glucosa)
ggwithinstats(df, x=trat, y=glucosa)
grouped_ggbetweenstats(df, x=trat, y=glucosa, grouping.var = sexo, p.adjust.methods = "bonferroni")
grouped_ggwithinstats(df, x=trat, y=glucosa, grouping.var = sexo, p.adjust.methods = "bonferroni")
ggscatterstats(df, glucosa, chol)


mod <- lm(glucosa ~ edad + chol + igA + igE, data = df)
ggcoefstats(mod, exclude.intercept = TRUE)




# library see
library(see)

ggplot(df, aes(x = tratamiento, y = glucosa, fill = sexo)) +
  geom_violindot(dots_size = 0.8, 
                 position_dots = position_dodge(0.1), 
                 flip = c(1)) +
  geom_line(aes(group = tratamiento), # Usamos un identificador de pareja si existe
            alpha = 0.3, 
            position = position_dodge(0.1)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 10))




