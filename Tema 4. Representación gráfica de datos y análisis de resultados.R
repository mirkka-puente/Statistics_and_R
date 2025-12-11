rm(list=ls())



####################################################################################################
#### ---- HISTOGRAMA ---- ####
####################################################################################################
# Distribución del contenido de GC en secuencias del SARs-COV-2
'''
En estudios genómicos, es frecuente analizar el contenido de GC de diferentes regiones del genoma para identificar regiones funcionales, como islas CpG 
(áreas con alta concentración de GC y que suelen asociarse con genes activamente transcritos).

El histograma resultante te mostrará cómo se distribuye el contenido de GC en diferentes fragmentos del genoma
'''


'''
# Instalar y cargar paquetes necesarios
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Biostrings")
'''

library(Biostrings)

# Leer  archivo FASTA con secuencias genómicas (usa el del SARS-COV-2 de Ensembl): https://covid-19.ensembl.org/Sars_cov_2/Info/Index
sars_cov2_sequences <- readDNAStringSet(file="/Users/vic/Library/CloudStorage/GoogleDrive-vdelaopascual@gmail.com/Mi unidad/MU en Bioinformática (UNIR 2023)/Presentaciones (AAAA-MM-DD)/Bases de datos práctica Tema 4 con gráficos en R/Sars_cov_2.ASM985889v3.dna.toplevel.fa")
sars_cov2_sequences[1] # Mostrar la primera secuencia del archivo (vemos que hay un total de 30000 pares de bases aprox)
names(sars_cov2_sequences)[1] # Mostrar el nombre (header) de la primera secuencia
width(sars_cov2_sequences[1]) # Mostrar la longitud de la primera secuencia
as.character(sars_cov2_sequences[1]) # Si quieres ver la secuencia en formato texto

fragment_size <- 100  # Tamaño de los fragmentos (yo divido el genoma en 100 trozos)
sequence_sars <- sars_cov2_sequences[[1]]  # Obtener la secuencia de ADN completa (29,903 bases)

# Dividir la secuencia del SARS-CoV-2 en fragmentos de 100 nucleótidos
num_fragments_sars <- ceiling(length(sequence_sars) / fragment_size)
fragments_sars <- DNAStringSet(lapply(1:num_fragments_sars, function(i) {
  start <- ((i - 1) * fragment_size) + 1
  end <- min(i * fragment_size, length(sequence_sars))
  subseq(sequence_sars, start=start, end=end)
}))

# Calcular el contenido de GC para cada fragmento de SARS-CoV-2
gc_content_sars <- letterFrequency(fragments_sars, letters = c("G", "C"), as.prob = TRUE)[, "G"] +
  letterFrequency(fragments_sars, letters = c("G", "C"), as.prob = TRUE)[, "C"]

gc_content_percentage_sars <- gc_content_sars * 100



# Leer un archivo FASTA con secuencias genómicas (usa el de la gripe INFLUENZA A): https://www.ncbi.nlm.nih.gov/gene?Db=gene&Cmd=DetailsSearch&Term=956529
influenza_a_sequences <- readDNAStringSet(file="/Users/vic/Library/CloudStorage/GoogleDrive-vdelaopascual@gmail.com/Mi unidad/MU en Bioinformática (UNIR 2023)/Presentaciones (AAAA-MM-DD)/Práctica con gráficos en R/Influenza_A.fna")
influenza_a_sequences[1] # Mostrar la primera secuencia del archivo (vemos que hay un total de 1700 pares de bases aprox)
names(influenza_a_sequences)[1] # Mostrar el nombre (header) de la primera secuencia
width(influenza_a_sequences[1]) # Mostrar la longitud de la primera secuencia
as.character(influenza_a_sequences[1]) # Si quieres ver la secuencia en formato texto


fragment_size <- 100  # Tamaño de los fragmentos (yo divido el genoma en 100 trozos)
sequence_influenza <- influenza_a_sequences[[1]]  # Obtener la secuencia de ADN del Influenza A

# Dividir la secuencia de Influenza A en fragmentos de 100 nucleótidos
num_fragments_influenza <- ceiling(length(sequence_influenza) / fragment_size)
fragments_influenza <- DNAStringSet(lapply(1:num_fragments_influenza, function(i) {
  start <- ((i - 1) * fragment_size) + 1
  end <- min(i * fragment_size, length(sequence_influenza))
  subseq(sequence_influenza, start=start, end=end)
}))

# Calcular el contenido de GC para cada fragmento de Influenza A
gc_content_influenza <- letterFrequency(fragments_influenza, letters = c("G", "C"), as.prob = TRUE)[, "G"] +
  letterFrequency(fragments_influenza, letters = c("G", "C"), as.prob = TRUE)[, "C"]

gc_content_percentage_influenza <- gc_content_influenza * 100


# Crear un data frame combinado para ambos virus
gc_data <- data.frame(
  gc_content_percentage = c(gc_content_percentage_sars, gc_content_percentage_influenza),
  virus = c(rep("SARS-CoV-2", length(gc_content_percentage_sars)),
            rep("Influenza A", length(gc_content_percentage_influenza)))
)

# Comparar ambos histogramas
ggplot(gc_data, aes(x = gc_content_percentage, fill = virus)) +
  geom_histogram(binwidth = 2, color = "black", position = "identity", alpha = 0.5) +
  labs(title = "Comparación del contenido de GC entre SARS-CoV-2 y Influenza A",
       x = "Contenido de GC (%) por 100 fragmentos (covid 300 bp vs. influenza 17 bp)",
       y = "Frecuencia de fragmentos") +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 2)) +
  scale_fill_manual(values = c("lightblue", "lightcoral")) +
  theme_classic()



# Comparar ambos histogramas
ggplot(gc_data, aes(x = gc_content_percentage, fill = virus)) +
  geom_histogram(data = subset(gc_data, virus == "SARS-CoV-2"), binwidth = 2, color = "black", alpha = 0.5) +
  geom_histogram(data = subset(gc_data, virus == "Influenza A"), binwidth = 2, color = "black", alpha = 0.8) +
  labs(title = "Comparación del contenido de GC entre SARS-CoV-2 y Influenza A",
       x = "Contenido de GC (%) por 100 fragmentos (covid 300 bp vs. influenza 17 bp)",
       y = "Frecuencia de fragmentos") +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 2)) +
  scale_fill_manual(values = c("lightblue", "lightcoral")) +
  theme_classic()






####################################################################################################
#### ---- GRÁFICO DE BARRAS ---- ####
####################################################################################################
rm(list = ls())
library(phyloseq)
library(ggplot2)
library(dplyr)
library(cowplot)
library(patchwork)
library(forcats)
#remotes::install_github("KarstensLab/microshades", dependencies = TRUE) // https://github.com/KarstensLab/microshades
library(microshades)

data("GlobalPatterns")
df <- psmelt(GlobalPatterns)

str(df)

table(df$Kingdom)
table(df$Phylum)
table(df$Class)
table(df$Order)
table(df$Family)
table(df$Genus)



mdf_prep <- GlobalPatterns %>% # objeto de entrada que contiene los datos de secuenciación o microbioma en phyloseq
  tax_glom("Genus") %>% # agrupa los datos taxonómicos al nivel de género
  phyloseq::transform_sample_counts(function(x) { x/sum(x) }) %>% # normaliza los conteos de abundancia para cada muestra (divide la abundancia de cada OTU/total)
  psmelt() %>% # convertimos el objeto en dataframe leible por R
  filter(Abundance > 0) # filtramos aquellos que tengan abundancia >0

mdf_prep_feces <- mdf_prep %>%
  filter(SampleType == "Feces")

color_objs_GP_feces <- create_color_dfs(mdf_prep_feces,
                                        selected_groups = c("Proteobacteria", "Actinobacteria", "Bacteroidetes", "Firmicutes"), 
                                        cvd = TRUE)

mdf_GP_feces <- color_objs_GP_feces$mdf # extraemos la base de datos con los grupos seleccionados
cdf_GP_feces <- color_objs_GP_feces$cdf # extraemos los colores


plot_microshades(mdf_GP_feces, cdf_GP_feces) +
  theme_classic()




####################################################################################################
#### ---- GRÁFICO DE BARRAS ---- ####
####################################################################################################

















