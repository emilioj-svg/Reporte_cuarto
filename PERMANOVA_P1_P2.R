# Paquetes
install.packages(c("vegan","readxl","tidyverse","factoextra"), dependencies = TRUE)
library(readxl); library(tidyverse); library(vegan); library(factoextra)

# --- 0. Leer y preparar datos (ajusta ruta y nombres de hojas si es necesario) ---
file <- "EC6ANALISIS.xlsx"
presencia <- read_excel(file, sheet = "Presencia o ausencia de algas")
cobertura  <- read_excel(file, sheet = "Cobertura")
anfipodos  <- read_excel(file, sheet = "Anfipodos")

# Normalizar nombres
names(presencia) <- make.names(names(presencia))
names(cobertura)  <- make.names(names(cobertura))
names(anfipodos)  <- make.names(names(anfipodos))

# Crear ID como carácter
presencia$ID <- as.character(presencia$ID)
cobertura$ID  <- as.character(cobertura$ID)
anfipodos$ID  <- as.character(anfipodos$ID)

# Construir predictores agregados (ejemplo: Cob_Pastos, Cob_Algas, Pres_Pastos, Pres_Algas)
pastos <- c("Thalassia.testudinum","Syringodium.filiforme")
algas  <- c("Halimeda.incrassata","Caulerpa.sertularioides","Cladophora.prolifera",
            "Cladocephalus.sp.","Laurencia.sp.","Dictyota.sp.","Bryopsis.sp.",
            "Avrainvillea.sp.","Acanthophora.spicifera","Chaetomorpha.sp.")

df_cov <- cobertura %>%
  rowwise() %>%
  mutate(
    Cob_Pastos = sum(c_across(any_of(intersect(names(cobertura), pastos))), na.rm = TRUE),
    Cob_Algas  = sum(c_across(any_of(intersect(names(cobertura), algas))), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(ID, Cuadrante, Hora, Dia, Dosel, Cob_Algas, Cob_Pastos)

df_pres <- presencia %>%
  rowwise() %>%
  mutate(
    Pres_Pastos = ifelse(sum(c_across(any_of(intersect(names(presencia), pastos))), na.rm = TRUE) > 0, 1, 0),
    Pres_Algas  = ifelse(sum(c_across(any_of(intersect(names(presencia), algas))), na.rm = TRUE) > 0, 1, 0)
  ) %>%
  ungroup() %>%
  select(ID, Pres_Pastos, Pres_Algas)

pred_df <- df_cov %>%
  left_join(df_pres, by = "ID") %>%
  mutate(Hora = factor(Hora, levels = c("M","T")),
         Hora_num = ifelse(Hora == "T", 1, 0)) %>%
  select(ID, Cuadrante, Hora, Hora_num, Dosel, Cob_Algas, Cob_Pastos, Pres_Algas, Pres_Pastos)

# Matriz de anfipodos: filas = ID, columnas = familias
anf_mat <- anfipodos %>%
  select(-Cuadrante, -Hora, -Dia) %>%
  column_to_rownames("ID") %>%
  as.data.frame()

# Asegurar mismo orden entre pred_df y anf_mat
pred_df <- pred_df %>% filter(ID %in% rownames(anf_mat))
anf_mat <- anf_mat[pred_df$ID, , drop = FALSE]

# --- 1. Transformación de la matriz de anfípodos (Hellinger) ---
# Hellinger recommended for community data (reduces weight of dominant taxa)
rowSums(anf_mat, na.rm = TRUE)


# Si Amphithoidae (o Amphithoidea) domina mucho y quieres reducir su influencia adicionalmente:
# opcional: aplicar cuarta raíz solo a esa columna antes de Hellinger (ejemplo)
# if("Amphithoidae" %in% colnames(anf_mat)) {
#   anf_mat_mod <- anf_mat
#   anf_mat_mod$Amphithoidae <- sqrt(sqrt(anf_mat_mod$Amphithoidae))
#   anf_hell <- decostand(anf_mat_mod, method = "hellinger")
# }

#Se borró la muestra C4T19 (línea 16) por contener solo 0
anf_mat_filtrado <- anf_mat[rowSums(anf_mat, na.rm = TRUE) > 0, ]
anf_hell <- decostand(anf_mat_filtrado, method = "hellinger")

# --- 2. Distancia de comunidad (Bray-Curtis sobre Hellinger o directamente 
#Bray sobre abundancias) Bray sobre Hellinger es equivalente a usar distancia 
#euclidiana en Hellinger; Bray es común.
dist_bray <- vegdist(anf_hell, method = "bray")  
# o method = "euclid" si prefieres euclidiana sobre Hellinger

# --- 3. PCA de predictores ambientales (para obtener PCs explicativas) ---
pred_numeric <- pred_df %>%
  select(Hora_num, Dosel, Cob_Algas, Cob_Pastos, Pres_Algas, Pres_Pastos) %>%
  mutate_all(as.numeric)

pred_numeric <- pred_numeric[-16,]


# Escalar y hacer PCA (matriz de correlación)
pca_env <- prcomp(pred_numeric, center = TRUE, scale. = TRUE) 
#Ese error es exactamente el mismo que te salió antes con el PCA: tienes columnas constantes
#o de varianza cero en pred_numeric. Al usar scale.=TRUE, R intenta dividir cada variable 
#por su desviación estándar, pero si esa desviación es cero, no puede hacerlo.

sapply(pred_numeric, var, na.rm = TRUE)
pred_numeric_filtrado <- pred_numeric[, sapply(pred_numeric, function(x) var(x, na.rm = TRUE) != 0)]

#Si alguna columna es constante porque representa una condición fija 
#(ej. todos los sitios muestreados a la misma hora), 
#no aporta nada al PCA y está bien eliminarla.
#Se eliminó presencia de pastos dado que estos se encontraron en todos los cuadrantes 

pca_env <- prcomp(pred_numeric_filtrado, center = TRUE, scale. = TRUE)
summary(pca_env)


nrow(pca_env$x)        # número de observaciones en el PCA
nrow(pred_df)    # número de observaciones en los predictores
pred_df <- pred_df[-16,]

# Extraer los scores (PCs) que usarás como variables explicativas en PERMANOVA
# Selecciona los primeros n PCs que expliquen suficiente varianza (ej. 2 o 3)
env_scores <- as.data.frame(pca_env$x)   # columnas PC1, PC2, ...
# Ejemplo: usar PC1 y PC2
pred_df$PC1_env <- env_scores$PC1
pred_df$PC2_env <- env_scores$PC2
pred_df$PC3_env <- env_scores$PC3

# --- 4. Comprobación de homogeneidad de dispersiones (betadisper) antes de PERMANOVA ---
# Para el test por Hora
bd_hora <- betadisper(dist_bray, pred_df$Hora)
anova(bd_hora)            # test ANOVA sobre dispersión
permutest(bd_hora, permutations = 999)

# Para los grupos definidos por PC1 (si quieres categorizar PC1 en terciles para chequear dispersión)
# pred_df$PC1_cat <- cut(pred_df$PC1_env, breaks = quantile(pred_df$PC1_env, probs = c(0,1/3,2/3,1)), include.lowest = TRUE)
# bd_pc1 <- betadisper(dist_bray, pred_df$PC1_cat)
# anova(bd_pc1); permutest(bd_pc1, permutations = 999)

# --- 5. PERMANOVA:  a) ¿Los PCs ambientales explican la composición? ---
# Modelo: distancia de comunidad ~ PC1 + PC2  (añade más PCs si lo deseas)
set.seed(42)
permanova_env <- adonis2(dist_bray ~ PC1_env + PC2_env + PC3_env, data = pred_df, permutations = 999, by = "margin")
permanova_env

# Alternativa: usar los scores numéricos directamente sobre la matriz de especies (sin precomputar dist)
adonis2(anf_hell ~ PC1_env + PC2_env, data = pred_df, permutations = 999, method = "bray")
##IGUAL DE SIGNIFICATIVO 


# --- 6. PERMANOVA: b) ¿Hay diferencia en composición entre Horas (M vs T)? ---
set.seed(42)
permanova_hora <- adonis2(dist_bray ~ Hora, data = pred_df, permutations = 999, by = "margin")
permanova_hora

# Si quieres incluir ambos efectos (Hora + PCs) en un solo modelo:
set.seed(42)
permanova_full <- adonis2(dist_bray ~ Hora + PC1_env + PC2_env, data = pred_df, permutations = 999, by = "margin")
permanova_full

# --- 7. Resultados y diagnóstico adicional ---
# Mostrar resultados resumidos
print("PERMANOVA: PCs ambientales")
print(permanova_env)

print("PERMANOVA: Hora (M vs T)")
print(permanova_hora)

print("PERMANOVA: Modelo completo (Hora + PCs)")
print(permanova_full)

# Visualización rápida: ordination (PCA/NMDS) con vectores de predictores
# PCA sobre anf_hell para visualizar ejes de composición
pca_anf <- prcomp(anf_hell, center = TRUE, scale. = FALSE)
fviz_pca_biplot(pca_anf, repel = TRUE, col.ind = pred_df$Hora, title = "PCA anfípodos (Hellinger) - coloreado por Hora")

# Añadir vectores de envfit para ver asociación de predictores con ejes de composición
ef <- envfit(pca_anf, pred_numeric, permutations = 999)
plot(pca_anf$x[,1:2], col = as.factor(pred_df$Hora), pch = 19)
plot(ef, col = "red")


#R2 indica la proporción de varianza explicada por cada término; Pr(>F) 
#sugiere si esa proporción es mayor que esperada por azar (permutaciones).

#Si Amphithoidae domina: si su influencia sigue siendo excesiva tras Hellinger, 
#prueba la opción comentada en el script (aplicar cuarta raíz a esa columna antes 
#de Hellinger) y repite PERMANOVA para ver si los resultados cambian.

