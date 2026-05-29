#RDA PREGUNTA 2
#El Análisis de Redundancia (RDA) no es una cosa o la otra: es ambas cosas a la vez. 
#Se clasifica dentro de las técnicas de ordenación restringida (constrained ordination),

# 1. Cargar librerías necesarias
library(vegan)
library(tidyverse)

# --- SUPOSICIÓN DE ESTRUCTURA DE DATOS ---
# Asegurar de que las filas coincidan perfectamente por la columna ID.

# 1. Cargar matrices de datos 
file <- "EC6ANALISIS.xlsx"
cobertura_raw  <- read_excel(file, sheet = "Cobertura")
anfipodos_raw <- read.csv("anfipodos.csv", row.names = 1)
env_raw  <- read.csv("ParametrosFQ.csv", row.names = 1)

# 2. Aislar únicamente las columnas numéricas de abundancia de anfípodos
# Quitamos columnas de diseño como 'Cuadrante', 'Hora', 'Dia' si están ahí
anf_comunidad <- (anfipodos_raw[, 4:ncol(anfipodos_raw)])^(1/4) 

# 3. Aplicar pretratamiento (Transformación de Hellinger)
#anf_bray <- vegdist(anf_comunidad, method = "bray")
anf_hellinger <- decostand(anf_comunidad, method = "hellinger")

# ==========================================================
# 2. PRETRATAMIENTO DE LOS DATOS
# ==========================================================


# B. Predictoras Ambientales: Filtrado de multicolinealidad y Escalamiento
# Eliminamos % DO, C -us/cm y TDS mg/L por redundancia con DO mg/L y SAL
env_filtrado <- env_raw %>%
  select(-Hora, -X..DO, -C..us.cm, -TDS.mg.L) 
env_escalado <- as.data.frame(scale(env_filtrado))

# C. Coberturas: Reducción por PCA para evitar datos composicionales (suma 100%)
matriz_coberturas <- cobertura_raw[, !(names(cobertura_raw) %in% c("ID", "Dosel", "Cuadrante", "Hora", "Dia"))]
pca_cobertura <- rda(matriz_coberturas) # PCA de la vegetación

# Extraemos los dos primeros ejes que resumen la estructura del hábitat
habitat_ejes <- as.data.frame(scores(pca_cobertura, choices = c(1, 2), display = "sites"))
colnames(habitat_ejes) <- c("Habitat_PC1", "Habitat_PC2")

# D. Consolidar todas las variables predictoras limpias
predictores <- cbind(
  env_escalado,
  habitat_ejes,
  Dosel = cobertura_raw$Dosel,
  Hora = as.factor(env_raw$Hora) # Convertir a factor (Mañana/Tarde)
)

# ==========================================================
# 3. ANÁLISIS ESTADÍSTICO Y SELECCIÓN DE VARIABLES
# ==========================================================

# A. Definir el modelo nulo (sin variables) y el modelo global (con todas)

anf_hellinger <- anf_hellinger[-16,]

rda_nulo   <- rda(anf_hellinger ~ 1, data = predictores)

predictores <- predictores[-16,]
rda_global <- rda(anf_hellinger ~ ., data = predictores)

# Verificar inflación de varianza remanente en el modelo global (VIF < 5 es ideal)
vif.cca(rda_global)


# VIF de temperatura: 11.754535  (Correlacionada con otras variables: Hora,pH, DO) 
# ELIMINAR 

##El VIF (Factor de Inflación de la Varianza) mide el grado de multicolinealidad entre 
#tus variables explicativas. En términos sencillos: evalúa si tus 
#variables ambientales están tan correlacionadas entre sí que se están 
#"robando" información la una a la otra, lo que desestabilizaría las 
#estimaciones de tu Análisis de Redundancia (RDA).


# B. SELECCIÓN HACIA ADELANTE (Forward Selection)
# Encuentra qué variables explican mejor la variación basándose en R2 ajustado
mejor_modelo <- ordiR2step(rda_nulo, 
                           scope = formula(rda_global), 
                           direction = "forward", 
                           permutations = 999)

# Ver el resumen de las variables seleccionadas y el R2 final
summary(mejor_modelo)

# C. Pruebas de Significancia con PERMANOVA (del modelo óptimo seleccionado)
# Evalúa si los efectos de las variables seleccionadas son reales
permanova_resultado <- adonis2(formula(mejor_modelo), data = predictores, permutations = 999, by = "margin")
print(permanova_resultado)

#¿Por qué usar adonis2 en lugar de PERMANOVA 2?

#Una restricción matemática fundamental de PERMANOVA2: 
#Está diseñada exclusivamente para evaluar un único factor categórico 
#(Análisis de una vía / One-way).
#Debido a que el denominador calcula varianzas particionadas por grupos discretos 
#(factor == lab[i]), no pueden meterse variables continuas 
#(como Temperatura, pH o Salinidad) directamente en esta función, 
#ni tampoco múltiples variables a la vez.

# Esto te dará los coeficientes numéricos exactos de cada especie en los ejes
cargas_vegetacion <- scores(pca_cobertura, display = "species", choices = c(1, 2))
print(cargas_vegetacion)

# ==========================================================
# 4. VISUALIZACIÓN (Biplot del RDA)
# ==========================================================
# Graficar el resultado para observar hacia dónde se ordenan las familias 
# de anfípodos en relación a las variables ambientales ganadoras.
plot(mejor_modelo, scaling = 2, main = "RDA de la Comunidad de Anfípodos")
orditorp(mejor_modelo, display = "species", col = "red", air = 0.5)
text(mejor_modelo, display = "bp", col = "blue", cex = 1.2, font = 2)


