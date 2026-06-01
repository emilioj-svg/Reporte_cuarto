# Paquetes necesarios
install.packages(c("vegan","readr","tidyverse"), dependencies = TRUE)
library(vegan)
library(readr)
library(tidyverse)

# --- 0. Leer la tabla de fauna móvil (archivo CSV adjunto) ---
fauna <- read_csv("Fauna movil.csv")   # ajusta la ruta si hace falta
# Verificar
glimpse(fauna)

# --- 1. Preparar matriz de respuesta y factor Hora ---
# Convertir ID a fila y extraer matriz de abundancias (especies)
fauna_mat <- fauna %>%
  column_to_rownames("ID") %>%
  select(-Hora) %>%
  as.data.frame()

# Factor Hora (alineado con las filas)
hora_factor <- factor(fauna$Hora, levels = c("M","T"))

# --- 2. Transformación recomendada para datos de abundancia comunitaria ---
# Hellinger (recomendado para análisis de ordenación y PERMANOVA)
fauna_hell <- decostand(fauna_mat, method = "hellinger")


# Alternativas (si prefieres): log1p o cuarta raíz
# fauna_log1p <- log1p(fauna_mat)
# fauna_4th <- sqrt(sqrt(fauna_mat))

# --- 3. Distancia de comunidad (Bray-Curtis sobre Hellinger) ---
dist_bray_fauna <- vegdist(fauna_mat, method = "bray")

# --- 4. Comprobación de homogeneidad de dispersiones (betadisper) ---
bd_hora_fauna <- betadisper(dist_bray_fauna, hora_factor)
anova(bd_hora_fauna)            # ANOVA sobre dispersión
permutest(bd_hora_fauna, permutations = 999)  # test permutacional

# --- 5. Usar funciones: definir las funciones  ---
# Sum of Squares using Theorem of Huygen
SS <- function (d) {
  n <- dim(as.matrix(d))[1]
  ss <- sum(d ^ 2) / n
  return(ss)
}
# Multivariate Dispersion
v = function (d) {
  n <- dim(as.matrix(d))[1]
  ss <- sum(d ^ 2) / n
  v <- ss / (n - 1)
  return(v)
}
# Modified Pseudo-F (Anderson et al., 2017)
pseudo.F <- function (x, factor, distancia) {
  d <- vegdist(x, method = distancia)
  TSS <- SS(d)
  group <- as.data.frame(factor)
  x$grp <- factor
  factor <- as.factor(factor)
  lab <- names(table(group))
  lev <- table(group)
  CR <- c(1:nlevels(factor))
  for (i in 1:nlevels(factor)) {
    CR[i] <-
      SS(vegdist(x[x$grp == lab[i], 1:length(x) - 1], method = distancia))
  }
  RSS <- sum(CR)
  Var <- c(1:nlevels(factor))
  d.res <-
    as.data.frame(matrix(nrow = length(levels(factor)), ncol = 3))
  for (i in 1:nlevels(factor)) {
    Var[i] <-
      v(vegdist(x[x$grp == lab[i], 1:length(x) - 1], method = distancia))
    d.res[i, ] <- c(lev[i],
                    Var[i],
                    (1 - (lev[i] / sum(lev))) * Var[i])
  }
  den <- sum(d.res$V3)
  ASS <- TSS - RSS
  Fobs <- ASS / den
  return(Fobs)
}
# PERMANOVA2

PERMANOVA2 <- function(x, factor, distancia, nperm = 999) {
  control <- how(nperm = nperm, within = Within(type = "free"))
  Fobs <- pseudo.F(x, factor, distancia = distancia)
  Nobs <- nobs(x)
  F.permu <- numeric(length = control$nperm) + 1
  F.permu[1] <- Fobs
  ## Generation of pseudo.F values for H0 using permutations without replacement
  for (i in seq_along(F.permu)) {
    ## return a permutation
    want <- permute(i, Nobs, control)
    ## calculate permuted F
    F.permu[i + 1] <-
      pseudo.F(x[want, ], factor, distancia = distancia)
  }
  ## probability for Fobs
  pval <- sum(abs(F.permu) >= abs(F.permu[1])) / (control$nperm + 1)
  ## Results
  return(data.frame("Pseudo-F" = F.permu[1], "p(perm)" = pval))
}

# --- 6. Ejecutar PERMANOVA2 sobre la matriz transformada  ---
# Nota: la función pseudo.F espera la matriz x con las especies en columnas y filas en muestras
set.seed(42)
res_perm2_hora <- PERMANOVA2(as.data.frame(fauna_mat), hora_factor, 
                             distancia = "bray", nperm = 999)
res_perm2_hora

# --- 7. Comparación con adonis2 (vegan) para referencia ---
set.seed(42)
adonis_res_hora <- adonis2(dist_bray_fauna ~ hora_factor, 
                           permutations = 999, by = "margin")
adonis_res_hora

# --- 8. Salida resumida ---
print("Resultados PERMANOVA2 (pseudo-F y p permutacional):")
print(res_perm2_hora)
#  Pseudo.F p.perm.
#  2.784055   0.042
# SIGNIFICATIVO

print("Resultados adonis2 (Bray ~ Hora):")
print(adonis_res_hora)

#            Df SumOfSqs      R2     F Pr(>F)  
#hora_factor  1  0.45626 0.26479 2.521  0.056 .
# NO SIGNIFICATIVO
