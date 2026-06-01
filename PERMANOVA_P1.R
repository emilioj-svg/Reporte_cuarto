# Paquetes
install.packages(c("vegan","readxl","tidyverse","factoextra"), dependencies = TRUE)
library(readxl); library(tidyverse); library(vegan); library(factoextra)

anfipodos_raw <- read.csv("anfipodos.csv")
# --- 2. Preparar matriz de abundancias y factor Hora ---
anfipodos_raw <- anfipodos_raw[-16,]
anf_mat <- anfipodos_raw %>%
  select(-Cuadrante, -Dia, -ID) %>%     
  as.data.frame()

# Extraer factor Hora
hora_factor <- factor(anfipodos_raw$Hora, levels = c("M","T"))

# --- 3. Transformación Hellinger (recomendada para abundancias) ---
anf_hell <- decostand(anf_mat %>% select(-Hora), method = "hellinger")


# Funciones preliminares
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
    
    4
    
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
## PERMANOVA2
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
## Permutation test based on Anderson et al. (2017)

set.seed(42)
res_perm2_hora <- PERMANOVA2(as.data.frame(anf_hell),
                             factor = hora_factor,
                             distancia = "bray",
                             nperm = 999)
res_perm2_hora

#Resultados
#  Pseudo.F p.perm.
#1 1.299584   0.307

#Pseudo.F      p.perm.     
#Min.   :1.3   Min.   :0.307  
#1st Qu.:1.3   1st Qu.:0.307  
#Median :1.3   Median :0.307  
#Mean   :1.3   Mean   :0.307  
#3rd Qu.:1.3   3rd Qu.:0.307  
#Max.   :1.3   Max.   :0.307  


