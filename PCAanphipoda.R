# Paquetes útiles
  install.packages(c("tidyverse","factoextra"), dependencies = TRUE)
  
  library(tidyverse)
  library(factoextra)
  library(readxl)
  library(vegan)
  
  file <- "EC6ANALISIS.xlsx"
  presencia <- read_excel(file, sheet = "Presencia o ausencia de algas")
  cobertura  <- read_excel(file, sheet = "Cobertura")
  anfipodos  <- read_excel(file, sheet = "Anfipodos")

  # 1.2 Normalizar nombres de columnas (opcional)
  names(presencia) <- make.names(names(presencia))
  names(cobertura) <- make.names(names(cobertura))
  names(anfipodos) <- make.names(names(anfipodos))
  
  # 1.3 Definir qué especies son pastos marinos (seagrass) y cuáles algas
  pastos <- c("Thalassia.testudinum","Syringodium.filiforme")
  algas  <- c("Halimeda.incrassata","Caulerpa.sertularioides","Cladophora.prolifera",
              "Cladocephalus.sp.","Laurencia.sp.","Dictyota.sp.","Bryopsis.sp.",
              "Avrainvillea.sp.","Acanthophora.spicifera","Chaetomorpha.sp.")
  
  # Construir predictores: Dosel, cobertura agregada y presencia/ausencia agregada
  df_cov <- cobertura %>%
    mutate(across(everything(), ~ .)) %>%
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
  
  # Unir predictores
  pred_df <- df_cov %>%
    left_join(df_pres, by = "ID") %>%
    mutate(
      Hora = factor(Hora, levels = c("M","T")),
      Hora_num = ifelse(Hora == "T", 1, 0)
    ) %>%
    select(ID, Cuadrante, Hora, Hora_num, Dosel, Cob_Algas, Cob_Pastos, Pres_Algas, Pres_Pastos)
  
  # Matriz de respuesta: especies de anfípodos (todas las columnas de familias)
  # asumimos que las columnas de anfipodos empiezan en la 5ª columna; si no, ajusta el select
  anf_mat <- anfipodos %>%
    select(-Cuadrante, -Hora, -Dia) %>%   # deja ID y columnas de familias
    column_to_rownames("ID") %>%
    as.data.frame()
  
  # Asegurar que las filas coincidan con pred_df (mismo orden)
  anf_mat <- anf_mat[rownames(anf_mat) %in% pred_df$ID, , drop = FALSE]
  pred_df  <- pred_df %>% filter(ID %in% rownames(anf_mat))
  anf_mat  <- anf_mat[pred_df$ID, ]  # reordenar para que coincidan
  
  rowSums(anf_mat, na.rm = TRUE)
  
  #Se borró la muestra C4T19 (línea 16) por contener solo 0
  anf_mat_filtrado <- anf_mat[rowSums(anf_mat, na.rm = TRUE) > 0, ]
  anf_hell <- decostand(anf_mat_filtrado, method = "hellinger")
  
  
  # PCA con prcomp sobre la matriz Hellinger (centrado y escalado no necesario tras Hellinger,
  # pero prcomp scale.=FALSE es apropiado porque Hellinger ya estandariza)
  pca_anf <- prcomp(anf_hell, center = TRUE, scale. = FALSE)
  
  # Eigen-análisis (autovalores) y cargas
  sdev <- pca_anf$sdev
  eigenvalues <- sdev^2
  loadings <- pca_anf$rotation   # cargas de especies en PCs
  scores <- pca_anf$x            # proyecciones de muestras (composición)
  
  # Resumen de varianza
  var_table <- tibble(
    PC = paste0("PC", 1:length(eigenvalues)),
    Eigenvalue = round(eigenvalues, 4),
    PropVar = round(eigenvalues / sum(eigenvalues), 4),
    CumVar = round(cumsum(eigenvalues / sum(eigenvalues)), 4)
  )
  print(var_table)
  
  # Matriz numérica de predictores para envfit (usar variables relevantes)
  pred_numeric <- pred_df %>%
    select(Hora_num, Dosel, Cob_Algas, Cob_Pastos, Pres_Algas, Pres_Pastos) %>%
    mutate_all(as.numeric)
  
  # Matriz de correlación entre predictores (PCA de correlación si se desea)
  cor_mat <- cor(pred_numeric, use = "pairwise.complete.obs")
  round(cor_mat, 3)
  
  pred_numeric <- pred_numeric[-16,]
  
  nrow(pca_anf$x)        # número de observaciones en el PCA
  nrow(pred_numeric)     # número de observaciones en los predictores
  nrow(pred_numeric_filtrado) 
  
  # Envfit: ajusta vectores de predictores sobre el ordination (PCA)
  set.seed(42)
  ef <- envfit(pca_anf, pred_numeric, permutations = 999)
  ef  # muestra vectores y p-values
  
  # Extraer resultados de envfit para graficar
  ef_scores <- scores(ef, display = "vectors")
  ef_pvals  <- ef$vectors$pvals
  ef_r2     <- ef$vectors$r
  ef_df     <- as.data.frame(ef_scores)
  ef_df$variable <- rownames(ef_df)
  ef_df$p.value  <- ef_pvals
  ef_df$r2       <- ef_r2
  ef_df
  
  
#############VISUALIZACIÓN###############
  pred_df<- pred_df[-16,]
  
  # 5.1 Añadir scores al dataframe para ggplot
  df_plot <- pred_df %>%
    bind_cols(as_tibble(scores))   # agrega PC1, PC2, ...
  
  # 5.2 Gráfico PC1 vs PC2 coloreado por Hora
  ggplot(df_plot, aes(x = PC1, y = PC2, color = Hora)) +
    geom_point(size = 3) +
    theme_minimal() +
    labs(title = "PCA de composición de anfípodos (Hellinger)", x = "PC1", y = "PC2")
  
  # 5.3 Biplot con especies y vectores de predictores (factoextra + envfit)
  # Graficar individuos y especies
  fviz_pca_biplot(pca_anf, repel = TRUE,
                  col.ind = "grey30",
                  col.var = "darkgreen",
                  title = "Biplot: anfípodos (Hellinger)")
  
  # Añadir vectores de envfit manualmente (ggplot) si quieres destacar predictores significativos
  # Ejemplo: extraer vectores para PC1 y PC2 y escalar para visualización
  vecs <- as.data.frame(scores(ef, display = "vectors")) %>%
    rownames_to_column("var")
  
  # Usando ggplot: puntos + vectores
  ggplot(df_plot, aes(PC1, PC2)) +
    geom_point(aes(color = Hora), size = 3) +
    geom_segment(data = vecs, aes(x = 0, y = 0, xend = PC1*2, yend = PC2*2),
                 arrow = arrow(length = unit(0.3, "cm")), color = "red") +
    geom_text(data = vecs, aes(x = PC1*2.2, y = PC2*2.2, label = var), color = "red") +
    theme_minimal() +
    labs(title = "PCA anfípodos + vectores de predictores (envfit)")
  
  #Al haber una presencia de pastos en todas las muestras no aportan nada al PCA 
  #dado que su varianza es cero, por lo que incluirlo es redundante 
  