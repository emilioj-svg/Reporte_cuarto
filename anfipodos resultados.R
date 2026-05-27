#GRÁFICO DE FRECUENCIA----------------------------------------------- 
library(tidyverse)

# 1. Cargar los datos
datos <- a

# 2. Procesar datos para Frecuencia (Corregido)
frecuencia_df <- datos %>% # Seleccionar las familias
  select(Anamixidae:Isopodae) %>% # Pasamos de formato ancho a largo
  pivot_longer(cols = everything(), names_to = "Familia", values_to = "Conteo") %>% # Limpiamr espacios en blanco 
  mutate(Familia = trimws(Familia)) %>%  # Agrupamos por familia
  group_by(Familia) %>% # Calculamos frecuencia
  summarise(
    Muestras_Presente = sum(Conteo > 0),
    Total_Muestras = n(),
    Porcentaje_Frecuencia = (Muestras_Presente / Total_Muestras) * 100
  ) %>%
  arrange(desc(Porcentaje_Frecuencia))

# 3. Graficar Frecuencia
ggplot(frecuencia_df, aes(x = reorder(Familia, Porcentaje_Frecuencia), y = Porcentaje_Frecuencia, fill = Porcentaje_Frecuencia)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() + 
  scale_fill_viridis_c(option = "YlOrRd") +
  labs(
    x = "Familia",
    y = "Porcentaje de frecuencia en las muestras(%)"
  ) +
  theme_minimal()

#GRÁFICO DE ABUNDANCIA-------------------------------------------
# 1. Procesar datos para Abundancia
abundancia_df <- datos %>%
  select(Anamixidae:Isopodae) %>% 
  pivot_longer(cols = everything(), names_to = "Familia", values_to = "Conteo") %>%
  group_by(Familia) %>%
  summarise(Abundancia_Total = sum(Conteo)) %>%
  arrange(desc(Abundancia_Total))

# 2. Graficar Abundancia
ggplot(abundancia_df, aes(x = reorder(Familia, Abundancia_Total), y = Abundancia_Total, fill = Abundancia_Total)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() + 
  scale_fill_viridis_c(option = "viridis") +
  labs(
    x = "Familia",
    y = "Número de individuos"
  ) +
  theme_minimal()

