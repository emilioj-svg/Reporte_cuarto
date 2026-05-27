#Diferencia de horario

library(tidyverse)

# 1. Cargar los nuevos datos
datos_fauna <- fauna

# 2. Procesar los datos
# Calcularemos qué % de los muestreos de la Mañana y qué % de la Tarde registran cada especie
fauna_horario <- datos_fauna %>%
  # Pasamos a formato largo manteniendo la columna 'Hora'
  pivot_longer(cols = -Hora, names_to = "Especie", values_to = "Presencia") %>%
  # Limpiamos nombres de especies por si tienen espacios extras
  mutate(Especie = trimws(Especie)) %>%
  # Agrupamos por Hora (M/T) y por Especie
  group_by(Hora, Especie) %>%
  summarise(
    Muestras_Presente = sum(Presencia > 0),
    Total_Muestras_Horario = n(),
    Porcentaje_Presencia = (Muestras_Presente / Total_Muestras_Horario) * 100,
    .groups = 'drop'
  )

# 3. Graficar la comparación
ggplot(fauna_horario, aes(x = reorder(Especie, Porcentaje_Presencia), y = Porcentaje_Presencia, fill = Hora)) +
  # 'position = position_dodge()' pone las barras de M y T una al lado de la otra
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  coord_flip() + 
  scale_fill_manual(
    values = c("M" = "#F1C40F", "T" = "#75C3ff"), 
    labels = c("M" = "Mañana", "T" = "Tarde")
  ) +
  labs(
    x = "Especie / Fauna",
    y = "Porcentaje de Presencia (%)",
    fill = "Horario"
  ) +
  theme_minimal() 





