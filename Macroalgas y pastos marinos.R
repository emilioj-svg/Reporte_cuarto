####Composición de pastos marinos y macroalgas####

library(ggplot2)
library(tidyr)
library(dplyr)

# 2. Cargar los datos
# Asegúrate de que el archivo esté en tu directorio de trabajo
datos <- read.csv("datos.csv", check.names = FALSE)

# 3. Acomodar los datos de formato ancho a formato largo para ggplot
datos_largos <- datos %>%
  pivot_longer(
    cols = -Especie, 
    names_to = "Especie_Biologica", 
    values_to = "Presencia"
  ) %>%
  # Ordenar de mayor a menor presencia para que la gráfica se vea limpia
  arrange(desc(Presencia))

# 4. Crear la gráfica de barras estética
grafica_barras <- ggplot(datos_largos, aes(x = reorder(Especie_Biologica, -Presencia), y = Presencia, fill = Especie_Biologica)) +
  geom_col(show.legend = FALSE, size = 0.08, width = 0.8) +
  # Agregar las etiquetas de los valores arriba de cada barra
  geom_text(aes(label = Presencia), vjust = -0.5, size = 3.5, fontface = "bold") +
  # Paleta de colores inspirada en tonos marinos/algas (puedes cambiarla por otra de viridis si prefieres)
  scale_fill_viridis_d(option = "mako", begin = 0.2, end = 0.8) +
  # Configurar títulos y etiquetas de los ejes
  labs(
    x = "Especie",
    y = "Total"
  ) +
  # Aplicar un tema limpio y minimalista
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, face = "italic", color = "gray30"),
    # Itálicas para los nombres científicos y rotación para que no se amontonen
    axis.text.x = element_text(angle = 45, hjust = 1, face = "italic", size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(), # Quitamos líneas verticales para que se vea más limpio
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

# 5. Mostrar la gráfica en RStudio
print(grafica_barras)

# 6. Opcional: Guardar la gráfica en alta calidad para tu tarea
# ggsave("grafica_presencia_especies.png", plot = grafica_barras, width = 10, height = 6, dpi = 300)


#### Dosel ####
# 1. Cargar librerías
library(dplyr)
library(ggplot2)

datos_dosel <- read.csv("Cobertura.csv", check.names = FALSE)

# Opcional: Ver un resumen estadístico rápido en tu consola (Media, Mediana, Mínimo, Máximo)
summary(datos_dosel$`Dosel (cm)`)


# 4. Crear la gráfica de distribución de frecuencias (Histograma + Densidad)
grafica_dosel <- ggplot(datos_dosel, aes(x = `Dosel (cm)`)) +
  # Dibujar las barras del histograma (frecuencia de alturas)
  # 'binwidth = 5' agrupa las alturas de 5 en 5 cm (ej. de 0-5, 5-10...). 
  #Puedes cambiarlo a 2 o 3 si quieres más detalle.
  geom_histogram(aes(y = after_stat(count)), binwidth = 5, fill = "#2a9d8f", 
                 color = "white", alpha = 0.85) +
  # Añadir una línea de tendencia suave (densidad ajustada al conteo)
  #geom_density(aes(y = after_stat(count) * 5), color = "#e76f51", size = 1.2) +
  # Línea vertical para marcar la media (promedio de altura)
  #geom_vline(aes(xintercept = mean(`Dosel (cm)`)), color = "#f4a261", 
  #           linetype = "dashed", size = 1) +
  # Etiquetas de texto informativas
  #annotate("text", x = mean(datos_dosel$`Dosel (cm)`) + 8, y = 3, 
  #        label = paste("Promedio:", round(mean(datos_dosel$`Dosel (cm)`), 
  #                                         1), "cm"), 
  #         color = "#d97706", fontface = "bold", size = 3.5) +
  # Títulos y ejes
  labs(
    x = "Altura del dosel (cm)",
    y = "Frecuencia (# de cuadrantes)"
  ) +
  # Estética limpia
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic", 
                                 color = "gray30"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

# 5. Mostrar la gráfica en RStudio
print(grafica_dosel)

# Guardar la gráfica en tu computadora
# ggsave("frecuencia_altura_dosel.png", plot = grafica_dosel, width = 8, height = 5, dpi = 300)


#### Cobertura ####
#Cargar librerías necesarias
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

#COBERTURA POR ESPECIE
#Cargar la base de datos completa
base_completa <- read.csv("Coberturatotal.csv", check.names = FALSE)
# Acomodo de datos para Cobertura por Género
datos_cobertura_genero <- base_completa %>%
  select(5:ncol(base_completa)) %>%
  pivot_longer(cols = everything(), names_to = "Especie", values_to = "Cobertura") %>%
  # Extraemos solo la primera palabra del nombre de la especie (el Género)
  mutate(Genero = word(Especie, 1)) %>%
  # Agrupamos por género y calculamos la cobertura promedio de todo el muestreo
  group_by(Genero) %>%
  summarise(Cobertura_Promedio = mean(Cobertura, na.rm = TRUE)) %>%
  arrange(desc(Cobertura_Promedio))

# Mostrar en consola la tabla de coberturas para responder cuál es el mayor
print(datos_cobertura_genero)

# Graficar Cobertura Promedio por Género
grafica_cobertura <- ggplot(datos_cobertura_genero, aes(x = 
                                                          reorder(Genero, -Cobertura_Promedio), y = Cobertura_Promedio, fill = Genero)) +
  geom_col(show.legend = FALSE, size = 0.3, width = 0.7) +
  geom_text(aes(label = paste0(round(Cobertura_Promedio, 1), "%")), 
            vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_fill_viridis_d(option = "plasma", begin = 0.2, end = 0.8) +
  labs(
    x = "Género",
    y = "Cobertura promedio (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"),
    panel.grid.major.x = element_blank()
  )

print(grafica_cobertura)
