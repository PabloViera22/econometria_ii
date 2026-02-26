source(here::here("config", "parametros.R"))

# Cargar librerías (instálalas si no las tienes con install.packages("flextable"))
library(flextable) # para hacer tablas


# Creamos la tabla con tus resultados de Stata
datos_test <- data.frame(
  Prueba = c("Augmented Dickey-Fuller", "Phillips-Perron"),
  Estadistico = c(-1.659, -3.109),
  p_valor = c(0.7683, 0.1041),
  Conclusion = c("No Estacionaria", "No Estacionaria")
)

mi_tabla <- flextable(datos_test) %>%
  set_header_labels(p_valor = "p-valor", Estadistico = "Estadístico Z(t)") %>%
  theme_booktabs() %>%      # Estilo profesional de libro
  autofit() %>%             # Ajustar tamaño de celdas
  bold(part = "header")     # Negritas en el encabezado

# Ver la tabla en el panel de RStudio
print(mi_tabla)







