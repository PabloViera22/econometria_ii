source(here::here("config", "parametros.R"))
source(here::here("funciones", "funciones_para_importar_exportar.R"))

#==============================================================================#
# Importamos la serie de tiempo
#==============================================================================#
indice_en_diferencia_ts<- readRDS(file.path(dir_data_clean, "serie.rds"))

#==============================================================================#
# Graficamos
#==============================================================================#
grafico <- autoplot(indice_construya_ts, colour = 'steelblue', linewidth = 0.8) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray60"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10, face = "bold"),
    plot.caption = element_text(hjust = 0)
  )
grafico

#==============================================================================#
# Queda analizar el cambio de estructura
#==============================================================================#
# Calculamos los quiebres en la tendencuia usando Bai-Perron


modelo_tendencia <- breakpoints(indice_construya_ts ~ time(indice_construya_ts))
#==============================================================================##==============================================================================##==============================================================================##==============================================================================##==============================================================================#

#CONCLUSION: dice que no hay cambios en la tendencia, eso es bueno

# Calculamos los quiebres en la media usando Bai-Perron
modelo_tendencia <- breakpoints(indice_en_diferencia_ts ~ 1)
#CONCLUSION: dice que no hay cambios en la media


# Calculamos los quiebres en los coeficientes Bai-Perron

modelo_tendencia <- breakpoints(indice_en_diferencia_ts ~ stats::lag(indice_en_diferencia_ts)) #quiebre en el 2018m4
valores_en_fecha<-indice_en_diferencia$indice_tiempo[c(modelo_tendencia$breakpoints)]
valores_en_fecha
# HAY CAMBIOS EN LOS COEFICIENTES!!!!!

#==============================================================================#
# Tengo mis dudas, uso un test de chow
#==============================================================================#
test_chow <- sctest(y ~ lag(y,1), type = "Chow", point = 63)
test_chow

#==============================================================================#
# Aplicamos un test Bai-Perron para la varianza YAPA
#==============================================================================#

# 1. Calculamos los residuos (restando la media)
residuos <- residuals(lm(log_diferencia ~ 1, data = indice_en_diferencia))
# 2. Elevamos al cuadrado (esto convierte la varianza en "nivel")
varianza_proxy <- residuos^2

# Buscamos quiebres en la media de los residuos al cuadrado
modelo_varianza <- breakpoints(varianza_proxy ~ 1)

# Ver el resumen para ver cuántos quiebres sugiere el BIC
summary(modelo_varianza)

indice_en_diferencia$indice_tiempo[c(37,67)]

#==============================================================================#
# Vamos a calcular la FAC y FACP, hacer tambien sus graficos (correlograma)
#==============================================================================#
grafico_analisis_correlacion <- ggtsdisplay(indice_en_diferencia$log_diferencia, 
                                            main = "Análisis de Autocorrelación: Índice Construya En Diferencia",
                                            theme = theme_minimal())
print(grafico_analisis_correlacion)

grafico_acf <- ggAcf(indice_en_diferencia$log_diferencia) +
  theme_minimal() +
  labs(title = "Función de Autocorrelación (ACF)", y = "Correlación")
# que nos dice el correlograma: NI IDEA, no espeba ver algo así


#==============================================================================#
# Vamos a "arreglar" el cambio estructural mediante dummies
#==============================================================================#
# la dummies se utiliza a partir del 2018(4)











