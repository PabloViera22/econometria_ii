source(here::here("config", "parametros.R"))
source(here::here("funciones", "funciones_para_importar_exportar.R"))

#==============================================================================#
# Importamos los datos
indice_en_diferencia<-impo_datos(nombre_archivo = "construya_diferencia.csv",carpeta = "processed")
indice_en_diferencia

names(indice_en_diferencia)
#==============================================================================#
# Graficamos

grafico<-ggplot(indice_en_diferencia, aes(x=indice_tiempo, y=diferencia))+
  geom_line(color = "steelblue", linewidth = 0.8) +
  scale_x_date(date_breaks = "1 years", 
               date_labels = "%Y") +
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
 getwd()

#==============================================================================#
# Queda analizar el cambio de estrucutra

# Calculamos los quiebres usando Bai-Perron
indice_p_perron<-indice_en_diferencia%>%dplyr::filter(!is.na(diferencia))
tiempo <- 1:length(indice_p_perron$indice_tiempo)
tiempo

modelo_tendencia <- breakpoints(indice_p_perron$diferencia ~ tiempo)
valores_en_fecha<-indice_p_perron$indice_tiempo[c(modelo_tendencia$breakpoints)]
#CONCLUSION: dice que no hay, eso es bueno

#==============================================================================#
# Vamos a calcular la FAC y FACP, hacer tambien sus graficos (correlograma)
grafico_analisis_correlacion <- ggtsdisplay(indice_en_diferencia$diferencia, 
                                            main = "Análisis de Autocorrelación: Índice Construya En Diferencia",
                                            theme = theme_minimal())

grafico_acf <- ggAcf(indice_en_diferencia$diferencia) +
  theme_minimal() +
  labs(title = "Función de Autocorrelación (ACF)", y = "Correlación")
# que nos dice el correlograma: NI IDEA, no espeba ver algo así

#==============================================================================#
# Aplicamos un test Bai-Perron para la varianza
# 1. Calculamos los residuos (restando la media)
residuos <- residuals(lm(diferencia ~ 1, data = indice_en_diferencia))
# 2. Elevamos al cuadrado (esto convierte la varianza en "nivel")
varianza_proxy <- residuos^2

# Buscamos quiebres en la media de los residuos al cuadrado
modelo_varianza <- breakpoints(varianza_proxy ~ 1)

# Ver el resumen para ver cuántos quiebres sugiere el BIC
summary(modelo_varianza)

indice_en_diferencia$indice_tiempo[c(37,67)]
# RESULTADO: tres cortes. Podemos cortar la serie, aplicar dummies, o modelo ARCH/GARCH












