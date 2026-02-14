source(here::here("config", "parametros.R"))
source(here::here("funciones", "funciones_para_importar_exportar.R"))


#==============================================================================#
# Importamos los datos
construya_diferencia<-impo_datos(nombre_archivo = "construya_diferencia.csv",carpeta = "processed")
construya_diferencia
summary(construya_diferencia)
#==============================================================================#
# Calculamos los quiebres usando Bai-Perron
construya_diferencia<-construya_diferencia%>%filter(!is.na(log_diferencia))
tiempo <- 1:(length(construya_diferencia$indice_tiempo))
tiempo

modelo_tendencia <- breakpoints(construya_diferencia$log_diferencia ~ tiempo)
valores_en_fecha<-construya_diferencia$indice_tiempo[c(modelo_tendencia$breakpoints)]

# AHORA RESULTA QUE LA SERIE NUNCA TUVO UN QUIEBRE ESTRUCTURAL

acf(construya_diferencia$log_diferencia)

#==============================================================================#
# YA QUE ESTAMOS PROBEMOS QUE ONDA CON LA SERIE EN LOG SIN DIFERENCIAR
construya_log<-construya_diferencia%>%
  mutate(log_indice=log(sin_estacionalidad3))%>%
  select(-log_diferencia)


grafico_analisis_correlacion <- ggtsdisplay(construya_log$log_indice, 
                                            main = "Análisis de Autocorrelación: Índice Construya",
                                            theme = theme_minimal())

grafico_acf <- ggAcf(construya_log$log_indice) +
  theme_minimal() +
  labs(title = "Función de Autocorrelación (ACF)", y = "Correlación")
# VA A HABER RAIZ UNITARIA, POR LO QUE ESTABA BIEN DIFERENCIARLA



