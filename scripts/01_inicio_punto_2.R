source(here::here("config", "parametros.R"))
source(here::here("funciones", "funciones_para_importar_exportar.R"))
source(here::here("funciones", "funciones_visualizacion.R"))

#==============================================================================#
# Importamos los datos para punto 2
indice_construya<-impo_datos(nombre_archivo = "indice_construya_acortado.csv",carpeta = "clean")
indice_construya%>%arrange(fecha)
summary(indice_construya)

# lo hacemos serie de tiempo
indice_construya_ts<-ts(indice_construya$con_estacionalidad,start = c(2017,1), frequency = 12)

#==============================================================================#
# Calculamos los quiebres usando Bai-Perron

tiempo <- 1:length(indice_construya)
tiempo

modelo_tendencia <- breakpoints(indice_construya ~ tiempo)

indice_construya_ts[60]
#==============================================================================#
#  

library(ggfortify) # Esencial para que ggplot entienda objetos ts

# fortify(indice_construya_ts) crea una columna llamada "Index" (la fecha) 
# y otra llamada "Data" (el valor)

grafico <- autoplot(indice_construya_ts, colour = 'steelblue', linewidth = 0.8) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray60"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10, face = "bold", angle = 45, vjust = 0.5),
    plot.caption = element_text(hjust = 0)
  )
grafico

#==============================================================================#
# Hacemos tabla con las fechas de los quiebres
tabla_quiebres <- data.frame(
  Evento = paste("Quiebre", 1:length(mis_fechas)),
  Fecha = format(valores_en_fecha, "%d-%m-%Y") # Formato Día-Mes-Año
)
print(tabla_quiebres)

#==============================================================================#
# Vamos a calcular la FAC y FACP, hacer tambien sus graficos (correlograma)
#no limpiamos la tendencia todavía

grafico_analisis_correlacion <- ggtsdisplay(indice_construya, 
            main = "Análisis de Autocorrelación: Índice Construya",
            theme = theme_minimal())

grafico_acf <- ggAcf(indice_construya) +
  theme_minimal() +
  labs(title = "Función de Autocorrelación (ACF)", y = "Correlación")
# que nos dice el correlograma: autocorrelacion muy cercana a uno y cae muy
#lentamente. Estamos ante una raiz unitaria. Testear con Dickey-Fuller

#==============================================================================#
# Analisis de raiz unitaria PARTE1: Dickey-Fuller
resultado_adf <- adf.test(indice_construya, 
                          alternative = "explosive")
# Ver el resultado
print(resultado_adf) # LA SERIE NO ES ESTACIONARIA (ES EXPLOSIVA)
# ahora el test para la primera diferencia
adf_con_diferencia<-adf.test(diff(indice_construya)) #este resultado p<0.05 confirma raiz unitaria. LA SERIE ES INTEGRADA DE ORDEN 1.

#==============================================================================#
# Analisis de raiz unitaria PARTE2: Phillips-Perron
# Escenario 1: Con Deriva y Tendencia
pp_trend <- ur.pp(indice_construya, type = "Z-tau", model = "trend", lags = "short")
summary(pp_trend)
#aca importa ver el valor del test-statistic, type:z-tau. se rechaza H0 si el estadistico es mas negativoque el valor critico. En este caso -3 es mas grande que -4,-3,4,-3,1 por lo que no rechazamos que exista raiz unitaria

# Escenario 2: Con Deriva únicamente
pp_drift <- ur.pp(indice_construya, type = "Z-tau", model = "constant", lags = "short")
summary(pp_drift)
#si el grafico tiene tendencia clara el test correcta fue el primero. Aca no rechazamos si somos prudentes aunque se suele usar al 0.05.


#==============================================================================#
# Vamos a trabajar con la serie en diferencia papá!!!
# hacemos otra columna con la serie en diferecnai
construya_log_diff <- diff(log(indice_construya_ts))
construya_ts_log_diff <- tk_tbl(construya_log_diff, rename_index = "fecha")

exportar_data(data = construya_ts_log_diff,nombre = "construya_diferencia", carpeta = "processed")

