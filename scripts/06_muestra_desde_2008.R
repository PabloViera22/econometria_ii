source(here::here("config", "parametros.R"))
source(here::here("funciones", "funciones_para_importar_exportar.R"))
source(here::here("funciones", "funciones_econometria_ii.R"))
#==============================================================================#
# Importamos los datos
#==============================================================================#
indice_filtrado<-impo_datos(nombre_archivo = "indice_filtrado.csv",carpeta = "processed")
indice_filtrado

#==============================================================================#
# Preparo los datos
mi_serie <- ts(indice_filtrado$log_diferencia, start = c(2008, 1), frequency = 12)
length(mi_serie)


#==============================================================================#
# Analisis Bai Perron medio que no va acá pero bueno
#==============================================================================#
tiempo <- time(mi_serie)  # Extrae el índice de tiempo
# Modelo con tendencia lineal
modelo_tendencia <- breakpoints(mi_serie ~ tiempo)

# Modelo solo con constante (cambios en media)
modelo_media <- breakpoints(mi_serie ~ 1)

# SEGUIMOS SIN ENCONTRAR EL QUIEBRE EN LA MEDIA OBVIAMENTE
#==============================================================================#
# El mejor modelo sin necesidad de pruebas
#==============================================================================#
auto.arima(mi_serie, stepwise = FALSE, approximation = FALSE)
# ARIMA(0,0,0)(2,0,0)[12]; AIC=-389   AICc=-389   BIC=-380

modelo_sarima<- Arima(mi_serie, order = c(0, 0, 0), seasonal = list(order = c(2, 0, 0), period = 12),
                       include.mean = TRUE)
#==============================================================================#
# Pasa la prueba de ljung?
#==============================================================================#
ljung(modelo_sarima)# 0.25

#CONCLUSIÓN ESTE MODELO SI SIRVE, VAMOS CON ESTE
# LOS OTROS ARIMA VAMOS A VER QUE NO SIRVEN


#MODELO EXTRA:
#modelo_sarima<- Arima(mi_serie, order = c(1, 0, 0), seasonal = list(order = c(1, 0, 0), period = 12),include.mean = TRUE)





#==============================================================================#
# Todos los modelos
#==============================================================================#
# FOR PARA MODELOS AR. Generamos los modelo ar del 1 hasta el 12
for (i in 1:12) {
    # 1. Creamos el nombre como texto: "modelo_ar01", "modelo_ar02"...
    nombre_dinamico <- paste0("modelo_ar", stringr::str_pad(i, width = 2, pad = "0"))
    
    # 2. Usamos assign para crear el objeto con ese nombre
    assign(nombre_dinamico, arima(mi_serie, order = c(i, 0, 0)))
  }
modelo_ar01
# otra opcion es acerlo en una lista

# FOR PARA MODELOS MA
for (i in 1:12) {
  # 1. Creamos el nombre como texto: "modelo_ar01", "modelo_ar02"...
  nombre_dinamico <- paste0("modelo_ma", stringr::str_pad(i, width = 2, pad = "0"))
  
  # 2. Usamos assign para crear el objeto con ese nombre
  assign(nombre_dinamico, arima(mi_serie, order = c(0, 0, i)))
}
#==============================================================================#
# Analisis BIC Y analisis AIC
#==============================================================================#
nombres_modelos<-paste0("modelo_ar", formatC(1:12, width = 2, flag = 0))
bic_ar<-sapply(nombres_modelos, function(modelo){BIC(get(modelo))})
aic_ar<-sapply(nombres_modelos, function(modelo){get(modelo)$aic})
bic_ar_orden<-sort(bic_ar) #ordenamos de menor a mayor
aic_ar_orden<-sort(aic_ar)

# vayamos con MA
nombres_modelos<-paste0("modelo_ma", formatC(1:12, width = 2, flag = 0))
bic_ma<-sapply(nombres_modelos, function(modelo){BIC(get(modelo))})
aic_ma<-sapply(nombres_modelos, function(modelo){get(modelo)$aic})
bic_ma_orden<-sort(bic_ma) #ordenamos de menor a mayor
aic_ma_orden<-sort(aic_ma)


#==============================================================================#
# Los residuos o errores presentan autocorrelacion?
#==============================================================================#
nombres_modelos_ma<-paste0("modelo_ma", formatC(1:12, width = 2, flag = 0))
nombres_modelos_ar<-paste0("modelo_ar", formatC(1:12, width = 2, flag = 0))
vector_nombres_modelos<-c(nombres_modelos_ar,nombres_modelos_ma)

# Test Ljun-Box para ar y ma
ljung_para_ar_y_ma<-sapply(vector_nombres_modelos, function(modelo){ljung(get(modelo))})
sort(ljung_para_ar_y_ma, decreasing = T)
# CONCLUSION, NO EN MUCHOS MODELOS Y ME QUEDO CON EL SARIMA

#==============================================================================#
# Prediccion un paso adelante
#==============================================================================#
# Obtener predicciones 1-paso adelante (valores ajustados)
predicciones_1paso <- fitted(modelo_sarima)

# Comparar con valores reales
comparacion <- data.frame(
  fecha = time(mi_serie),
  real = as.numeric(mi_serie),
  prediccion_1paso = as.numeric(predicciones_1paso)
)

# Ver primeras filas
head(comparacion)

# Graficar
plot(mi_serie, type = "l", col = "black", lwd = 2, 
     main = "Serie Real vs Predicción 1-paso adelante")
lines(predicciones_1paso, col = "red", lwd = 2, lty = 2)
legend("topleft", legend = c("Real", "Predicción 1-paso"), 
       col = c("black", "red"), lty = c(1, 2), lwd = 2)



