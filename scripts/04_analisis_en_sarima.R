source(here::here("config", "parametros.R"))
source(here::here("funciones", "funciones_para_importar_exportar.R"))
source(here::here("funciones", "funciones_econometria_ii.R"))

#==============================================================================#
# Importamos los datos
#==============================================================================#
indice_en_diferencia<-impo_datos(nombre_archivo = "indice_con_dummies.csv",carpeta = "processed")
indice_en_diferencia

# La transofrmamos en una serie de tiempo
mi_serie <- ts(indice_en_diferencia$log_diferencia, start = c(2002, 6), frequency = 12)
#==============================================================================#
# hacemos los modelos
#==============================================================================#
sarima_01<-Arima(mi_serie, order = c(0, 0, 1), seasonal = list(order = c(2, 0, 0), period = 12), 
                 xreg = indice_en_diferencia$dummie)

sarima_02<-Arima(mi_serie, order = c(1, 0, 0), seasonal = list(order = c(2, 0, 0), period = 12), 
                 xreg = indice_en_diferencia$dummie)

sarima_03<-Arima(mi_serie, order = c(1, 0, 1), seasonal = list(order = c(2, 0, 0), period = 12), 
                 xreg = indice_en_diferencia$dummie)
#==============================================================================#
# Testeo de AIC y BIC
#==============================================================================#
BIC(sarima_01)
BIC(sarima_02)
BIC(sarima_03)

AIC(sarima_01)
AIC(sarima_02)
AIC(sarima_03)
#==============================================================================#
# Testeo de Aautocorrelacion de los errores
#==============================================================================#
ljung(sarima_01)
ljung(sarima_02)
ljung(sarima_03)
# ESTOS TAMPOCO SIRVEN. ENTONCES ME ESTÁS OBLIGANDO A HACER ALGO QUE REALMENTE NO QUIERO HACER
#voy a cortar la serie!!!!


#Muchas veces puede haber cambios múltiples o interacciones que no logremos captar, por 
#lo que es mejor acotar el período de análisis


#==============================================================================#
# REDUCIMOS LA SERIE DESDE EL DATO 63, mas adelante en realidad, el 68 
#ese numero me late
#==============================================================================#
indice_en_diferencia[68,"indice_tiempo"]
indice_filtrado<-indice_en_diferencia%>%dplyr::filter(indice_tiempo>="2008-01-01")

exportar_data(data = indice_filtrado,nombre = "indice_filtrado",carpeta = "processed",format = "csv")






