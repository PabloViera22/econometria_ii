source(here::here("config", "parametros.R"))
source(here::here("funciones", "funciones_para_importar_exportar.R"))
source(here::here("funciones", "funciones_econometria_ii.R"))
#==============================================================================#
# Importamos los datos
indice_en_diferencia<-impo_datos(nombre_archivo = "construya_diferencia.csv",carpeta = "processed")
indice_en_diferencia
#==============================================================================#
# La transofrmamos en una serie de tiempo
mi_serie <- ts(indice_en_diferencia$log_diferencia, start = c(2002, 6), frequency = 12)

mi_serie_03 <- window(mi_serie, start = c(2003, 1))
mi_serie_04<- window(mi_serie, start = c(2004, 1))
mi_serie_05<- window(mi_serie, start = c(2005, 1))
mi_serie_06<- window(mi_serie, start = c(2006, 1))
mi_serie_07<- window(mi_serie, start = c(2007, 1))
mi_serie_08<- window(mi_serie, start = c(2008, 1))
mi_serie_09<- window(mi_serie, start = c(2009, 1))
mi_serie_10<- window(mi_serie, start = c(2010, 1))
mi_serie_11<- window(mi_serie, start = c(2011, 1))
mi_serie_12<- window(mi_serie, start = c(2012, 1))
mi_serie_13<- window(mi_serie, start = c(2013, 1))
mi_serie_14<- window(mi_serie, start = c(2014, 1))
mi_serie_15<- window(mi_serie, start = c(2015, 1))
#==============================================================================#
#buscamos los respectivos modelos
modelo_03<-Arima(mi_serie_03, order = c(1,0,0), seasonal = c(2,0,0))
modelo_04<-Arima(mi_serie_04, order = c(1,0,0), seasonal = c(2,0,0))
modelo_05<-Arima(mi_serie_05, order = c(1,0,0), seasonal = c(2,0,0))
modelo_06<-Arima(mi_serie_06, order = c(1,0,0), seasonal = c(2,0,0))
modelo_07<-Arima(mi_serie_07, order = c(1,0,0), seasonal = c(2,0,0))
modelo_08<-Arima(mi_serie_08, order = c(1,0,0), seasonal = c(2,0,0))
modelo_09<-Arima(mi_serie_09, order = c(1,0,0), seasonal = c(2,0,0))
modelo_10<-Arima(mi_serie_10, order = c(1,0,0), seasonal = c(2,0,0))
modelo_11<-Arima(mi_serie_11, order = c(1,0,0), seasonal = c(2,0,0))
modelo_12<-Arima(mi_serie_12, order = c(1,0,0), seasonal = c(1,0,0))
modelo_13<-Arima(mi_serie_13, order = c(1,0,0), seasonal = c(1,0,0))
modelo_14<-Arima(mi_serie_14, order = c(1,0,0), seasonal = c(1,0,0))
modelo_15<-Arima(mi_serie_15, order = c(1,0,0), seasonal = c(1,0,0))
length(mi_serie_10)
#==============================================================================#
# VAMOS A VER QUE ONDA LA AUTOCORRELACION DE LOS ERRORES DE LA "PREDICCION" en SARIMA

nombres_modelos<-paste0("modelo_", formatC(3:15, width = 2, flag = 0)) #vector de nombres del modelo
sapply(nombres_modelos, function(modelo){ljung_gl(get(modelo))})# le aplico a los modelos test ljung

sapply(nombres_modelos, function(modelo){
  y<-checkresiduals(get(modelo))
  print(y$p.value)
  })

sapply(nombres_modelos, function(modelo){BIC(get(modelo))})
sapply(nombres_modelos, function(modelo){get(modelo)$aic})

#==============================================================================#
# Vemos ahora con el AR(1)
modelo_ar03<-arima(mi_serie_03, order = c(1, 0, 0))
modelo_ar04<-arima(mi_serie_04, order = c(1, 0, 0))
modelo_ar05<-arima(mi_serie_05, order = c(1, 0, 0))
modelo_ar06<-arima(mi_serie_06, order = c(1, 0, 0))
modelo_ar07<-arima(mi_serie_07, order = c(1, 0, 0))
modelo_ar08<-arima(mi_serie_08, order = c(1, 0, 0))
modelo_ar09<-arima(mi_serie_09, order = c(1, 0, 0))
modelo_ar10<-arima(mi_serie_10, order = c(1, 0, 0))
modelo_ar11<-arima(mi_serie_11, order = c(1, 0, 0))
modelo_ar12<-arima(mi_serie_12, order = c(1, 0, 0))
modelo_ar13<-arima(mi_serie_13, order = c(1, 0, 0))
modelo_ar14<-arima(mi_serie_14, order = c(1, 0, 0))
modelo_ar15<-arima(mi_serie_15, order = c(1, 0, 0))

nombres_modelos<-paste0("modelo_ar", formatC(3:15, width = 2, flag = 0))
sapply(nombres_modelos, function(modelo){ljung(get(modelo))}) #TEST LJUNG
sapply(nombres_modelos, function(modelo){BIC(get(modelo))})# TEST BIC
sapply(nombres_modelos, function(modelo){get(modelo)$aic})# TEST AIC

ljung(modelo_ar03)


# CONCLUSIONES:
#voy a trabajar con un sarima (100)(200) desde los datos del 2007 y que sea lo que dios quiera
#o mejor con ar 2011que cagada


#==============================================================================#
# Que onda el SARIMA VIEJAAAA
#==============================================================================#
sarima_03<-Arima(mi_serie_03, order = c(1, 0, 0), seasonal = list(order = c(2, 0, 0), period = 12))
sarima_04<-Arima(mi_serie_04, order = c(1, 0, 0), seasonal = list(order = c(2, 0, 0), period = 12))
sarima_05<-Arima(mi_serie_05, order = c(1, 0, 0), seasonal = list(order = c(2, 0, 0), period = 12))
sarima_06<-Arima(mi_serie_06, order = c(1, 0, 0), seasonal = list(order = c(2, 0, 0), period = 12))
sarima_07<-Arima(mi_serie_07, order = c(1, 0, 0), seasonal = list(order = c(2, 0, 0), period = 12))
sarima_08<-Arima(mi_serie_08, order = c(1, 0, 0), seasonal = list(order = c(2, 0, 0), period = 12))
sarima_09<-Arima(mi_serie_09, order = c(1, 0, 0), seasonal = list(order = c(2, 0, 0), period = 12))
sarima_10<-Arima(mi_serie_10, order = c(1, 0, 0), seasonal = list(order = c(2, 0, 0), period = 12))
sarima_11<-Arima(mi_serie_11, order = c(1, 0, 0), seasonal = list(order = c(2, 0, 0), period = 12))

nom_sarima<-paste0("sarima_", formatC(3:11, width = 2, flag = 0))
sapply(nom_sarima, function(modelo){ljung(get(modelo))}) #TEST LJUNG









nombres_serie<-paste0("mi_serie_", formatC(3:15, width = 2, flag = 0)) #vector de nombres del modelo
sapply(nombres_serie, function(modelo){breakpoints(get(modelo)~1, h=)})# le aplico a los modelos test ljung
bp_test <- breakpoints(mi_serie_03 ~ 1, h = 0.15)  

bp_test <- breakpoints(tu_serie ~ 1, h = 0.15)  # 15% mínimo por segmento

