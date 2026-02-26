source(here::here("config", "parametros.R"))
source(here::here("funciones", "funciones_para_importar_exportar.R"))
source(here::here("funciones", "funciones_econometria_ii.R"))

#==============================================================================#
# importo las 4 variables
#==============================================================================#
emae<-impo_datos(nombre_archivo = "emae.csv",carpeta = "processed")
cambio_real<-impo_datos(nombre_archivo = "itcrm_mensual.csv",carpeta = "processed")
tasa_de_interes<-impo_datos(nombre_archivo = "interes_badlar.csv",carpeta = "processed")
indice_construya<-impo_datos(nombre_archivo = "indice_construya_final.csv",carpeta = "processed")


#==============================================================================#
# transformar en series de tiempo
#==============================================================================#
emae<-emae%>%arrange(anio,mes)
cambio_real<-cambio_real%>%arrange(fecha)
tasa_de_interes<-tasa_de_interes%>%arrange(fecha)
indice_construya<-indice_construya%>%arrange(fecha)

emae_ts <- ts(emae$indice_serie_original_base_2004, start = c(2004,1), frequency = 12)
cambio_real_ts<-ts(cambio_real$itcrm, start = c(1997,1), frequency = 12)
tasa_de_interes_ts<-ts(tasa_de_interes$valor_badlar_porc,start = c(2008,1), frequency = 12)
indice_construya_ts<-ts(indice_construya$con_estacionalidad,start = c(2017,1), frequency = 12)
#==============================================================================#
# Recortamos la serie
#==============================================================================#
series_alineadas <- ts.intersect(emae_ts, cambio_real_ts, tasa_de_interes_ts, indice_construya_ts)
comienzo<-start(series_alineadas)
final<-end(series_alineadas)
#recortamos todas las series
emae_ts <- window(emae_ts, start = comienzo, end = final)
cambio_real_ts<-window(cambio_real_ts, start = comienzo, end = final)
tasa_de_interes_ts<-window(tasa_de_interes_ts, start = comienzo, end = final)
indice_construya_ts<-window(indice_construya_ts, start = comienzo, end = final)

#==============================================================================#
# Graficamos
#==============================================================================#
series_unidas <- cbind(emae_ts, 
                       cambio_real_ts, 
                       tasa_de_interes_ts, 
                       indice_construya_ts)

# 2. Graficamos. 'yax.flip = TRUE' ayuda a que los ejes no se encimen
plot(series_unidas, 
     main = "Análisis de Variables Económicas", 
     col = "steelblue", 
     lwd = 2, 
     xlab = "Año")
#==============================================================================#
# test de dickey fuller
#==============================================================================#
nombres<-c("emae_ts","cambio_real_ts","tasa_de_interes_ts","indice_construya_ts")
sapply(nombres, function(modelo){adf.test(get(modelo), alternative = "stationary")})

# resultados, todos los P-value estan por arriba del 0.55. Son raicez unitarias
# el pvalue del test para indice_construya es no significativo al 10%, está ahí nomás
#igual voy a trabajar con la diferencia

#==============================================================================#
# desestacionalizo
#==============================================================================#
vesctor_p_values<-sapply(nombres, function(modelo){desestacionalizar(get(modelo))})
# siguen igual

#==============================================================================#
# aplico log diferencia
#==============================================================================#
dlog_emae <- diff(log(emae_ts))
dlog_cambio_real   <- diff(log(cambio_real_ts))
dlog_tasa_de_interes <- diff(log(tasa_de_interes_ts))
dlog_indice_construya<- diff(log(indice_construya_ts))

#==============================================================================#
# test de dickey fuller - VUELVO A PROBAR SI TIENEN  RAIZ UNITARIA
#==============================================================================#
nombres_diff<-c("dlog_emae","dlog_cambio_real","dlog_tasa_de_interes","dlog_indice_construya")
sapply(nombres_diff, function(modelo){adf.test(get(modelo), alternative = "stationary")})
# ahora si son series estacionarias

#==============================================================================#
# ver cauntos rezagos son optimos
#==============================================================================#
data_ts <- cbind(dlog_emae, dlog_cambio_real, dlog_tasa_de_interes, dlog_indice_construya)

VARselect(data_ts, lag.max = 12, type = "const")
# 1 rezago solo

#==============================================================================#
# Hacemos el modelo VAR
#==============================================================================#
modelo_var <- VAR(data_ts, p = 1, type = "const")

#vemos las rtaices unitarias del modelo, deben ser menores a 1
roots(modelo_var)
plot(stability(modelo_var))

# Test de autocorrelación
serial.test(modelo_var, lags.pt = 12, type = "PT.asymptotic")
#no rechazo H_0, entonces no hay autocorrelacion, PVALUE=0.2

#==============================================================================#
# Causalidad de Grnager
#==============================================================================#
# Causalidad de Granger
data_limpia <- data_var
colnames(data_limpia) <- c("emae", "tc", "tasa", "construya")

# 1. Instala y carga el paquete aod si no lo tienes
library(aod)


#==============================================================================#
# Funcion impulso respuesta
#==============================================================================#

irf_modelo_TC <- irf(modelo_var,
                  impulse = "dlog_cambio_real",
                  response = "dlog_indice_construya",
                  n.ahead = 12,
                  boot = TRUE)

plot(irf_modelo_TC)


irf_modelo_EMAE <- irf(modelo_var,
                     impulse = "dlog_emae",
                     response = "dlog_indice_construya",
                     n.ahead = 12,
                     boot = TRUE)
plot(irf_modelo_EMAE)


irf_modelo_TASA <- irf(modelo_var,
                       impulse = "dlog_tasa_de_interes",
                       response = "dlog_indice_construya",
                       n.ahead = 12,
                       boot = TRUE)
plot(irf_modelo_TASA)








