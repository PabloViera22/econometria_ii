source(here::here("config", "parametros.R"))
source(here::here("funciones", "funciones_para_importar_exportar.R"))

#==============================================================================#
# Importamos los datos
indice_en_diferencia<-impo_datos(nombre_archivo = "construya_diferencia.csv",carpeta = "processed")
indice_en_diferencia

# La transofrmamos en una serie de tiempo
mi_serie <- ts(indice_en_diferencia$log_diferencia, start = c(2002, 6), frequency = 12)


#==============================================================================#
# Nos ponemos a hacer los distintos modelo 
#==============================================================================#
auto.arima(mi_serie, stepwise = FALSE, approximation = FALSE) #AIC:-503   BIC:-490
# La funcion nos devuelve que el mejor modelo es uno que depende del valor anterior y del mismo mes uno y dos años atras ARIMA(1,0,0)(2,0,0)[12] 

modelo_ar01 <- arima(mi_serie, order = c(1, 0, 0)) # aic = -492
BIC(modelo_ar01)#-457
modelo_ar02 <- arima(mi_serie, order = c(2, 0, 0)) # aic = -491**
BIC(modelo_ar02)#-477
modelo_ar03 <- arima(mi_serie, order = c(3, 0, 0)) # aic = -490
modelo_ar04 <- arima(mi_serie, order = c(4, 0, 0)) # aic = -489
modelo_ar05 <- arima(mi_serie, order = c(5, 0, 0)) #  aic = -487
modelo_ar06 <- arima(mi_serie, order = c(6, 0, 0)) #  aic = -485
modelo_ar07 <- arima(mi_serie, order = c(7, 0, 0)) # aic = -484
modelo_ar08 <- arima(mi_serie, order = c(8, 0, 0)) # aic = -482
modelo_ar09 <- arima(mi_serie, order = c(9, 0, 0)) # aic = -486
modelo_ar10 <- arima(mi_serie, order = c(10, 0, 0)) # aic = -490
modelo_ar11 <- arima(mi_serie, order = c(11, 0, 0)) # aic = -491
modelo_ar12 <- arima(mi_serie, order = c(12, 0, 0)) # aic = -503**
BIC(modelo_ar12, modelo_ar01,modelo_ar02)#-457

modelo_ma01 <- arima(mi_serie, order = c(0, 0, 1)) # aic = -488
modelo_ma02 <- arima(mi_serie, order = c(0, 0, 2)) # aic = -491**
BIC(modelo_ma02)#-478
modelo_ma03 <- arima(mi_serie, order = c(0, 0, 3)) # aic = -489
modelo_ma04 <- arima(mi_serie, order = c(0, 0, 4)) # aic = -488
modelo_ma12 <- arima(mi_serie, order = c(0, 0, 12)) #aic = -498
BIC(modelo_ma12)#-451


modelo_arma01 <- arima(mi_serie, order = c(1, 0, 1)) # aic = -491**
BIC(modelo_arma01)#-478
modelo_arma02 <- arima(mi_serie, order = c(2, 0, 1)) # aic = -489
modelo_arma03 <- arima(mi_serie, order = c(1, 0, 2)) # aic = -489
modelo_arma04 <- arima(mi_serie, order = c(2, 0, 2)) # aic = -488

modelo_arma12<-arima(mi_serie, order = c(12, 0, 12)) # aic = -507
BIC(modelo_arma12)#-420
modelo_arma13<-arima(mi_serie, order = c(12, 0, 2)) # aic = -500
BIC(modelo_arma13)#-447




nombres_modelo<-paste0("modelo_ar", str_pad(string = 1:12,width = 2,pad = "0"))
nombres_modelo
resultados_bic <- sapply(nombres_modelo, function(nombre) {
  BIC(get(nombre))})

# VOY A HACER UN RANKING DE LOS MEJORES MODELOS:
# modelo_ma02 2VAR, modelo_arma01 2VAR,modelo_ar02 2VAR


### *** EXTRA ***
modelo <- Arima(mi_serie, 
                order = c(1,0,0),           # Parte regular: AR(1)
                seasonal = c(2,0,0))         # Parte estacional: AR(2) con periodo 12

residuos <- residuals(modelo)
# TEST DE BARTLETT
# Necesitas dividir los residuos en grupos
n <- length(residuos)
grupos <- rep(1:3, each = floor(n/3), length.out = n)

# Bartlett test
bartlett.test(residuos ~ grupos) 

#==============================================================================#
# Los residuos o errores deben ser white-noise
#==============================================================================#

# PRIMERO MODELO_AR02
tsdiag(modelo_ar02)#me lo dio chatGPT. Tiene el gráfico del TestQ (Ljung-Box)

residuos <- residuals(modelo_ar02)
# TEST DE BARTLETT
# Necesitas dividir los residuos en grupos
n <- length(residuos)
grupos <- rep(1:3, each = floor(n/3), length.out = n)

# Bartlett test
bartlett.test(residuos ~ grupos) 
#El p valor es muy chico, RECHAZO H_0, la varianza cambia entre grupos
#==============================================================================#

# segundo MODELO_MA02
tsdiag(modelo_ma02)#me lo dio chatGPT. Tiene el gráfico del TestQ (Ljung-Box)

residuos_ma <- residuals(modelo_ar02)
# TEST DE BARTLETT
# Necesitas dividir los residuos en grupos
n <- length(residuos_ma)
grupos <- rep(1:3, each = floor(n/3), length.out = n)

# Bartlett test
bartlett.test(residuos_ma ~ grupos) 
#El p valor es muy chico, RECHAZO H_0, la varianza cambia entre grupos
#==============================================================================#

# PRIMERO MODELO_ARMA02
tsdiag(modelo_arma01)#me lo dio chatGPT. Tiene el gráfico del TestQ (Ljung-Box)

residuos_arma <- residuals(modelo_arma01)
# TEST DE BARTLETT
# Necesitas dividir los residuos en grupos
n <- length(residuos_arma)
grupos <- rep(1:3, each = floor(n/3), length.out = n)

# Bartlett test
bartlett.test(residuos_arma ~ grupos) 
#El p valor es muy chico, RECHAZO H_0, la varianza cambia entre grupos

#==============================================================================#
# CONCLUSION: como vimos con el test de BaiPerron esta mierda tiene cambio es-
#-tructural en la varianza y se traslada al error
# VAMO A REDUCIR LA MUESTRA PARA EVITAR ESTE PROBLEMON
#==============================================================================#


















