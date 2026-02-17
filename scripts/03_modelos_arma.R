source(here::here("config", "parametros.R"))
source(here::here("funciones", "funciones_para_importar_exportar.R"))
source(here::here("funciones", "funciones_econometria_ii.R"))

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
auto.arima(mi_serie, d = NA, D = NA)
# La funcion nos devuelve que el mejor modelo es uno que depende del valor anterior y del mismo mes uno y dos años atras ARIMA(1,0,0)(2,0,0)[12] 

modelo_ar01 <- arima(mi_serie, order = c(1, 0, 0)) # aic = -492
modelo_ar02 <- arima(mi_serie, order = c(2, 0, 0)) # aic = -491**
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


modelo_ma01 <- arima(mi_serie, order = c(0, 0, 1)) # aic = -488
modelo_ma02 <- arima(mi_serie, order = c(0, 0, 2)) # aic = -491**
modelo_ma03 <- arima(mi_serie, order = c(0, 0, 3)) # aic = -489
modelo_ma04 <- arima(mi_serie, order = c(0, 0, 4)) # aic = -488
<<<<<<< HEAD
modelo_ma05 <- arima(mi_serie, order = c(0, 0, 5)) # aic = -487
modelo_ma06 <- arima(mi_serie, order = c(0, 0, 6)) # aic = -487
modelo_ma07 <- arima(mi_serie, order = c(0, 0, 7)) # aic = -485
modelo_ma08 <- arima(mi_serie, order = c(0, 0, 8)) # aic = -489
modelo_ma09 <- arima(mi_serie, order = c(0, 0, 9)) # aic = -489
modelo_ma10 <- arima(mi_serie, order = c(0, 0, 10)) # aic = -489
modelo_ma11 <- arima(mi_serie, order = c(0, 0, 11)) # aic = -493
modelo_ma12 <- arima(mi_serie, order = c(0, 0, 12)) #aic = -498
BIC(modelo_ma12)#-451
=======
modelo_ma05 <- arima(mi_serie, order = c(0, 0, 5)) 
modelo_ma06 <- arima(mi_serie, order = c(0, 0, 6)) 
modelo_ma07 <- arima(mi_serie, order = c(0, 0, 7)) 
modelo_ma08 <- arima(mi_serie, order = c(0, 0, 8))
modelo_ma09 <- arima(mi_serie, order = c(0, 0, 9)) 
modelo_ma10 <- arima(mi_serie, order = c(0, 0, 10))
modelo_ma11 <- arima(mi_serie, order = c(0, 0, 11))
modelo_ma12 <- arima(mi_serie, order = c(0, 0, 12)) 
>>>>>>> 2a5954b2a2b6793ff739ac6b961f6c264340d9c7


modelo_arma01 <- arima(mi_serie, order = c(1, 0, 1)) # aic = -491**
modelo_arma02 <- arima(mi_serie, order = c(2, 0, 1)) # aic = -489
modelo_arma03 <- arima(mi_serie, order = c(1, 0, 2)) # aic = -489
modelo_arma04 <- arima(mi_serie, order = c(2, 0, 2)) # aic = -488
modelo_arma05 <- arima(mi_serie, order = c(3, 0, 3))
modelo_arma06 <- arima(mi_serie, order = c(4, 0, 4))
modelo_arma07 <- arima(mi_serie, order = c(5, 0, 5))
modelo_arma08 <- arima(mi_serie, order = c(6, 0, 6))
modelo_arma12<-arima(mi_serie, order = c(12, 0, 12)) # aic = -507
BIC(modelo_arma12)#-420
barlett(modelo_arma12)
ljung(modelo_arma12) 
modelo_arma13<-arima(mi_serie, order = c(12, 0, 2)) # aic = -500
BIC(modelo_arma13)#-447




nombres_modelo<-paste0("modelo_ar", str_pad(string = 1:12,width = 2,pad = "0"))
nombres_modelo
resultados_bic <- sapply(nombres_modelo, function(nombre) {
  BIC(get(nombre))})
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

#vayamos con ARMA
nombres_modelos<-paste0("modelo_arma", formatC(1:8, width = 2, flag = 0))
bic_arma<-sapply(nombres_modelos, function(modelo){BIC(get(modelo))})
aic_arma<-sapply(nombres_modelos, function(modelo){get(modelo)$aic})
bic_arma_orden<-sort(bic_arma) #ordenamos de menor a mayor
aic_arma_orden<-sort(aic_arma)


<<<<<<< HEAD
### *** EXTRA ***
modelo <- Arima(mi_serie, 
                order = c(1,0,0),           # Parte regular: AR(1)
                seasonal = c(2,0,0))         # Parte estacional: AR(2) con periodo 12
# aic=-502      BIC=-486
checkresiduals(modelo)
barlett(modelo)#presenta heterocedasticidad
ljung(modelo)# presenta autocorrelacion en los errores
residuos<-na.omit(residuals(modelo))

acf(residuos)

# Bamodelo# Bartlett test
bartlett.test(residuos ~ grupos) 
=======
#los juntamos a todos

sort(c(bic_ar,bic_arma, bic_ma))
sort(c(aic_ar,aic_arma, aic_ma))
>>>>>>> 2a5954b2a2b6793ff739ac6b961f6c264340d9c7

#==============================================================================#
# Los residuos o errores presentan autocorrelacion?
#==============================================================================#
nombres_modelos_arma<-paste0("modelo_arma", formatC(1:8, width = 2, flag = 0))
nombres_modelos_ma<-paste0("modelo_ma", formatC(1:12, width = 2, flag = 0))
nombres_modelos_ar<-paste0("modelo_ar", formatC(1:12, width = 2, flag = 0))
vector_nombres_modelos<-c(nombres_modelos_ar,nombres_modelos_ma)

<<<<<<< HEAD
# PRIMERO TEST de BARLETT
tsdiag(modelo_ar02)#me lo dio chatGPT. Tiene el gráfico del TestQ (Ljung-Box)


# Barlett para los AR
modelos_nombres<-paste0("modelo_ar", formatC(1:12, width = 2, flag = 0))
sapply(X = modelos_nombres, function(nombre){j<-barlett(get(nombre))
j$p.value})

# Barlett para los MA
modelos_nombres_ma<-paste0("modelo_ma", formatC(1:4, width = 2, flag = 0))
sapply(X = modelos_nombres_ma, function(nombre){j<-barlett(get(nombre))
j$p.value})

# Barlett para los ARMA
modelos_nombres_arma<-paste0("modelo_ma", formatC(1:4, width = 2, flag = 0))
sapply(X = modelos_nombres_arma, function(nombre){j<-barlett(get(nombre))
j$p.value})

barlett(modelo_arma12)
#El p valor es muy chico, RECHAZO H_0, la varianza cambia entre grupos
=======
# Test Ljun-Box para ar y ma
ljung_para_ar_y_ma<-sapply(vector_nombres_modelos, function(modelo){ljung(get(modelo))})
sort(ljung_para_ar_y_ma, decreasing = T)

# Test Ljun-Box para arma
ljung_para_arma<-sapply(nombres_modelos_arma, function(modelo){ljung_gl(get(modelo))})
sort(ljung_para_arma, decreasing = T)

# ***CONCLUSIÓN***
# Los mejores modelos que pasan la correlacion son: AR_12, MA_12. Nuestros candidatos papáaaa!

#==============================================================================#
# Calculamos los quiebres en la varianza usando Bai-Perron 
#que no es lo mejor pero es mejor que nada
#==============================================================================#
bai_perron_result<-sapply(vector_nombres_modelos, function(modelo){bai_perron(get(modelo))})
sort(bai_perron_result, decreasing = T)
bai_perron()

hey<-bai_perron(modelo = modelo_ma02)
hey
#==============================================================================#
>>>>>>> 2a5954b2a2b6793ff739ac6b961f6c264340d9c7

# TEST DE JLUNG-BOX
#Para los AR
modelos_nombres<-paste0("modelo_ar", formatC(1:12, width = 2, flag = 0))
sapply(X = modelos_nombres, function(nombre){j<-ljung(get(nombre))
j$p.value})

#Para los MA
sapply(X = modelos_nombres_ma, function(nombre){j<-ljung(get(nombre))
j$p.value})

#Para los ARMA
sapply(X = modelos_nombres_arma, function(nombre){j<-ljung(get(nombre))
j$p.value})

#==============================================================================#
# CONCLUSION: como vimos con el test de BaiPerron esta mierda tiene cambio es-
#-tructural en la varianza y se traslada al error
# VAMO A REDUCIR LA MUESTRA PARA EVITAR ESTE PROBLEMON
#==============================================================================#

<<<<<<< HEAD


sapply(X = modelos_nombres, function(nombre){ArchTest(residuals(nombre))
})

ArchTest(residuals(modelo_ar02))


chec










=======
# VAMOS A VER QUE ONDA LA AUTOCORRELACION DE LOS ERRORES DE LA "PREDICCION"
#QUE ES LO QUE IMPORTA
#==============================================================================#
# VEMOS PRIMERO LOS AR
nombres_modelos<-paste0("modelo_ar", formatC(1:12, width = 2, flag = 0))
sapply(nombres_modelos, function(modelo){ljung(get(modelo))})

sapply(nombres_modelos, function(modelo){ljung_filtro_anio(get(modelo),anio = 2014)})

ljung(modelo)
sapply(nombres_modelos, function(modelo){grafico_residuo(get(modelo))})

# VEMOS AHORA LOS MA
nombres_modelos_ma<-paste0("modelo_ma", formatC(1:12, width = 2, flag = 0))
sapply(nombres_modelos_ma, function(modelo){ljung(get(modelo))})
>>>>>>> 2a5954b2a2b6793ff739ac6b961f6c264340d9c7


