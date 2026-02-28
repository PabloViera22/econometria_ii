source(here::here("config", "parametros.R"))
source(here::here("funciones", "funciones_para_importar_exportar.R"))
source(here::here("funciones", "funciones_econometria_ii.R"))

#==============================================================================#
# Importamos los datos
mi_serie<-readRDS(file.path(dir_data_clean, "serie.rds"))
mi_serie
# HACER VECTOR DE DUMMIES
dummy <- ts(0, start = start(mi_serie), end = end(mi_serie), frequency = frequency(mi_serie))
#Llenamos con 1 a partir de la fecha de quiebre
window(dummy, start = c(2018, 6)) <- 1
window(dummy, start = c(2020, 4)) <- 2
dummy
#==============================================================================#
# Nos ponemos a hacer los distintos modelo 
#==============================================================================#
auto.arima(mi_serie, stepwise = FALSE, approximation = FALSE, xreg = dummy) 
#con dummies en 2020m4 - ARIMA(0,0,3) - AIC=-79   AICc=-79   BIC=-66
#con dos dummies en 2018m6 y 2020m4 - ARIMA(0,0,3) -  IC=-79   AICc=-79   BIC=-66
# sin dummies ARIMA(0,0,3) - AIC=-81   AICc=-81   BIC=-71

# Regression with ARIMA(0,0,1)(2,0,0)[12] errors

# La funcion nos devuelve que el mejor modelo es uno que depende del valor anterior y del mismo mes uno y dos años atras ARIMA(1,0,0)(2,0,0)[12] 
lista_modelos_ar<-list()
for(i in 1:12){
  nombre <- paste0("AR", i) # Crea el nombre "AR1"..->
  lista_modelos_ar[[nombre]] <- arima(mi_serie, 
                                      order = c(i, 0, 0),xreg=dummy)
}

lista_modelos_ma<-list()
for(i in 1:12){
  nombre <- paste0("ma", i) # Crea el nombre "AR1"..->
  lista_modelos_ma[[nombre]] <- arima(mi_serie, 
                                      order = c(0, 0, i),xreg=dummy)
}

modelo_arma01 <- arima(mi_serie, order = c(1, 0, 1), xreg = dummy) # aic = -491**
modelo_arma02 <- arima(mi_serie, order = c(2, 0, 1), xreg = dummy) # aic = -489
modelo_arma03 <- arima(mi_serie, order = c(1, 0, 2), xreg = dummy) # aic = -489
modelo_arma04 <- arima(mi_serie, order = c(2, 0, 2), xreg = dummy) # aic = -488
modelo_arma05 <- arima(mi_serie, order = c(3, 0, 3), xreg = dummy)
modelo_arma06 <- arima(mi_serie, order = c(4, 0, 4), xreg = dummy)
modelo_arma07 <- arima(mi_serie, order = c(5, 0, 5), xreg = dummy)
modelo_arma08 <- arima(mi_serie, order = c(6, 0, 6), xreg = dummy)


modelo_arma13<-arima(mi_serie, order = c(12, 0, 2)) # aic = -500
BIC(modelo_arma13)#-27 muy pobre

#==============================================================================#
# Summary de ar y ma
#==============================================================================#
summary_ar<-sapply(lista_modelos_ar, summary)
summary_ma<-sapply(lista_modelos_ma, summary)

summary(lista_modelos_ma$ma3)
summary(lista_modelos_ar$AR4)



#==============================================================================#
# Analisis BIC Y analisis AIC
#==============================================================================#
# Primero con los modelos AR
bic_ar<-sapply(lista_modelos_ar, BIC)
aic_ar<-sapply(lista_modelos_ar, AIC)
bic_ar_orden<-sort(bic_ar) #ordenamos de menor a mayor
aic_ar_orden<-sort(aic_ar)

# vayamos con MA
bic_ma<-sapply(lista_modelos_ma, BIC)
aic_ma<-sapply(lista_modelos_ma, AIC)
bic_ma_orden<-sort(bic_ma) #ordenamos de menor a mayor
aic_ma_orden<-sort(aic_ma)

#vayamos con ARMA
nombres_modelos<-paste0("modelo_arma", formatC(1:8, width = 2, flag = 0))
bic_arma<-sapply(nombres_modelos, function(modelo){BIC(get(modelo))})
aic_arma<-sapply(nombres_modelos, function(modelo){get(modelo)$aic})
bic_arma_orden<-sort(bic_arma) #ordenamos de menor a mayor
aic_arma_orden<-sort(aic_arma)

#los juntamos a todos

sort(c(bic_ar,bic_arma, bic_ma))
sort(c(aic_ar,aic_arma, aic_ma))


#==============================================================================#
# Los residuos o errores presentan autocorrelacion?
#==============================================================================#
# Test Ljun-Box para ar y ma
ljung_para_ar<-sapply(lista_modelos_ar, function(modelo){ljung(modelo)})
ljung_para_ma<-sapply(lista_modelos_ma, function(modelo){ljung(modelo)})
ljung_para_ar_y_ma<-c(ljung_para_ar,ljung_para_ma)
sort(ljung_para_ar_y_ma, decreasing = T)

# Test Ljun-Box para arma
ljung_para_arma<-sapply(nombres_modelos, function(modelo){ljung_gl(get(modelo))})
sort(ljung_para_arma, decreasing = T)

# ***CONCLUSIÓN***
# SON TODOS BASURA NIGGA

#==============================================================================#
# CONCLUSION: como vimos con el test de BaiPerron esta mierda tiene cambio es-
#-tructural en la varianza y se traslada al error
# VAMO A REDUCIR LA MUESTRA PARA EVITAR ESTE PROBLEMON

#==============================================================================#












