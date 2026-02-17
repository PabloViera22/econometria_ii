#==============================================================================#
# Test Ljung-Box
#==============================================================================#

ljung<-function(modelo){
  residuos <- residuals(modelo)
  test_resultado <- Box.test(residuos, lag = 14, type = "Ljung-Box", fitdf =length(modelo$coef))
  print(test_resultado$p.value)
}


ljung_gl <- function(modelo, lags = 15) {
  residuos <- residuals(modelo)
  n_parametros <- length(modelo$coef) #contamos los coef
  if (lags <= n_parametros) {
    stop("El número de lags debe ser mayor a los parámetros del modelo (", n_parametros, ")")
  } # validamos que los sean mayores que los coef
    # Ejecutar el test con fitdf dinámico
  test_resultado <- Box.test(residuos, 
                             lag = lags, 
                             type = "Ljung-Box", 
                             fitdf = n_parametros)
  return(test_resultado$p.value)
}

#==============================================================================#
# Test Ljung-Box que comienza a partir de cierto año
#==============================================================================#

ljung_filtro_anio<-function(modelo, anio){
  residuos <- residuals(modelo)
  residuos_filtrados <- window(residuos, start = c(anio, 1))
  test_resultado <- Box.test(residuos_filtrados, lag = 14, type = "Ljung-Box", fitdf =length(modelo$coef))
  return(test_resultado$p.value)
}

#==============================================================================#
# Grafico simple con título
#==============================================================================#
grafico_residuo<-function(modelo){
  residuos <- residuals(modelo)
  plot(residuos, 
       main = paste("Residuos del modelo:", modelo))
}


#==============================================================================#
# Test Bartlett
#==============================================================================#
bartlett<-function(modelo){
  residuos <- residuals(modelo)
  test_resultado <- bartlett.test(residuos, lag = 12, type = "Ljung-Box")
  print(test_resultado$p.value)
}

#==============================================================================#
# TEST Bai-Perron
#==============================================================================#

bai_perron<-function(modelo){
  residuos <- residuals(modelo)
  cuadrados <- residuos^2
  bp_auto <- breakpoints(cuadrados ~ 1, h = 0.15)  
  return(bp_auto)
}









