#==============================================================================#
# Modelo de test barlett
#==============================================================================#
barlett<-function(modelo){
  residuos<-residuals(modelo)
  n <- length(residuos)
  grupos <- rep(1:3, each = floor(n/3), length.out = n)
  bartlett.test(residuos ~ grupos) 
}


ljung<-function (modelo){
  residuos<-residuals(modelo)
  Box.test( x = residuos, lag = 20, type = "Ljung-Box")
}
