source(here::here("config", "parametros.R"))
source(here::here("funciones", "funciones_econometria_ii.R"))
source(here::here("funciones", "funciones_para_importar_exportar.R"))
#==============================================================================#
# LIBRERÍAS que vamos a utilizar
#==============================================================================#
library(xts)#necesario para cargar el siguiente
library(quantmod) #Descargar y manipular datos financieros (por ejemplo desde Yahoo Finance) y graficarlos fácilmente.
library(rugarch)#Estimar modelos ARCH, GARCH y sus extensiones
library(tseries)#Tests clásicos de series de tiempo (ADF, Jarque-Bera, ARCH test, etc.).
library(FinTS)#Herramientas econométricas financieras, especialmente el test ARCH-LM y utilidades para volatilidad.

#==============================================================================#
# Preparar los datos y verlos
#==============================================================================#

# Descargar datos del S&P500
getSymbols("^GSPC", src = "yahoo", from = "2015-01-01")
sp500 <- Cl(GSPC)

# Calcular retornos logarítmicos
ret <- diff(log(sp500))
ret <- na.omit(ret)

# Graficar retornos
plot(ret, main="Retornos diarios S&P 500")

#==============================================================================#
# Hacer test de Dickey-Fuller
#==============================================================================#
resultado_adf <- adf.test(ret)
#Es estacionaria

#==============================================================================#
# ¿Hay efectos ARCH? Este paso el profe no lo hizo
#==============================================================================#
ArchTest(ret, lags = 12)
# Es un test de ARCH, si la rechazas significa que hay clusterind en la varianza.
# RECHAZAMOS H_0

#==============================================================================#
# Calculamos un modelo para la media
#==============================================================================#
auto.arima(ret, stepwise = FALSE, approximation = FALSE)
#ARIMA(2,0,2) with non-zero mean 
modelo_arima<-arima(x = ret, order = c(2,0,2))


# Calcular solo donde el valor real NO sea cero
reales <- as.numeric(ret$GSPC.Close)
estimados <- as.numeric(fitted(modelo_arima))

# Filtrar índices donde reales != 0
validos <- reales != 0
errores<-(reales[validos] - estimados[validos]) / reales[validos]
cambio_pct <- mean(errores, na.rm = TRUE) * 100
plot(errores)

# Testeo de errores correlacionados
ljung(modelo = modelo_arima)

#==============================================================================#
# Estimar modelo ARCH(1)
#==============================================================================#
# Especificar modelo ARCH(1)
spec_arch <- ugarchspec( #para especificar el modelo
  variance.model = list(model="sGARCH", garchOrder=c(1,0)),#sGarch es el modelo garch estandar GARCH (p,q)#garchOrder=c(1,0) vector p=terminos ARCH, q=terminos garch
  mean.model = list(armaOrder=c(2,2)),#aca iría el modelo ARIMA, en este caso solo es una constante
  distribution.model = "std" #afecta la funcion de verosimilitud que se maximizan, tambien se usa t-studenr std
)

fit_arch <- ugarchfit(spec=spec_arch, data=ret, solver = "hybrid")
show(fit_arch)

# Todo va mal pero continuamos como si nada

#==============================================================================#
# Estimar modelo ARCH(1)
#==============================================================================#

spec_garch_t <- ugarchspec(
  variance.model = list(model="sGARCH", garchOrder=c(1,1)),
  mean.model = list(armaOrder=c(2,2)),
  distribution.model = "std"
)

fit_garch_t <- ugarchfit(spec=spec_garch_t, data=ret)
show(fit_garch_t)

# Mejora del ajuste extraordinariamente bueno

infocriteria(fit_arch)
infocriteria(fit_garch_t)

# Test sobre residuos estandarizados
res_std <- residuals(fit_garch_t, standardize=TRUE)
ArchTest(res_std, lags=12)

# no efecto ARCH, cazó el efecto










