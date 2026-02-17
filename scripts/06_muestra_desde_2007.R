source(here::here("config", "parametros.R"))
source(here::here("funciones", "funciones_para_importar_exportar.R"))
source(here::here("funciones", "funciones_econometria_ii.R"))
#==============================================================================#
# Importamos los datos
indice_en_diferencia<-impo_datos(nombre_archivo = "construya_diferencia.csv",carpeta = "processed")
indice_en_diferencia
mi_serie <- ts(indice_en_diferencia$log_diferencia, start = c(2002, 6), frequency = 12)
serie_filtrada <- window(mi_serie, start = c(2007, 1))

