# Para tener las mismas versiones de paquetes ***EN DUDA***
#install.packages("renv") #solo una vez
#renv::init() # Crea una carpeta para este proyecto
#renv::snapshot() #Para cuando se termine de actualizar los paquetes
#renv::restore() #Para cuando se abra el proyecto por primera vez
# =============================================================================#
# CONFIGURACIÓN GLOBAL DEL PROYECTO
# =============================================================================#
# LIMPIAR ENTORNO
rm(list = ls())

# CONFIGURAR OPCIONES GLOBALES
options(stringsAsFactors = FALSE)
options(scipen = 999)  # Evitar notación científica
options(digits = 2) # Decimales a mostrar 

# LIBRERÍAS DEL PROYECTO
library(here)
library(tidyverse)
library(lubridate)
library(strucchange) #Es para cosas de quiebre de estructura de econometría
library(forecast) # Para econometria, calcular y graficar Funcion de autocorrelacion y FACP

library(httr) #traer info de apis
library(jsonlite)# para reestructurar datos jason
#library(dplyr)
library(readxl) #leer excel

library(tseries)# paquete para trabajar series de tiempo papá! Test Dickey-fuller
library(urca)
#library(scales)
#library(readr)
# library(naniar) # Librería para analizar datos faltantes
#library(eurostat)
# library(VIM)   # Librería clásica para visualizar datos faltantes
#library(mice)         # Imputación múltiple
#library(Hmisc)        # Herramientas estadísticas
library(kableExtra)   # Tablas mejoradas
#library(patchwork)    # Combinar gráficos
#library(ggpubr)   
#library(zoo)
#library(lmtest)
#library(car)
#library(carData)
#library(MASS)
#library(ggplot2)
#library(mgcv)
#library(dplyr)
#library(writexl)
#library(countrycode)
#library(rvest)
#library(stringr)
#library(purrr)
#library(rlang)
#library(ggrepel) # Etiquetas para los graficos de outliers
library(stargazer)
library(vars) # para trbajar con modelos VAR

# DEFINIR DIRECTORIO DE MANERA RERODUCIBLE
if (!exists("proyecto_econoii")) {
  proyecto_econoii <- here::here()  # Usa el paquete 'here'
}

# RUTAS PRINCIPALES
dir_data_raw <- file.path(proyecto_econoii, "data", "raw")
dir_data_clean<-file.path(proyecto_econoii, "data", "clean")
dir_data_processed <- file.path(proyecto_econoii, "data", "processed")
dir_outputs_figures <- file.path(proyecto_econoii, "outputs", "figures")
dir_outputs_tables <- file.path(proyecto_econoii, "outputs", "tables")
dir_scripts<-file.path(proyecto_econoii, "scripts")

# CREAR DIRECTORIOS SI NO EXISTEN
dirs_crear <- c(dir_data_raw, dir_data_clean,dir_data_processed, 
                dir_outputs_figures, dir_outputs_tables,
                dir_scripts)
for (dir in dirs_crear) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
}

# PARAMETROS DE ANÁLISIS



# Funciones para mensajes consistentes
mensaje_exito <- function(texto) {
  cat("
 ✅
 ", texto, "\n")
}
mensaje_proceso <- function(texto) {
  cat("
 🔄
 ", texto, "...\n")
}
mensaje_exito("Configuración cargada correctamente")

##Carga de configuración en cada script
# Inicio de cada script