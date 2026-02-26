source(here::here("config", "parametros.R"))
source(here::here("funciones", "funciones_para_importar_exportar.R"))
source(here::here("funciones", "funciones_visualizacion.R"))

#==============================================================================#
# Importamos los datos de stock de pestamos al sector privado para punto 3
#==============================================================================#

token<-"eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJleHAiOjE4MDM0OTM1OTcsInR5cGUiOiJleHRlcm5hbCIsInVzZXIiOiJwNGJsby52aWVyYUBnbWFpbC5jb20ifQ.Ulp4UhmSJAXlVHcxMvDVr16L4D66MbLP9jX4UzAv5KrIDY8VfqCyFkoxeNNmSs2rPPLRoSlFCwP-GEe8w-ReFw"
endpoint<-"prestamos"
prestamos<-api_bcra(token = token,endpoint ="prestamos")

stock_prestamo_mensual<-prestamos%>%arrange(d)%>% 
  group_by(anio=year(d), mes=month(d))%>%
  slice_head(n = 1) %>%
  ungroup()

exportar_data(data = stock_prestamo_mensual, nombre = "prestamos", carpeta = "raw",format = "csv")
prestamo_mensual<-impo_datos(nombre_archivo = "prestamos.csv",carpeta = "raw")

# hay que desestacionalizarla a la basura esta

#==============================================================================#
# datos de emae 1
#==============================================================================#
emae<-impo_datos(nombre_archivo = "sh_emae_mensual_base2004.xls",carpeta ="raw")

emae<-read_excel("D:/ProyectosR/econoii/data/raw/sh_emae_mensual_base2004.xls", sheet= "Hoja1")

emae_orden<-emar%>%fill(anio, .direction = "down")

exportar_data(data = emae_orden, nombre = "emae", carpeta = "processed")

emae_viejo<-impo_datos(nombre_archivo = "emae.csv",carpeta = "processed")
#junio 2025
#==============================================================================#
# datos de tipo de cambio real 2
#==============================================================================#

cambio_real<-read_excel(path = "D:/ProyectosR/econoii/data/raw/ITCRMSerie.xlsx",
                        skip = 1,
                        col_names = T)

itcrm<-cambio_real%>%select(c(fecha=Período,itcrm=ITCRM))%>%mutate(mes=month(fecha))

# TRANSFORMAR DATOS DIARIOS EN PROMEDIOS MENSUALES
itcrm_mensual <- itcrm %>%
  mutate(fecha = ymd(fecha)) %>%
  group_by(anio = year(fecha), mes = month(fecha)) %>%
  summarise(itcrm = mean(itcrm, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(fecha = make_date(anio, mes, 1)) %>%
  select(fecha, itcrm)


exportar_data(data = itcrm_mensual, nombre = "itcrm_mensual", carpeta = "processed")

#hasta 2026-02

#==============================================================================#
# datos de tasa de interés 3
#==============================================================================#
badlar<-read_excel(path = "D:/ProyectosR/econoii/data/raw/tasa_de_interes_badlar.xlsx")

badlar_nom<-badlar%>%select(fecha=Fecha,valor_badlar_porc=`Valor BADLAR (%)`,variacion_porc=`Variación (%)`)

#PONEMOS FORMATO FECHA, CUANDO SOLO ESTABA ESCRITO
badlar_nom <- badlar_nom %>%
  mutate(fecha = dmy(fecha, locale = "es_ES"))

badlar_mes<-badlar_nom%>%
  group_by(anio=year(fecha), mes= month(fecha))%>%
  summarise(valor_badlar_porc= mean(valor_badlar_porc, na.rm=T))%>%
  ungroup%>%
  mutate(fecha = make_date(anio, mes, 1)) %>%
  select(fecha, valor_badlar_porc)

exportar_data(data = badlar_mes, nombre = "interes_badlar", carpeta = "processed")

# hasta 2025/09

#==============================================================================#
# datos de indice construya 4
#==============================================================================#
url <- "https://www.grupoconstruya.com.ar/servicios/indice_construya"
texto <- read_html(url) %>% html_text()

# Extraer cada campo por separado con regex multilínea
meses  <- str_match_all(texto, "Mes:\\s*\\**([A-Za-záéíóúÁÉÍÓÚñÑ]+ \\d{4})")[[1]][, 2]
est    <- str_match_all(texto, "Con Estacionalidad:\\s*([\\d,\\.]+)")[[1]][, 2]
desest <- str_match_all(texto, "Desestacionalizado:\\s*([\\d,\\.]+)")[[1]][, 2]

df <- tibble(
  mes                = meses,
  con_estacionalidad = as.numeric(str_replace(est,    ",", ".")),
  desestacionalizado = as.numeric(str_replace(desest, ",", "."))
) %>%
  mutate(fecha = my(mes)) %>%
  select(fecha, con_estacionalidad, desestacionalizado) %>%
  arrange(fecha)

head(df, 10)

n<-nrow(df)

indice_construya_final<-df%>%mutate(fecha=seq(
  from = as.Date("2026-01-01"),
  by = "-1 month",
  length.out = n
))

exportar_data(data = indice_construya_final,nombre ="indice_construya_final" ,carpeta = "processed")

indice_importado<-impo_datos(nombre_archivo = "indice_construya_final.csv",carpeta = "processed")
indice_construya_acortado<-indice_importado%>%filter(fecha<="2025-11-01")%>%filter(fecha>="2017-01-01")
exportar_data(data = indice_construya_acortado,nombre ="indice_construya_acortado" ,carpeta = "clean")
# empieza en enero del 2017
#==============================================================================#
# datos de indice de inflacion de la ciudad
#==============================================================================#
inflacion<-impo_datos(nombre_archivo = "indice-precios-al-consumidor-apertura-por-categorias-base-diciembre-2016-mensual.csv", carpeta = "raw")
inflacion_nombre<-inflacion%>%select(indice_tiempo, ipc_nivel_general_nacional)
tail(inflacion_nombre)
#hasta 2025/08






 
 
 
