source(here::here("config", "parametros.R"))
source(here::here("funciones", "funciones_para_importar_exportar.R"))
source(here::here("funciones", "funciones_visualizacion.R"))

#==============================================================================#
# Fecha piso mas alta y ultima fecha menor
#==============================================================================#
comienzo<-ymd("2017-01-01")
final<-ymd("2025-11-01")

#==============================================================================#
# Importamos los datos de stock de pestamos al sector privado para punto 3
#==============================================================================#

token<-"eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJleHAiOjE4MDM0OTM1OTcsInR5cGUiOiJleHRlcm5hbCIsInVzZXIiOiJwNGJsby52aWVyYUBnbWFpbC5jb20ifQ.Ulp4UhmSJAXlVHcxMvDVr16L4D66MbLP9jX4UzAv5KrIDY8VfqCyFkoxeNNmSs2rPPLRoSlFCwP-GEe8w-ReFw"
endpoint<-"prestamos"
prestamos<-api_bcra(token = token,endpoint ="prestamos")

stock_prestamo_mensual<-prestamos%>%arrange(d)%>% 
  group_by(anio=year(d), mes=month(d))%>%
  slice_head(n = 1) %>%


exportar_data(data = stock_prestamo_mensual, nombre = "prestamos", carpeta = "raw",format = "csv")

prestamo_mensual<-impo_datos(nombre_archivo = "prestamos.csv",carpeta = "raw")
prestamo_mensual_acortado<-prestamo_mensual%>%filter(d>=comienzo,d<=final)
exportar_data(data = exportar_data(data = prestamo_mensual_acortado, nombre = "prestamos_acortado", carpeta = "clean",format = "csv"), nombre = "prestamos", carpeta = "raw",format = "csv")

# hay que desestacionalizarla a la basura esta

#==============================================================================#
# datos de emae 1
#==============================================================================#
emae<-impo_datos(nombre_archivo = "sh_emae_mensual_base2004.xls",carpeta ="raw")

emae<-read_excel(file.path(dir_data_raw, "sh_emae_mensual_base2004.xls"), sheet= "Hoja1")

emae_orden <- emae %>% fill(anio, .direction = "down") %>%
  # Creamos la fecha asumiendo que es el día 1 de cada mes
  mutate(fecha = ymd(paste(anio, mes, "01", sep = "-"))) %>% 
  dplyr::select(fecha, indice_serie_original_base_2004)

exportar_data(data = emae_orden, nombre = "emae", carpeta = "processed")

emae_para_acortar<-impo_datos(nombre_archivo = "emae.csv",carpeta = "processed")
emae_acortado<-emae_para_acortar%>%filter(fecha>=comienzo,fecha<=final)
exportar_data(data = emae_acortado,nombre = "emae_acortado",carpeta = "clean")

#junio 2025
#==============================================================================#
# datos de tipo de cambio real 2
#==============================================================================#

cambio_real<-read_excel(path = file.path(dir_data_raw, "ITCRMSerie.xlsx"),
                        skip = 1,
                        col_names = T)

itcrm<-cambio_real%>%dplyr::select(c(fecha=Período,itcrm=ITCRM))%>%mutate(mes=month(fecha))

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

itcrm_m<-impo_datos(nombre_archivo = "itcrm_mensual.csv",carpeta = "processed")
itcrm_acortado<-itcrm_m%>%filter(fecha>=comienzo,fecha<=final)
exportar_data(data = itcrm_acortado, nombre = "itcrm_acortado", carpeta = "clean")
#==============================================================================#
# datos de tasa de interés 3
#==============================================================================#
badlar<-read_excel(path = file.path(dir_data_raw, "tasa_de_interes_badlar.xlsx"))

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

badlar_m<-impo_datos(nombre_archivo = "interes_badlar.csv",carpeta = "processed")
badlar_acortado<-badlar_m%>%filter(fecha>=comienzo,fecha<=final)
exportar_data(data = badlar_acortado, nombre = "badlar_acortado", carpeta = "clean")
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
inflacion<-impo_datos(nombre_archivo = "indice-precios-al-consumidor-apertura-por-categorias-base-diciembre-2016-mensual.csv", 
                      carpeta = "raw")
inflacion_nombre<-inflacion%>%
  select(indice_tiempo, ipc_nivel_general_nacional)%>%
  mutate(ipc=(ipc_nivel_general_nacional/dplyr::lag(ipc_nivel_general_nacional))-1)

inflacion_acortada<-inflacion_nombre%>%filter(indice_tiempo <="2025-11-01")%>%
  filter(indice_tiempo >="2017-01-01")%>%
  arrange(indice_tiempo)
#hasta 2025/08
exportar_data(data = inflacion_acortada,nombre = "inflacion_acortada",carpeta = "clean")

#==============================================================================#
# datos de tasa de interés real
#==============================================================================#
inflacion_acortada_m<-inflacion_acortada%>%rename(fecha=indice_tiempo)
badlar_acortado

interes_real<-badlar_acortado%>%
  left_join(inflacion_acortada_m,by = "fecha")%>%
  select(-ipc_nivel_general_nacional)%>%
  mutate(badlar_mensualizado=valor_badlar_porc/1200)%>%
  mutate(interes_real= (1+badlar_mensualizado)/(1+ipc)-1)%>%
  select(fecha, interes_real)

ggplot(data = interes_real, aes(x = fecha, y = interes_real)) + 
  geom_line(color = "steelblue") + 
  theme_minimal()

exportar_data(data = interes_real,nombre = "interes_real",carpeta = "clean")

#==============================================================================#
# Hay que hacer la union de dos dataframe para stata
#==============================================================================#

itcrm<-impo_datos(nombre_archivo = "itcrm_acortado.csv",carpeta = "clean")
construya<-impo_datos(nombre_archivo = "indice_construya_acortado.csv",carpeta = "clean")

construya_itcrm<-construya%>%left_join(itcrm,by = "fecha")%>%select(-desestacionalizado)
exportar_data(data = construya_itcrm,nombre = "construya_itcrm",carpeta = "clean")

#==============================================================================#
# Combinamos las 4 series
#==============================================================================#
interes<-impo_datos(nombre_archivo = "badlar_acortado.csv",carpeta = "clean")
emae<-impo_datos(nombre_archivo = "emae_acortado.csv",carpeta = "clean")
interes_real<-impo_datos(nombre_archivo = "interes_real.csv",carpeta = "clean")

series_juntas<-construya_itcrm%>%
  left_join(interes,by = "fecha")%>%
  left_join(emae,by = "fecha")%>%
  rename(construya=con_estacionalidad)%>%
  left_join(interes_real,by = "fecha")%>%
  arrange(fecha)
exportar_data(data = series_juntas,nombre = "series_juntas",carpeta = "clean", format = "excel")













