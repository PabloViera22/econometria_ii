source(here::here("config", "parametros.R"))
source(here::here("funciones", "funciones_para_importar_exportar.R"))

#==============================================================================#
# Importamos los datos
indice_en_diferencia<-impo_datos(nombre_archivo = "construya_diferencia.csv",carpeta = "processed")
indice_en_diferencia

names(indice_en_diferencia)
#==============================================================================#
# Graficamos

grafico<-ggplot(indice_en_diferencia, aes(x=indice_tiempo, y=diferencia))+
  geom_line(color = "steelblue", linewidth = 0.8) +
  scale_x_date(date_breaks = "1 years", 
               date_labels = "%Y") +
  theme_minimal() +
  
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray60"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10, face = "bold"),
    plot.caption = element_text(hjust = 0)
  ) 
grafico
 getwd()





