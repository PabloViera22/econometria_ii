source(here::here("config", "parametros.R"))

# Cargar librerías (instálalas si no las tienes con install.packages("flextable"))
library(flextable) # para hacer tablas

#==============================================================================#
# Tabla 1
#==============================================================================#
# Creamos la tabla con tus resultados de Stata
datos_test <- data.frame(
  Prueba = c("Augmented Dickey-Fuller", "Phillips-Perron"),
  Estadistico = c(-1.659, -3.109),
  p_valor = c(0.7683, 0.1041),
  Conclusion = c("No Estacionaria", "No Estacionaria")
)

mi_tabla <- flextable(datos_test) %>%
  set_header_labels(p_valor = "p-valor", Estadistico = "Estadístico Z(t)") %>%
  theme_booktabs() %>%      # Estilo profesional de libro
  autofit() %>%             # Ajustar tamaño de celdas
  bold(part = "header")     # Negritas en el encabezado

# Ver la tabla en el panel de RStudio
print(mi_tabla)


#==============================================================================#
# Tabla 2
#==============================================================================#

datos_test <- data.frame(
  Prueba = c("Augmented Dickey-Fuller", "Phillips-Perron"),
  Estadistico = c(-3.742, -13.514),
  p_valor = c(0.0036, 0.0000),
  Conclusion = c("Estacionaria", "Estacionaria")
)

mi_tabla <- flextable(datos_test) %>%
  set_header_labels(p_valor = "p-valor", Estadistico = "Estadístico Z(t)") %>%
  theme_booktabs() %>%      # Estilo profesional de libro
  autofit() %>%             # Ajustar tamaño de celdas
  bold(part = "header")     # Negritas en el encabezado

# Ver la tabla en el panel de RStudio
print(mi_tabla)

#==============================================================================#
# Tabla/dataframe para poder seleccionar mejor modelo
#==============================================================================#


modelos <- data.frame(
  modelo = c(
    # 2 quiebres
    "ARIMA(0,0,1)", "ARIMA(0,0,2)", "ARIMA(0,0,3)", "ARIMA(0,0,4)",
    "ARIMA(1,0,0)", "ARIMA(2,0,0)", "ARIMA(3,0,0)", "ARIMA(4,0,0)",
    "ARIMA(1,0,1)", "ARIMA(2,0,1)", "ARIMA(1,0,2)", "ARIMA(2,0,2)",
    
    # 1 quiebre
    "ARIMA(0,0,1)", "ARIMA(0,0,3)", "ARIMA(0,0,4)",
    "ARIMA(1,0,0)", "ARIMA(2,0,0)", "ARIMA(3,0,0)", "ARIMA(4,0,0)",
    "ARIMA(1,0,1)", "ARIMA(2,0,1)", "ARIMA(1,0,2)", "ARIMA(2,0,2)",
    
    # 0 quiebres
    "ARIMA(0,0,1)", "ARIMA(0,0,2)", "ARIMA(0,0,3)", "ARIMA(0,0,4)",
    "ARIMA(1,0,0)", "ARIMA(2,0,0)", "ARIMA(3,0,0)", "ARIMA(4,0,0)",
    "ARIMA(1,0,1)", "ARIMA(2,0,1)", "ARIMA(1,0,2)", "ARIMA(2,0,2)"
  ),
  
  quiebres = c(
    rep(2, 12),
    rep(1, 11),
    rep(0, 12)
  ),
  
  AIC = c(
    # 2 quiebres
    -82.12446, -86.79419, -87.93122, -86.01412,
    -80.65946, -83.78515, -84.19148, -82.37857,
    -88.95048, -87.71214, -87.78447, -85.78449,
    
    # 1 quiebre
    -82.88609, -88.66360, -86.69129,
    -81.70990, -84.52572, -84.71079, -82.90889,
    -93.21811, -88.75784, -88.84879, -86.86612,
    
    # 0 quiebres
    -84.36879, -88.53479, -90.17344, -88.18740,
    -83.33898, -86.03537, -86.13724, -84.32889,
    -95.20450, -93.28590, -93.31419, -91.42887
  ),
  
  BIC = c(
    # 2 quiebres
    -68.80726, -70.81355, -69.28715, -64.70661,
    -67.34227, -67.80452, -65.54740, -61.07106,
    -72.96985, -69.06806, -69.14040, -64.47697,
    
    # 1 quiebre
    -72.23233, -72.68297, -68.04722,
    -71.05614, -71.20852, -68.73016, -64.26482,
    -82.56435, -72.77721, -72.86816, -68.22205,
    
    # 0 quiebres
    -76.37848, -77.88104, -76.85625, -72.20677,
    -75.34866, -75.38161, -72.82004, -68.34826,
    -87.21419, -82.63215, -82.66043, -78.11168
  )
)



modelos


modelos[order(modelos$AIC), ][1:10, ]
modelos[order(modelos$BIC), ][1:10, ]


#==============================================================================#
# modelo MA(2)
#==============================================================================#

datos_test <- data.frame(
  dif_log = c("ma", "L1", "L2"),
  Coeficientes = c("",-0.242, -0.268),
  std_err= c("", 0.046,0.071),
  z=c("",-5.25,-3.73),
  "P-Valor"=  c("",0.000,0.000)
)
datos_test

mi_tabla <- flextable(datos_test) %>%
  set_header_labels(p_valor = "p-valor", Estadistico = "Estadístico Z(t)") %>%
  theme_booktabs() %>%      # Estilo profesional de libro
  autofit() %>%             # Ajustar tamaño de celdas
  bold(part = "header")     # Negritas en el encabezado

# Ver la tabla en el panel de RStudio
print(mi_tabla)



#==============================================================================#
# regeresion TCRM y constucción
#==============================================================================#
resultado <- data.frame(
  variable = c("itcrm", "_cons"),
  coefficient = c(-0.3836133, 366.1518),
  std_error = c(0.3956686, 41.15942),
  t_value = c(-0.97, 8.90),
  p_value = c(0.335, 0.000),
  "95porc. conf" = c(-1.168151, 284.5402),
  intervalo = c(0.4009244, 447.7633)
)


mi_tabla <- flextable(resultado) %>%
  set_header_labels(p_valor = "p-valor", Estadistico = "Estadístico Z(t)") %>%
  theme_booktabs() %>%      # Estilo profesional de libro
  autofit() %>%             # Ajustar tamaño de celdas
  bold(part = "header")     # 

#==============================================================================#
# devolucion del VAR
#==============================================================================#
regresion_1 <- data.frame(
  variable = c("dif_itcrm_des_L2",
               "dif_real_des_L2",
               "dif_emae_des_L2",
               "dif_construya_des_L2",
               "_cons"),
  coefficient = c(-0.0441521,
                  -110.5675,
                  -1.648324,
                  -0.0000235,
                  -0.434131),
  std_error = c(0.5829108,
                227.934,
                0.9809622,
                0.1389707,
                2.688191),
  t_value = c(-0.08,
              -0.49,
              -1.68,
              -0.00,
              -0.16),
  p_value = c(0.940,
              0.628,
              0.093,
              1.000,
              0.872),
  ci_lower_95 = c(-1.186636,
                  -557.3098,
                  -3.570974,
                  -0.2724012,
                  -5.702888),
  ci_upper_95 = c(1.098332,
                  336.1748,
                  0.2743269,
                  0.2723541,
                  4.834626)
)


mi_tabla <- flextable(regresion_1) %>%
  set_header_labels(p_valor = "p-valor", Estadistico = "Estadístico Z(t)") %>%
  theme_booktabs() %>%      # Estilo profesional de libro
  autofit() %>%             # Ajustar tamaño de celdas
  bold(part = "header")     # 





regresion_2 <- data.frame(
  variable = c("dif_itcrm_des_L2","dif_real_des_L2","dif_emae_des_L2","dif_construya_des_L2","_cons"),
  coefficient = c(-0.1296287, 96.37441,-0.5455058, 0.0661906,0.1192727),
  std_error = c(0.1136804,44.45214, 0.1913092,0.0271024, 0.5242563),
  z_value = c(-1.14,2.17, -2.85,2.44,0.23),
  p_value = c(0.254, 0.030,0.004, 0.015, 0.820),
  ci_lower_95 = c(-0.3524382,9.249821,-0.9204649, 0.0130709, -0.9082509),
  ci_upper_95 = c(0.0931809,
                  183.499,
                  -0.1705466,
                  0.1193102,
                  1.146796)
)

regresion_2



mi_tabla <- flextable(regresion_2) %>%
  set_header_labels(p_valor = "p-valor", Estadistico = "Estadístico Z(t)") %>%
  theme_booktabs() %>%      # Estilo profesional de libro
  autofit() %>%             # Ajustar tamaño de celdas
  bold(part = "header")     # 



df_stats <- data.frame(
  Lag = 0:12,
  LL = c(-698.161, -680.564, -647.903, -633.414, -626.413, -614.836, 
         -605.142, -598.256, -579.37, -560.928, -538.749, -530.923, -516.025),
  LR = c(NA, 35.193, 65.322, 28.978, 14.003, 23.155, 19.387, 
         13.771, 37.773, 36.885, 44.358, 15.652, 29.797),
  df = c(NA, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16),
  p = c(NA, 0.004, 0.000, 0.024, 0.599, 0.110, 0.249, 0.616, 
        0.002, 0.002, 0.000, 0.477, 0.019),
  FPE = c(36.1648, 34.9709, "24.5812*", 25.506, 31.1661, 34.7435, 
          40.6062, 50.8326, 49.8374, 49.9843, 47.048, 61.2729, 70.2786),
  AIC = c(14.9396, 14.9056, "14.5511*", 14.5833, 14.7747, 14.8688, 
          15.003, 15.1969, 15.1355, 15.0836, 14.9521, 15.126, 15.1495),
  HQIC = c(14.9833, 15.1242, "14.9446*", 15.1516, 15.5179, 15.7869, 
           16.0959, 16.4647, 16.5781, 16.701, 16.7444, 17.0932, 17.2915),
  SBIC = c("15.0478*", 15.4467, 15.5252, 15.9902, 16.6146, 17.1416, 
           17.7087, 18.3355, 18.707, 19.0879, 19.3893, 19.9962, 20.4525)
)

mi_tabla <- flextable(df_stats) %>%
  set_header_labels(p_valor = "p-valor", Estadistico = "Estadístico Z(t)") %>%
  theme_booktabs() %>%      # Estilo profesional de libro
  autofit() %>%             # Ajustar tamaño de celdas
  bold(part = "header")     


