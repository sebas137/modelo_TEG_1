library(WDI)
library(dplyr)

# primero aquí haré mi construccion del data frame 

data_1 <- WDI( country = "all",
               indicator = c("BN.CAB.XOKA.CD",
                             "BX.KLT.DINV.WD.GD.ZS",
                             "NY.GDP.MKTP.KD.ZG",
                             "NE.GDI.TOTL.ZS",
                             "FR.INR.RINR",
                             "GC.TAX.TOTL.GD.ZS",
                             "CC.EST",                       
                             "GE.EST",                       
                             "PV.EST",                       
                             "RQ.EST",                       
                             "RL.EST",                       
                             "VA.EST"),
               
               start = 1996,
               end   = 2023,
               extra = FALSE,
               cache = NULL,
               latest = NULL,
               language = "en")

###cambie los nombres pra identificarlos con mayor facilidad

data_1 <- data_1 %>%
  rename(
    BN_balanza_cuenta_corriente = BN.CAB.XOKA.CD,
    BX_inversion_extranjera_directa = BX.KLT.DINV.WD.GD.ZS,
    NY_crecimiento_pib = NY.GDP.MKTP.KD.ZG,
    NE_formacion_bruta_capital = NE.GDI.TOTL.ZS,
    FR_tasa_interes_real = FR.INR.RINR,
    CC_control_corrupcion = CC.EST,
    GE_efectividad_gobierno = GE.EST,
    PV_estabilidad_politica = PV.EST,
    RQ_calidad_regulatoria = RQ.EST,
    RL_estado_de_derecho = RL.EST,
    VA_voz_y_rendicion_cuentas = VA.EST
  )



### Filtré los datos para los países de interés (América Latina y OCDE)
paises_latam <- c("ARG", "BRA", "CHL", "COL", "CRI", "ECU", "MEX", "PER", "URY", "VEN")
paises_ocde <- c("CAN", "USA", "DEU", "ESP", "FRA", "ITA", "PRT", "KOR")

### Creé un subset de datos
data_latam <- data_1[data_1$iso3c %in% paises_latam,]
data_ocde <- data_1[data_1$iso3c %in% paises_ocde,]
data_total <- rbind(data_latam, data_ocde)



### use random forest para la imputación de datos

library(missForest)


### Seleccioné las variables para la imputación (incluyendo los 6 WGI + tasa interés)
vars_imputar <- c("CC_control_corrupcion", "GE_efectividad_gobierno", "PV_estabilidad_politica",
                  "RQ_calidad_regulatoria", "RL_estado_de_derecho", "VA_voz_y_rendicion_cuentas", 
                  "FR_tasa_interes_real","BX_inversion_extranjera_directa","NY_crecimiento_pib","NE_formacion_bruta_capital","BN_balanza_cuenta_corriente")

set.seed(123)
imputacion_rf <- missForest(data_total[, vars_imputar])

### Extrajé los datos imputados
data_imputada_rf <- imputacion_rf$ximp

### chequee el error de imputación que me da muy alto con un valor de 0.5797
print(imputacion_rf$OOBerror)





### Normalicé los  datos (Random Forest es invariante a escala, pero ayuda numéricamente)
data_normalized <- scale(data_total[, vars_imputar])

### Imputar y luego revertir escala
imputacion_rf_norm <- missForest(data_normalized)
data_imputada_norm <- imputacion_rf_norm$ximp
data_imputada_raw <- t(apply(data_imputada_norm, 1, function(x) x * attr(data_normalized, 'scaled:scale') + attr(data_normalized, 'scaled:center')))



### Convertir a dataframe y asignar nombres de columnas
data_imputada_raw <- as.data.frame(data_imputada_raw)
colnames(data_imputada_raw) <- vars_imputar  # Asegurar que los nombres se mantengan

### Chequee summary estadístico
summary(data_imputada_raw)

### Eliminé las columnas originales (no imputadas) del dataset total
data_total_clean <- data_total %>% 
  select(-all_of(vars_imputar))

### Combiné con los datos imputados
data_total_imputed <- bind_cols(data_total_clean, data_imputada_raw)



library(FactoMineR)

# Sahora solo tengo que selecionar los 6 indicadores de gobernanza imputados
wgi_imputado <- data_total_imputed[, c("CC_control_corrupcion", "GE_efectividad_gobierno", "PV_estabilidad_politica",
                  "RQ_calidad_regulatoria", "RL_estado_de_derecho", "VA_voz_y_rendicion_cuentas")]

# Calcular PCA
pca_result <- PCA(wgi_imputado, graph = FALSE)
data_total_imputed$gobernanza <- pca_result$ind$coord[, 1]  # Primer componente

# Verificar varianza explicada
summary(pca_result)$eig



library(ggplot2)

# Comparar distribuciones antes/después 
ggplot() +
  geom_density(data = data_total, aes(x = CC_control_corrupcion, color = "Original")) +
  geom_density(data = data_total_imputed, aes(x = CC_control_corrupcion, color = "Imputado")) +
  labs(title = "Distribución de control de corrupcion: Original vs Imputado")


ggplot() +
  geom_density(data = data_total, aes(x = GE_efectividad_gobierno, color = "Original")) +
  geom_density(data = data_total_imputed, aes(x = GE_efectividad_gobierno, color = "Imputado")) +
  labs(title = "Distribución de efectividad de gobierno: Original vs Imputado")


ggplot() +
  geom_density(data = data_total, aes(x = PV_estabilidad_politica, color = "Original")) +
  geom_density(data = data_total_imputed, aes(x = PV_estabilidad_politica, color = "Imputado")) +
  labs(title = "Distribución de estabilidad politica: Original vs Imputado")

ggplot() +
  geom_density(data = data_total, aes(x = RQ_calidad_regulatoria, color = "Original")) +
  geom_density(data = data_total_imputed, aes(x = RQ_calidad_regulatoria, color = "Imputado")) +
  labs(title = "Distribución de calidad regulatoria: Original vs Imputado")


ggplot() +
  geom_density(data = data_total, aes(x = RL_estado_de_derecho, color = "Original")) +
  geom_density(data = data_total_imputed, aes(x = RL_estado_de_derecho, color = "Imputado")) +
  labs(title = "Distribución de estado de derecho: Original vs Imputado")

ggplot() +
  geom_density(data = data_total, aes(x = VA_voz_y_rendicion_cuentas, color = "Original")) +
  geom_density(data = data_total_imputed, aes(x = VA_voz_y_rendicion_cuentas, color = "Imputado")) +
  labs(title = "Distribución de voz y rendicion de cuentas: Original vs Imputado")

###modelo panel con datos imputados
library(plm)
modelo_fe <- plm(NE_formacion_bruta_capital ~  gobernanza + 
                   FR_tasa_interes_real  + NY_crecimiento_pib + BX_inversion_extranjera_directa,
                 data = data_total_imputed,
                 index = c("iso3c", "year"),
                 model = "within")

summary(modelo_fe)
