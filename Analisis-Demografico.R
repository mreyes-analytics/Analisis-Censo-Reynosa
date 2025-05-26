library(readr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(viridis)
library(forcats)
library(sf)
library(RColorBrewer)
library(leaflet)  


setwd("C:\\Users\\Miguel Reyes\\Desktop\\Supply Chain Data Science Project")

reynx_datos <- read_csv("conjunto_de_datos_ageb_urbana_28_cpv2020.csv")


reynx_datos <- reynx_datos %>% filter(NOM_MUN == "Reynosa")

#####Tibble por edades
reynx_loc_edades <- reynx_datos %>%
  select(LOC, NOM_LOC, AGEB, MZA, POBTOT, POBFEM, POBMAS, P_18YMAS, P_18YMAS_F, P_18YMAS_M, P_18A24, P_18A24_M, P_18A24_M, P_60YMAS,
         P_60YMAS_F, P_60YMAS_M,POB0_14, POB15_64, POB65_MAS, P_0A2_F, P_3A5_F, P_6A11_F, P_12A14_F, P_0A2_M, P_3A5_M, P_6A11_M, P_12A14_M) %>%
  filter(NOM_LOC != "Total AGEB urbana" & NOM_LOC != "Total de la localidad urbana") %>%
  mutate(
    POBMAS = as.numeric(gsub("\\*", "0", POBMAS)),  # Replace asterisks with 0
    POBFEM = as.numeric(gsub("\\*", "0", POBFEM)),   # Replace asterisks with 0
    POB65_MAS = as.numeric(gsub("\\*", "0", POB65_MAS)),   # Replace asterisks with 0
    POB15_64 = as.numeric(gsub("\\*", "0", POB15_64)),   # Replace asterisks with 0
    POB0_14 = as.numeric(gsub("\\*", "0", POB0_14)),   # Replace asterisks with 0
    P_0A2_F = as.numeric(gsub("\\*", "0", P_0A2_F)),  # Replace asterisks with 0
    P_3A5_F = as.numeric(gsub("\\*", "0", P_3A5_F)),   # Replace asterisks with 0
    P_6A11_F = as.numeric(gsub("\\*", "0", P_6A11_F)),   # Replace asterisks with 0
    P_12A14_F = as.numeric(gsub("\\*", "0", P_12A14_F)),   # Replace asterisks with 0
    P_0A2_M = as.numeric(gsub("\\*", "0", P_0A2_M)),  # Replace asterisks with 0
    P_3A5_M = as.numeric(gsub("\\*", "0", P_3A5_M)),   # Replace asterisks with 0
    P_6A11_M = as.numeric(gsub("\\*", "0", P_6A11_M)),   # Replace asterisks with 0
    P_12A14_M = as.numeric(gsub("\\*", "0", P_12A14_M))   # Replace asterisks with 0
    # Subtracting the sum of the specified columns from POBFEM and POBMAS
    )


# Crear el tibble de estado civil
reynx_estado_civil <- reynx_datos %>% filter(AGEB!="0000")%>% 
  mutate(across(c(P12YM_SOLT,P12YM_CASA,P12YM_SEPA),as.numeric))%>%# Asumiendo que tienes un dataframe con estos datos
  group_by(AGEB) %>%
  summarize(
    Personas_Solteras = sum(P12YM_SOLT, na.rm = TRUE),
    Personas_Casadas_Union = sum(P12YM_CASA, na.rm = TRUE),  # Casadas o en unión libre
    Personas_Separadas = sum(P12YM_SEPA, na.rm = TRUE)  # Separadas, divorciadas o viudas
  )


# Crear un dataframe de validación usando reynx_datos original
validacion_estado_civil <- reynx_datos %>%
  filter(AGEB != "0000") %>%
  mutate(
    # Convertir a numérico de manera segura
    P_12YMAS = as.numeric(as.character(P_12YMAS))
  ) %>%
  group_by(AGEB) %>%
  summarize(
    # Población total de 12 años y más según el campo directo
    Pob_12_Y_Mas = sum(P_12YMAS, na.rm = TRUE),
    
    # Suma por estado civil (también convertimos estas columnas a numéricas)
    Total_Estado_Civil = sum(as.numeric(as.character(P12YM_SOLT)), na.rm = TRUE) + 
      sum(as.numeric(as.character(P12YM_CASA)), na.rm = TRUE) + 
      sum(as.numeric(as.character(P12YM_SEPA)), na.rm = TRUE)
  ) %>%
  mutate(
    Diferencia = Total_Estado_Civil - Pob_12_Y_Mas,
    Porcentaje_Cobertura = round((Total_Estado_Civil / Pob_12_Y_Mas) * 100, 2)
  )

# También podríamos unir esto con el dataframe de estado civil
validacion_completa_edades <- reynx_estado_civil %>%
  left_join(
    validacion_estado_civil,
    by = "AGEB"
  )


##Análisis por edad y género
##Contemplar edades menores a 14
Edades_interes <- reynx_loc_edades %>% 
  group_by(AGEB) %>% 
  summarise(
    Pob_Tot = sum(POBTOT),
    Pob_14 = sum(POB0_14),
    Pob_Tot_sub = sum(POBTOT) - sum(POB0_14),
    POBFEM_sub = sum(POBFEM) - sum(P_0A2_F + P_3A5_F + P_6A11_F + P_12A14_F),
    POBMAS_sub = sum(POBMAS) - sum(P_0A2_M + P_3A5_M + P_6A11_M + P_12A14_M),
    PoB15_64 = sum(POB15_64),
    Pob_65 = sum(POB65_MAS)
  )


##### Análisis por hogar y jefatura femenina o masculina

reynx_hogares <- reynx_datos %>% 
  select(ENTIDAD, NOM_LOC, NOM_ENT, MUN, LOC, AGEB, MZA, 
         TOTHOG, HOGJEF_F, HOGJEF_M, POBHOG, PHOGJEF_F, PHOGJEF_M) %>%
  filter(NOM_LOC != "Total AGEB urbana") 

reynx_hogares <- reynx_hogares %>%
  mutate(across(c(TOTHOG, HOGJEF_F, HOGJEF_M, POBHOG, PHOGJEF_F, PHOGJEF_M), 
                ~as.numeric(as.character(.))))

reynx_hogares_analisis <- reynx_hogares %>%
  group_by(AGEB) %>%
  summarize(
    Total_Hogares = sum(TOTHOG, na.rm = TRUE),
    Hogares_Jef_Fem = sum(HOGJEF_F, na.rm = TRUE),
    Hogares_Jef_Masc = sum(HOGJEF_M, na.rm = TRUE),
    Porc_Hogares_Jef_Fem = round(Hogares_Jef_Fem / Total_Hogares * 100, 2),
    Porc_Hogares_Jef_Masc = round(Hogares_Jef_Masc / Total_Hogares * 100, 2)
  )

## Análisis por vivienda
# Primero convertir las columnas necesarias a numéricas
reynx_vivienda_numerica <- reynx_datos %>%
  mutate(across(c(VPH_C_SERV, VPH_TINACO, VPH_EXCSA, 
                  VPH_2YMASD, VPH_3YMASC, 
                  VPH_REFRI, VPH_LAVAD, VPH_HMICRO,
                  VPH_AUTOM, VPH_MOTO, VPH_BICI,
                  VPH_PC, VPH_INTER, VPH_STVP, VPH_SPMVPI, VPH_CVJ,
                  VIVTOT, TVIVHAB, TVIVPAR, VIVPAR_HAB, VIVPAR_DES, OCUPVIVPAR), 
                as.numeric))

# Luego creamos el análisis por AGEB incluyendo los índices socioeconómicos
reynx_vivienda_completo <- reynx_vivienda_numerica %>%
  group_by(AGEB) %>%
  summarize(
    # Métricas de vivienda
    Total_Viviendas = sum(VIVTOT, na.rm = TRUE),
    Viviendas_Habitadas = sum(TVIVHAB, na.rm = TRUE),
    Viviendas_Particulares = sum(TVIVPAR, na.rm = TRUE),
    Viviendas_Part_Habitadas = sum(VIVPAR_HAB, na.rm = TRUE),
    Viviendas_Part_Deshabitadas = sum(VIVPAR_DES, na.rm = TRUE),
    Total_Ocupantes = sum(OCUPVIVPAR, na.rm = TRUE),
    Porc_Viviendas_Deshabitadas = round(Viviendas_Part_Deshabitadas / Viviendas_Particulares * 100, 2),
    Promedio_Ocupantes = round(Total_Ocupantes / Viviendas_Part_Habitadas, 2),
    
    # Sumatoria de los indicadores por AGEB
    VPH_C_SERV_total = sum(VPH_C_SERV, na.rm = TRUE),
    VPH_TINACO_total = sum(VPH_TINACO, na.rm = TRUE),
    VPH_EXCSA_total = sum(VPH_EXCSA, na.rm = TRUE),
    VPH_2YMASD_total = sum(VPH_2YMASD, na.rm = TRUE),
    VPH_3YMASC_total = sum(VPH_3YMASC, na.rm = TRUE),
    VPH_REFRI_total = sum(VPH_REFRI, na.rm = TRUE),
    VPH_LAVAD_total = sum(VPH_LAVAD, na.rm = TRUE),
    VPH_HMICRO_total = sum(VPH_HMICRO, na.rm = TRUE),
    VPH_AUTOM_total = sum(VPH_AUTOM, na.rm = TRUE),
    VPH_MOTO_total = sum(VPH_MOTO, na.rm = TRUE),
    VPH_BICI_total = sum(VPH_BICI, na.rm = TRUE),
    VPH_PC_total = sum(VPH_PC, na.rm = TRUE),
    VPH_INTER_total = sum(VPH_INTER, na.rm = TRUE),
    VPH_STVP_total = sum(VPH_STVP, na.rm = TRUE),
    VPH_SPMVPI_total = sum(VPH_SPMVPI, na.rm = TRUE),
    VPH_CVJ_total = sum(VPH_CVJ, na.rm = TRUE)
  ) %>%
  # Calcular porcentajes y los índices con base en las viviendas particulares habitadas
  mutate(
    Porc_Viviendas_Deshabitadas = round(Viviendas_Part_Deshabitadas / Viviendas_Particulares * 100, 2),
    Promedio_Ocupantes = round(Total_Ocupantes / Viviendas_Part_Habitadas, 2),
    
    # Cálculo de índices (proporción de viviendas con cada característica)
    IIB = (VPH_C_SERV_total + VPH_TINACO_total + VPH_EXCSA_total) / Viviendas_Part_Habitadas,
    IEH = (VPH_2YMASD_total + VPH_3YMASC_total) / Viviendas_Part_Habitadas,
    IBD = (VPH_REFRI_total + VPH_LAVAD_total + VPH_HMICRO_total) / Viviendas_Part_Habitadas,
    IM = (VPH_AUTOM_total + VPH_MOTO_total + VPH_BICI_total) / Viviendas_Part_Habitadas,
    ITE = (VPH_PC_total + VPH_INTER_total + VPH_STVP_total + VPH_SPMVPI_total + VPH_CVJ_total) / Viviendas_Part_Habitadas
    )

percentiles <- apply(reynx_vivienda_completo[, c("IIB", "IEH", "IBD", "IM", "ITE")], 2, function(x) quantile(x, c(0.25, 0.5, 0.75), na.rm = TRUE))
mean_values <- colMeans(reynx_vivienda_completo[, c("IIB", "IEH", "IBD", "IM", "ITE")], na.rm = TRUE)
sd_values <- apply(reynx_vivienda_completo[, c("IIB", "IEH", "IBD", "IM", "ITE")], 2, sd, na.rm = TRUE)


# Calcular los percentiles y valores estadísticos
reynx_vivienda_completo <- reynx_vivienda_completo %>% mutate(Nivel_Socioeconomico = case_when(
                                     IIB > percentiles[3, "IIB"] & IEH > percentiles[3, "IEH"] & IBD > percentiles[3, "IBD"] & IM > percentiles[3, "IM"] & ITE > percentiles[3, "ITE"] ~ "Clase Alta",
                                     IIB > percentiles[2, "IIB"] & IEH > percentiles[2, "IEH"] & IBD > percentiles[2, "IBD"] & IM > percentiles[2, "IM"] & ITE > percentiles[2, "ITE"] ~ "Clase Media Alta",
                                     IIB > percentiles[1, "IIB"] & IEH > percentiles[1, "IEH"] & IBD > percentiles[1, "IBD"] & IM > percentiles[1, "IM"] & ITE > percentiles[1, "ITE"] ~ "Clase Media",
                                     TRUE ~ "Baja"))


##Agrupación por ageb, quitando las viviendas totales en cero

reynx_vivienda_analisis_no0 <- reynx_vivienda_completo %>% filter(AGEB!="0000" & Total_Viviendas>0)%>% arrange(desc(Total_Viviendas))


summary(reynx_vivienda_analisis_no0[, c("IIB", "IEH", "IBD", "IM", "ITE")])

# Gráfico de barras para ver la distribución de Nivel_Socioeconomico
ggplot(reynx_vivienda_analisis_no0, 
       aes(x = fct_infreq(Nivel_Socioeconomico))) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(x = "Nivel Socioeconómico", y = "Frecuencia", 
       title = "Distribución de Nivel Socioeconómico") +
  theme_minimal()

  # Convertir Nivel_Socioeconomico a factor ordenado con tus niveles específicos
  reynx_vivienda_analisis_no0 <- reynx_vivienda_analisis_no0 %>%
  mutate(Nivel_Socioeconomico = factor(Nivel_Socioeconomico,
                                       levels = c("Baja", "Clase Media", "Clase Media Alta", "Clase Alta"),                                       ordered = TRUE))

# También actualiza el dataframe para los gráficos individuales
reynx_vivienda_completo <- reynx_vivienda_completo %>%
  mutate(Nivel_Socioeconomico = factor(Nivel_Socioeconomico,
                                       levels = c("Baja", "Clase Media", "Clase Media Alta", "Clase Alta"),
                                       ordered = TRUE))

# Crear indices_long con el factor ya ordenado
indices_long <- reynx_vivienda_analisis_no0 %>%
  select(AGEB, Nivel_Socioeconomico, IIB, IEH, IBD, IM, ITE) %>%
  pivot_longer(cols = c(IIB, IEH, IBD, IM, ITE),
               names_to = "Indice", 
               values_to = "Valor") %>%
  mutate(Indice = factor(Indice, levels = c("IIB", "IEH", "IBD", "IM", "ITE"),
                         labels = c("Infraestructura Básica", 
                                    "Espacio Habitacional", 
                                    "Bienes Duraderos", 
                                    "Movilidad", 
                                    "Tecnología y Entretenimiento")))

# Verificar que el factor esté ordenado correctamente
levels(reynx_vivienda_analisis_no0$Nivel_Socioeconomico)

##---------------------------------------------------------------------------------
#Comportamiento nivel socioeconómico

# Crear el boxplot facetado
ggplot(indices_long, aes(x = Nivel_Socioeconomico, y = Valor, fill = Nivel_Socioeconomico)) +
  geom_boxplot() +
  facet_wrap(~ Indice, scales = "free_y") +
  labs(title = "Distribución de Índices Socioeconómicos por Nivel",
       subtitle = "Análisis por AGEB",
       x = "Nivel Socioeconómico",
       y = "Valor del Índice (0-1)",
       caption = "Valores más altos indican mejor condición socioeconómica") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Matriz de gráficos de dispersión
#pairs(reynx_vivienda_completo[, c("IIB", "IEH", "IBD", "IM", "ITE")], 
#      col = as.numeric(factor(reynx_vivienda_completo$Nivel_Socioeconomico)),
#      main = "Relación entre Índices Socioeconómicos por AGEB")

####Indices por separado

# Boxplot para IIB
# ggplot(reynx_vivienda_completo, aes(x = Nivel_Socioeconomico, y = IIB, fill = Nivel_Socioeconomico)) +
#   geom_boxplot() +
#   labs(title = "Índice de Infraestructura Básica por Nivel Socioeconómico",
#        subtitle = "Servicios esenciales: agua, drenaje, electricidad, sanitario",
#        x = "Nivel Socioeconómico",
#        y = "Valor del Índice (0-1)",
#        caption = "Valores más altos indican mejor acceso a servicios básicos") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "none")
# 
# # Boxplot para IEH
# ggplot(reynx_vivienda_completo, aes(x = Nivel_Socioeconomico, y = IEH, fill = Nivel_Socioeconomico)) +
#   geom_boxplot() +
#   labs(title = "Índice de Espacio Habitacional por Nivel Socioeconómico",
#        subtitle = "Amplitud de viviendas: 2+ dormitorios, 3+ cuartos",
#        x = "Nivel Socioeconómico",
#        y = "Valor del Índice (0-1)",
#        caption = "Valores más altos indican viviendas más espaciosas") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "none")
# 
# # Boxplot para IBD
# ggplot(reynx_vivienda_completo, aes(x = Nivel_Socioeconomico, y = IBD, fill = Nivel_Socioeconomico)) +
#   geom_boxplot() +
#   labs(title = "Índice de Bienes Duraderos por Nivel Socioeconómico",
#        subtitle = "Presencia de electrodomésticos: refrigerador, lavadora, microondas",
#        x = "Nivel Socioeconómico",
#        y = "Valor del Índice (0-1)",
#        caption = "Valores más altos indican mayor presencia de electrodomésticos") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "none")
# 
# # Boxplot para IM
# ggplot(reynx_vivienda_completo, aes(x = Nivel_Socioeconomico, y = IM, fill = Nivel_Socioeconomico)) +
#   geom_boxplot() +
#   labs(title = "Índice de Movilidad por Nivel Socioeconómico",
#        subtitle = "Medios de transporte: automóvil, motocicleta, bicicleta",
#        x = "Nivel Socioeconómico",
#        y = "Valor del Índice (0-1)",
#        caption = "Valores más altos indican mayor capacidad de movilidad") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "none")
# 
# # Boxplot para ITE
# ggplot(reynx_vivienda_completo, aes(x = Nivel_Socioeconomico, y = ITE, fill = Nivel_Socioeconomico)) +
#   geom_boxplot() +
#   labs(title = "Índice de Tecnología y Entretenimiento por Nivel Socioeconómico",
#        subtitle = "Acceso a: internet, TV paga, streaming, consola, computadora",
#        x = "Nivel Socioeconómico",
#        y = "Valor del Índice (0-1)",
#        caption = "Valores más altos indican mayor acceso a tecnología y entretenimiento") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "none")


# Categorizar las AGEBs según su porcentaje de desocupación
reynx_ageb <- reynx_vivienda_analisis_no0 %>%
    mutate(
      nivel_desocupacion = case_when(
        Porc_Viviendas_Deshabitadas <= 10 ~ "Baja",
        Porc_Viviendas_Deshabitadas > 10 & Porc_Viviendas_Deshabitadas < 30 ~ "Media",
        Porc_Viviendas_Deshabitadas >= 30 & Porc_Viviendas_Deshabitadas < 50 ~ "Alta",
        Porc_Viviendas_Deshabitadas >= 50 ~ "Muy Alta",
        TRUE ~ NA_character_ # Para manejar posibles valores fuera de los rangos definidos
      ),
      
    # Convertir a factor ordenado
    nivel_desocupacion = factor(nivel_desocupacion, 
                                levels = c("Baja", "Media", "Alta", "Muy alta"))
  )

# Calcular promedio de ocupantes por vivienda como indicador de hacinamiento
reynx_ageb <- reynx_vivienda_completo %>% filter(AGEB != "0000") %>%
  mutate(
    # Agregar más indicadores según los datos disponibles
    densidad_habitantes = Total_Ocupantes / Total_Viviendas,
    tasa_ocupacion = Viviendas_Habitadas / Total_Viviendas * 100
  )


# Visualización: Relación entre desocupación y promedio de ocupantes
plot_ocupantes_desocupacion <- ggplot(reynx_ageb, 
                                      aes(x = Promedio_Ocupantes, y = densidad_habitantes)) +
  geom_point(aes(size = Total_Viviendas, color = tasa_ocupacion)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Relación entre ocupantes promedio y densidad",
       x = "Promedio de ocupantes por vivienda habitada", 
       y = "% de Densidad poblacional",
       color = "Tasa de Ocupación", 
       size = "Total de viviendas") +
  scale_size_continuous(labels = scales::comma) +
  scale_color_viridis_c()


# #################################################################### Análisis economico poblacional

reynx_economico <- reynx_datos %>%
  select(ENTIDAD, NOM_ENT, MUN, LOC, NOM_LOC, AGEB, MZA,
         PEA, PEA_F, PEA_M, PE_INAC, PE_INAC_F, PE_INAC_M, POCUPADA, PDESOCUP) %>%
  filter(NOM_LOC != "Total AGEB urbana")

# Convertir columnas a numéricas
reynx_datos_filtrados <- reynx_datos %>%
  filter(NOM_LOC != "Total del municipio" &
           NOM_LOC != "Total de la localidad urbana" &
           NOM_LOC != "Total AGEB urbana")

# # Recalcular económico
reynx_economico_corregido <- reynx_datos_filtrados %>%
  select(ENTIDAD, NOM_ENT, MUN, LOC, NOM_LOC, AGEB, MZA,
         PEA, PEA_F, PEA_M, PE_INAC, PE_INAC_F, PE_INAC_M, POCUPADA, PDESOCUP) %>%
  mutate(across(c(PEA, PEA_F, PEA_M, PE_INAC, PE_INAC_F, PE_INAC_M, POCUPADA, PDESOCUP),
                ~as.numeric(as.character(.))))

#suma_pea_corregida <- sum(reynx_economico_corregido$PEA, na.rm = TRUE)

## Análisis economico poblacional corregido
reynx_economico_analisis_corregido <- reynx_economico_corregido %>%
  group_by(AGEB) %>%
  summarize(
    PEA_Total = sum(PEA, na.rm = TRUE),
    PEA_Femenina = sum(PEA_F, na.rm = TRUE),
    PEA_Masculina = sum(PEA_M, na.rm = TRUE),
    Pob_Inactiva = sum(PE_INAC, na.rm = TRUE),
    Pob_Inactiva_F = sum(PE_INAC_F, na.rm = TRUE),
    Pob_Inactiva_M = sum(PE_INAC_M, na.rm = TRUE),
    Pob_Ocupada = sum(POCUPADA, na.rm = TRUE),
    Pob_Desocupada = sum(PDESOCUP, na.rm = TRUE),

    # Calculando porcentajes
    Tasa_Participacion = round(PEA_Total / (PEA_Total + Pob_Inactiva) * 100, 2),
    Tasa_Participacion_F = round(PEA_Femenina / (PEA_Femenina + Pob_Inactiva_F) * 100, 2),
    Tasa_Participacion_M = round(PEA_Masculina / (PEA_Masculina + Pob_Inactiva_M) * 100, 2),
    Tasa_Desempleo = round(Pob_Desocupada / PEA_Total * 100, 2),
    Brecha_Participacion = round(Tasa_Participacion_M - Tasa_Participacion_F, 2)
  )


reynx_economico_depurado <- reynx_economico_analisis_corregido %>%
  select(
    AGEB,
    PEA_Total,
    PEA_Femenina,
    PEA_Masculina,
    Tasa_Participacion,
    Tasa_Participacion_F,
    Tasa_Participacion_M,
    Brecha_Participacion
  )

## Hogares X Viviendas X Edades X PEA
reyno_analisis_completo <- Edades_interes %>%
  left_join(reynx_hogares_analisis, by = "AGEB") %>%
  left_join(reynx_ageb, by = "AGEB") %>%
  left_join(reynx_economico_analisis_corregido, by = "AGEB") %>%
  left_join(reynx_estado_civil, by = "AGEB")

reyno_analisis_actualizado <- reyno_analisis_completo %>%
  left_join(
    # Seleccionamos solo las columnas nuevas que no están ya en el análisis completo
    reynx_vivienda_completo %>%
      select(
        AGEB,
        # Índices calculados
        IIB, IEH, IBD, IM, ITE,
        # Indicadores específicos por vivienda
        VPH_C_SERV_total, VPH_TINACO_total, VPH_EXCSA_total,
        VPH_2YMASD_total, VPH_3YMASC_total,
        VPH_REFRI_total, VPH_LAVAD_total, VPH_HMICRO_total,
        VPH_AUTOM_total, VPH_MOTO_total, VPH_BICI_total,
        VPH_PC_total, VPH_INTER_total, VPH_STVP_total, VPH_SPMVPI_total, VPH_CVJ_total,
        # Clasificación socioeconómica
        Nivel_Socioeconomico
      ),
    by = "AGEB"
  )

reyno_analisis_completo <- reyno_analisis_actualizado %>%
 left_join(
   validacion_completa_edades %>%
     select(AGEB, Pob_12_Y_Mas, Porcentaje_Cobertura),
   by = "AGEB"
 )


reyno_analisis_completo <- reyno_analisis_completo %>% filter(AGEB!="0000",Tasa_Participacion!="NaN")%>% select(-c(VPH_C_SERV_total,VPH_2YMASD_total,VPH_LAVAD_total,
                                                                 VPH_MOTO_total,VPH_INTER_total,VPH_CVJ_total, VPH_TINACO_total,VPH_3YMASC_total,VPH_HMICRO_total,VPH_STVP_total,
                                                                 VPH_EXCSA_total,VPH_REFRI_total,VPH_AUTOM_total,VPH_PC_total,VPH_SPMVPI_total)) %>%arrange(desc(PEA_Total))

reyno_analisis_completo <- reyno_analisis_completo %>%
  select(
    -ends_with(".y")    # Elimina todas las columnas duplicadas con .y
  ) %>%
  rename_with(~gsub("\\.x$", "", .), ends_with(".x")) # Quita el sufijo .x a las columnas

# Revisa las columnas finales
colnames(reyno_analisis_completo)


# --- Indicadores Económicos: Población Económicamente Activa (PEA) y tasas ---

colnames(reynx_datos)
reynx_economico <- reynx_datos %>%
  select(
    AGEB, PEA, PEA_M, PEA_F,
    P_12YMAS, P_12YMAS_M, P_12YMAS_F,
    PDESOCUP, PDESOCUP_M, PDESOCUP_F
  ) %>%
  mutate(across(everything(), ~as.numeric(gsub("\\*", "0", .))))

reynx_economico_depurado <- reynx_economico %>%
  group_by(AGEB) %>%
  summarize(
    PEA = sum(PEA, na.rm = TRUE),
    Tasa_Participacion = round(sum(PEA, na.rm = TRUE) / sum(P_12YMAS, na.rm = TRUE) * 100, 2),
    Tasa_Desempleo = round(sum(PDESOCUP, na.rm = TRUE) / sum(PEA, na.rm = TRUE) * 100, 2),
    Tasa_Part_M = round(sum(PEA_M, na.rm = TRUE) / sum(P_12YMAS_M, na.rm = TRUE) * 100, 2),
    Tasa_Part_F = round(sum(PEA_F, na.rm = TRUE) / sum(P_12YMAS_F, na.rm = TRUE) * 100, 2)
  )


reynx_hogares_analisis        <- reynx_hogares_analisis        %>% mutate(AGEB = as.character(AGEB))
reynx_vivienda_analisis_no0   <- reynx_vivienda_analisis_no0   %>% mutate(AGEB = as.character(AGEB))
reynx_economico_depurado      <- reynx_economico_depurado      %>% mutate(AGEB = as.character(AGEB))


# --- Integración Final de Indicadores ---
reynx_analisis_completo <- reynx_hogares_analisis %>%
  left_join(reynx_vivienda_analisis_no0, by = "AGEB") %>%
  left_join(Edades_interes, by = "AGEB") %>%
  left_join(reynx_economico_depurado, by = "AGEB")

--------------------------------------------------------------------------------------------------------
################

# Definir un sistema de puntuación para los AGEBs basado en múltiples criterios - versión optimizada
reyno_ageb_potencial <- reyno_analisis_completo %>%
  # Paso 1: Calcular métricas adicionales relevantes
  mutate(
    # Densidad habitacional (personas por vivienda)
    Densidad_Habitacional = Pob_Tot / Viviendas_Part_Habitadas,
    
    # Proporción de PEA en la población total
    Prop_PEA = PEA_Total / Pob_Tot,
    
    
    # Tasa de ocupación (% de la PEA que está ocupada)
    #Tasa_Ocupacion = Pob_Ocupada / PEA_Total * 100,
    
    
    # Índice de potencial de mejoras habitacionales (NUEVO)
    # Valores más altos = más potencial para mejoras
      Indice_Potencial_Mejoras = (
        (1 - IIB) * 0.40 +           # 40% a carencias de infraestructura básica
          (1 - IEH) * 0.30 +           # 30% a limitaciones de espacio habitacional
          IBD * 0.10 +                 # 10% a capacidad de consumo (bienes)
          IM * 0.10 +                  # 10% a movilidad (posibilidad económica)
          ITE * 0.10                   # 10% a tecnología (nivel socioeconómico)
    )
  ) %>%
  
  # Paso 2: Normalizar variables clave (escala 0-100)
  mutate(
    # Normalizar población total (más población = más mercado potencial)
    Score_Poblacion = 100 * (Pob_Tot - min(Pob_Tot, na.rm = TRUE)) / 
      (max(Pob_Tot, na.rm = TRUE) - min(Pob_Tot, na.rm = TRUE)),
    
    # Normalizar PEA (más PEA = más poder adquisitivo)
    Score_PEA = 100 * (PEA_Total - min(PEA_Total, na.rm = TRUE)) / 
      (max(PEA_Total, na.rm = TRUE) - min(PEA_Total, na.rm = TRUE)),
    
  
    # Normalizar tasa de ocupación de viviendas 
    Score_Ocupacion = 100 * (tasa_ocupacion - min(tasa_ocupacion, na.rm = TRUE)) / 
      (max(tasa_ocupacion, na.rm = TRUE) - min(tasa_ocupacion, na.rm = TRUE)),
    
    # Normalizar total de hogares (más viviendas = más unidades de consumo)
    Score_Viviendas = 100 * (Total_Viviendas - min(Total_Viviendas, na.rm = TRUE)) / 
      (max(Total_Viviendas, na.rm = TRUE) - min(Total_Viviendas, na.rm = TRUE)),
    
    # Normalizar promedio de ocupantes (más personas por hogar = mayor consumo por unidad)
    Score_Ocupantes = 100 * (Promedio_Ocupantes - min(Promedio_Ocupantes, na.rm = TRUE)) / 
      (max(Promedio_Ocupantes, na.rm = TRUE) - min(Promedio_Ocupantes, na.rm = TRUE)),
    
    # Normalizar índice de potencial de mejoras habitacionales (NUEVO)
    Score_Potencial_Mejoras = 100 * (Indice_Potencial_Mejoras - min(Indice_Potencial_Mejoras, na.rm = TRUE)) / 
      (max(Indice_Potencial_Mejoras, na.rm = TRUE) - min(Indice_Potencial_Mejoras, na.rm = TRUE)),
    
    # Convertir nivel socioeconómico a valor numérico (AJUSTADO PARA PRIORIZAR CLASE BAJA Y MEDIA BAJA)
   Score_NSE = case_when(
     Nivel_Socioeconomico == "Baja" ~ 100,             # Prioridad máxima
     Nivel_Socioeconomico == "Clase Media" ~ 70,       # Prioridad alta
     Nivel_Socioeconomico == "Clase Media Alta" ~ 45,  # Prioridad media
     Nivel_Socioeconomico == "Clase Alta" ~ 10,        # Prioridad baja
     TRUE ~ 0
   )
  ) %>%
  
  # Paso 3: Calcular puntaje total con ponderaciones personalizables AJUSTADAS
  mutate(
    # Ponderaciones redistribuidas según feedback
    Score_Total = (
      Score_Poblacion * 0.15 +           # 15% importancia a volumen poblacional
        Score_PEA * 0.18 +                 # 18% importancia a población económicamente activa (AUMENTADO)
        Score_Ocupacion * 0.12 +           # 12% importancia a tasa de ocupación vivienda (AUMENTADO)
        Score_Viviendas * 0.12 +             # 12% total viviendas (REDUCIDO)
        Score_Ocupantes * 0.08 +           # 10% importancia a tamaño promedio de los hogares
        Score_NSE * 0.20 +                 # 20% importancia al nivel socioeconómico
        Score_Potencial_Mejoras * 0.15     # 15% importancia al potencial de mejoras (NUEVO)
      
 #        Score_Pob_0_14 *  +            # 10% importancia a población de 0-14 años
 #       Score_Pob_Fem * 0.05 +             # 5% importancia a población femenina (NUEVO)
    )
  ) %>%
  
  # Paso 4: Ordenar por puntaje para obtener ranking de AGEBs potenciales
  arrange(desc(Score_Total))


head(reyno_ageb_potencial$Score_NSE)
summary_score_nse <- table(reyno_ageb_potencial$Score_NSE)
print(summary_score_nse)

# Crear categorías de potencial basado en quintiles
# Dividir Score_Total en quintiles y asignar categoría de potencial
reyno_ageb_categorizado <- reyno_ageb_potencial %>%
  mutate(
    Quintil_Potencial = ntile(Score_Total, 5),  # Dividir Score_Total en 5 grupos iguales (quintiles)
    Categoria_Potencial = case_when(
      Quintil_Potencial == 5 ~ "Muy Alto Potencial",    # Quintil 5 es el 20% superior
      Quintil_Potencial == 4 ~ "Alto Potencial",        # Quintil 4 es el siguiente 20%
      Quintil_Potencial == 3 ~ "Potencial Medio",       # Quintil 3 es el 20% medio
      Quintil_Potencial == 2 ~ "Bajo Potencial",        # Quintil 2 es el siguiente 20%
      Quintil_Potencial == 1 ~ "Muy Bajo Potencial"     # Quintil 1 es el 20% inferior
    )
  )

ggplot(reyno_ageb_potencial, aes(x = Score_Total)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +  # Ajusta binwidth según tus datos
  labs(title = "Distribución de Score_Total",
       x = "Score Total",
       y = "Frecuencia") +
  theme_minimal()


# Ver el resumen de las categorías para confirmar la distribución

reyno_ageb_categorizado <- reyno_ageb_categorizado %>% 
  filter(!is.na(Categoria_Potencial))
--------------------------------------------------------------
  ###Mapa con todos los agebs    26/03/2025
# Unir los datos de potencial con el shapefile

setwd("C:/Users/Miguel Reyes/Desktop/Supply Chain Data Science Project")

shp_path <- "C:/Users/Miguel Reyes/Desktop/Supply Chain Data Science Project/28_tamaulipas/conjunto_de_datos/AGEBS" # Ajusta según el archivo que quieras probar
mapa_reynosa <- st_read(shp_path) 
class(mapa_reynosa)


class(mapa_reynosa$CVE_MUN)

reynosa_map <- mapa_reynosa[mapa_reynosa$CVE_MUN == "032", ]
plot(reynosa_map["geometry"])

# Visualizar el mapa de Reynosa
  
mapa_reyno <- reynosa_map %>%
  left_join(reyno_ageb_categorizado, by = c("CVE_AGEB" = "AGEB"))


# Recrear la columna de categorías como factor con orden explícito
# Imprimir todos los valores únicos

# Eliminar NA antes de graficar
mapa_reyno <- mapa_reyno %>%
  filter(!is.na(Categoria_Potencial))

mapa_reyno$Categoria_Potencial <- factor(
  mapa_reyno$Categoria_Potencial, 
  levels = c(
    "Muy Bajo Potencial", 
    "Bajo Potencial", 
    "Potencial Medio", 
    "Alto Potencial", 
    "Muy Alto Potencial"
  )
)

# Mapa directo
plot(mapa_reyno["Categoria_Potencial"])

-------------------------------------------------
#Mapa leaflet 26/03/2025
  

# Transformar las coordenadas
mapa_reyno_transformado <- st_transform(mapa_reyno, crs = 4326)

# Preparar el mapa
mapa_reyno_leaflet <- mapa_reyno_transformado %>%
  mutate(
    popup_info = paste(
      "<strong>Información del AGEB:</strong><br>",
      "<strong>AGEB:</strong>", CVE_AGEB, "<br>",
      "<strong>Categoría de Potencial:</strong>", Categoria_Potencial, "<br>",
      "<strong>Score Total:</strong>", round(Score_Total, 2), "<br>",
      "<hr>",
      "<strong>Población Total:</strong>", Pob_Tot, "<br>",
      "<strong>Densidad de habitantes:</strong>", round(densidad_habitantes,2), "<br>",
      "<strong>Total Viviendas:</strong>", Total_Viviendas, "<br>",
      "<strong>Nivel Socioeconómico:</strong>", Nivel_Socioeconomico, "<br>",
      "<hr>",
      "<strong>Indicadores:</strong><br>",
      "<strong>Proporción PEA:</strong>", round(Prop_PEA,2), "<br>",
      "Tasa de Ocupación de Vivienda: ", round(tasa_ocupacion, 2), "%", "<br>",
      "Potencial de Mejoras: ", round(Indice_Potencial_Mejoras, 2)
    )
  )

# Definir paleta de colores personalizada
paleta_colores <- colorFactor(
  palette = c(
    "Muy Bajo Potencial" = "#f1f1f1",    # Gris muy claro
    "Bajo Potencial" = "#a1c5e8",        # Azul claro
    "Potencial Medio" = "#4682b4",       # Azul medio
    "Alto Potencial" = "#1e4d82",        # Azul oscuro
    "Muy Alto Potencial" = "#0b2c4a"     # Azul muy oscuro casi negro
  ),
  domain = mapa_reyno_leaflet$Categoria_Potencial
)

# Crear mapa interactivo
mapa_leaflet <- leaflet(mapa_reyno_leaflet) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addPolygons(
    fillColor = ~paleta_colores(Categoria_Potencial),
    color = "white",
    weight = 1,
    opacity = 1,
    fillOpacity = 0.7,
    popup = ~popup_info,
    highlight = highlightOptions(
      weight = 3,
      color = "red",
      fillOpacity = 0.7,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal = paleta_colores,
    values = ~Categoria_Potencial,
    title = "Potencial de AGEBs",
    opacity = 0.7
  ) %>%
  setView(
    lng = mean(st_coordinates(mapa_reyno_leaflet)[,1]), 
    lat = mean(st_coordinates(mapa_reyno_leaflet)[,2]), 
    zoom = 11
  )

# Mostrar el mapa basado en categoría potencial
mapa_leaflet


-------------------------------------------#Mapas leaflet potencial de categoría y score total
# Crear paleta de colores para Score_Total
paleta_score <- colorNumeric(
  palette = "YlOrRd",  # Paleta de amarillo a rojo
  domain = mapa_reyno_leaflet$Score_Total
)

# Preparar mapa con Score_Total
mapa_leaflet_score <- leaflet(mapa_reyno_leaflet) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addPolygons(
    fillColor = ~paleta_score(Score_Total),
    color = "white",
    weight = 1,
    opacity = 1,
    fillOpacity = 0.7,
    popup = ~popup_info,
    highlight = highlightOptions(
      weight = 3,
      color = "blue",
      fillOpacity = 0.7,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal = paleta_score,
    values = ~Score_Total,
    title = "Score Total de AGEBs",
    opacity = 0.7
  ) %>%
  setView(
    lng = mean(st_coordinates(mapa_reyno_leaflet)[,1]), 
    lat = mean(st_coordinates(mapa_reyno_leaflet)[,2]), 
    zoom = 11
  )

# Mostrar el mapa
mapa_leaflet_score
---------------------------------------------------
reynx_vivienda_resumen <- reynx_hogares %>%
  group_by(AGEB) %>%
  summarize(
    Total_Hogares = sum(TOTHOG, na.rm = TRUE)
  ) %>%
  left_join(
    reynx_vivienda_numerica %>%
      group_by(AGEB) %>%
      summarize(
        Total_Viviendas = sum(VIVTOT, na.rm = TRUE),
        Viviendas_Habitadas = sum(TVIVHAB, na.rm = TRUE),
        Viviendas_Particulares = sum(TVIVPAR, na.rm = TRUE),
        Viviendas_Part_Habitadas = sum(VIVPAR_HAB, na.rm = TRUE),
        Viviendas_Part_Deshabitadas = sum(VIVPAR_DES, na.rm = TRUE),
        Total_Ocupantes = sum(OCUPVIVPAR, na.rm = TRUE)
      ),
    by = "AGEB"
  ) %>%
  mutate(
    Porc_Viviendas_Deshabitadas = round(Viviendas_Part_Deshabitadas / Viviendas_Particulares * 100, 2),
    Promedio_Ocupantes = round(Total_Ocupantes / Viviendas_Part_Habitadas, 2)
  )

-----------------------
  # Comparar valores
summary(mapa_reyno$Total_Hogares)
summary(mapa_reyno$Viviendas_Habitadas)

# Mostrar algunas filas para ver la relación
mapa_reyno %>%
  select(CVE_AGEB, Total_Hogares, Viviendas_Habitadas) %>%
  head()
  
  
# Resumen estadístico para cada categoría de potencial
resumen_categorias <- reyno_ageb_categorizado %>%
  group_by(Categoria_Potencial) %>%
  summarize(
    Cantidad_AGEBs = n(),
    Poblacion_Total = sum(Pob_Tot, na.rm = TRUE),
    PEA_Total = sum(PEA_Total, na.rm = TRUE),
    Hogares_Total = sum(Total_Hogares, na.rm = TRUE),
    Pob_Ocupada_Total = sum(Pob_Ocupada, na.rm = TRUE),
    #Tasa_Ocupacion_Promedio = mean(Tasa_Ocupacion, na.rm = TRUE),
    Indice_Potencial_Mejoras_Promedio = mean(Indice_Potencial_Mejoras, na.rm = TRUE),
    Score_Promedio = mean(Score_Total, na.rm = TRUE)
    #Pob_0_14_Total = sum(Pob_14, na.rm = TRUE),
    #Poblacion_Femenina = sum(POBFEM_sub, na.rm = TRUE),
    
  ) %>%
  arrange(desc(Score_Promedio))

# Análisis de distribución de NSE en AGEBs de alto potencial
nse_top_agebs <- View(reyno_ageb_categorizado) %>%
  filter(Categoria_Potencial %in% c("Muy Alto Potencial", "Alto Potencial")) %>%
  group_by(Nivel_Socioeconomico) %>%
  summarize(
    Cantidad_AGEBs = n(),
    Pct_AGEBs = n() / nrow(filter(reyno_ageb_categorizado, 
                                  Categoria_Potencial %in% c("Muy Alto Potencial", "Alto Potencial"))) * 100,
    Poblacion_Total = sum(Pob_Tot, na.rm = TRUE),
    Pob_0_14_Total = sum(Pob_14, na.rm = TRUE),
    Poblacion_Femenina = sum(POBFEM_sub, na.rm = TRUE),
    Score_Promedio = mean(Score_Total, na.rm = TRUE)
  ) %>%
  arrange(desc(Cantidad_AGEBs))

# Características detalladas de los AGEBs de Alto Potencial
top_agebs_detalle <- reyno_ageb_categorizado %>%
  filter(Categoria_Potencial %in% c("Muy Alto Potencial", "Alto Potencial")) %>%
  select(
    AGEB, 
    Categoria_Potencial,
    Score_Total,
    Pob_Tot,
    Pob_14,
    POBFEM_sub,
    #Prop_Mujeres,
    PEA_Total,
    #Tasa_Ocupacion,
    Total_Hogares,
    Promedio_Ocupantes,
    Nivel_Socioeconomico,
    Indice_Potencial_Mejoras,
    IIB,
    IEH
  ) %>%
  arrange(desc(Score_Total))

view(top_agebs_detalle)

head(top_agebs_detalle, n=15)


colnames(top_agebs_detalle)
#IGNORAR-------------------------------------------------------------------------------------------------------------------
##Mezclar agebs detalle con archivo SHP
  
# top_agebs_detalle$AGEB <- as.character(top_agebs_detalle$AGEB)
# reynosa_map$CVE_AGEB <- as.character(reynosa_map$CVE_AGEB)
# 
# agebs_cross_shp <- reynosa_map %>%
#   left_join(top_agebs_detalle, by = c("CVE_AGEB" = "AGEB"))
# 
# ggplot(agebs_cross_shp) +
#   geom_sf(aes(fill = Score_Total)) +
#   scale_fill_viridis_c(option = "magma") +  # Puedes cambiar la escala de color
#   theme_minimal() +
#   labs(title = "Mapa de Reynosa - Score Total por AGEB",
#        fill = "Score Total")
# 
# 
# ggplot(agebs_cross_shp) +
#   geom_sf(aes(fill = Categoria_Potencial)) +
#   scale_fill_brewer(palette = "Set2") +  # Paleta de colores para categorías
#   theme_minimal() +
#   labs(title = "Mapa de Reynosa - Categoría de Potencial por AGEB",
#        fill = "Categoría Potencial")
# 
# # Definir una paleta de colores para Score_Total
# pal <- colorNumeric("YlOrRd", mapa_reynosa_wgs84$Score_Total, na.color = "transparent")
# 
# ### Leaflet sin los pop ups
# leaflet(mapa_reynosa_wgs84) %>%
#   addTiles() %>%
#   addPolygons(
#     fillColor = ~pal(Score_Total),
#     weight = 1,
#     opacity = 1,
#     color = "black",
#     fillOpacity = 0.7,
#     popup = ~paste0("AGEB: ", CVE_AGEB, "<br>Score Total: ", Score_Total)
#   ) %>%
#   addLegend(pal = pal, values = ~Score_Total, title = "Score Total", opacity = 1)
# 
# # Ver la proyección actual
# st_crs(agebs_cross_shp)
# 
# # Convertir a WGS84 (EPSG:4326)
# mapa_reynosa_wgs84 <- st_transform(agebs_cross_shp, crs = 4326)
# 
# ####Añadir valores pop ups con detalles de nuestros datos
# leaflet(mapa_reynosa_wgs84) %>%
#   addTiles() %>%
#   addPolygons(
#     fillColor = ~pal(Score_Total),
#     weight = 1,
#     opacity = 1,
#     color = "black",
#     fillOpacity = 0.7,
#     popup = ~paste0("<b>AGEB:</b> ", CVE_AGEB,
#                     "<br><b>Score Total:</b> ", Score_Total,
#                     "<br><b>Categoría:</b> ", Categoria_Potencial,
#                     "<br><b>Población Total:</b> ", Pob_Tot,
#                     "<br><b>Índice de Mejoras:</b> ", round(Indice_Potencial_Mejoras, 2)),
#     label = ~paste0("AGEB: ", CVE_AGEB, " | Score: ", Score_Total),
#     highlight = highlightOptions(
#       weight = 3,
#       color = "blue",
#       fillOpacity = 0.9,
#       bringToFront = TRUE
#     )
#   ) %>%
#   addLegend(pal = pal, values = ~Score_Total, title = "Score Total", opacity = 1)
-------------------------------------------------------------------------------------------------------
#Guardando archivos para el markdown
  # Guardar como CSV o RDS
  #write.csv(top_agebs_detalle, "top_agebs_detalle.csv", row.names = FALSE)
  #write.csv(reyno_ageb_categorizado, "reyno_ageb_categorizado", row.names = FALSE)
  
  
# Para archivos sf, usar st_write
#st_write(reynosa_map, "reynosa_map.shp")

#st_write(reynosa_map, "mapa_reyno.shp")


#View(reyno_analisis_final) <- reyno_analisis_completo %>%
 # left_join(reyno_ageb_categorizado, by = "AGEB")
#write.csv(reyno_analisis_final, "reyno_analisis_final.csv", row.names = FALSE)


###################
--------------------------------------------------------------------------------------------------------
#Analisis posteriores con solo el top 10 agebs
#colnames(reyno_ageb_categorizado)
  
#write.csv(reyno_ageb_categorizado, "reyno_ageb_categorizado.csv", row.names = FALSE)
