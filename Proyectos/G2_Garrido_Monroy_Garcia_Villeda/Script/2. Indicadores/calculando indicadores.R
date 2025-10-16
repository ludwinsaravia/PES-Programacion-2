# ----------------------------
# 1) Librerías y limpieza de entorno
# ----------------------------
graphics.off()
rm(list=ls())
cat("\014")


library(readxl)
library(dplyr)
library(stringr)
library(magrittr)
library(tidyverse)
library(lubridate)
library(readr)
library(writexl)

# ----------------------------
# 2) Cargas de datos
# ----------------------------
getwd()
setwd("/Users/stefanivilleda/Desktop/Programación II/Proyecto/programa-II/Proyecto/")
datos <- read_csv("output/personas_encovi.csv")

# ----------------------------
# 3) construyendo indicadores de acceso tecnologico por departamento
# ----------------------------


# Indicador 1: Proporción de personas que usan internet por departamento
#--------------------    
uso_intern <- datos %>%
    group_by(depto) %>%
    summarise(
        total_resp = sum(factor, na.rm = TRUE),
        usan_intern     = sum(if_else(uso_internet == 1, factor, 0), na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(pct_uso_internet = 100 * usan_intern / total_resp) %>%
    arrange(depto)
#variable
var_uso_intern <- round(uso_intern$pct_uso_internet,2)

# Indicador 2: Proporción de personas que usan internet por departamento y por genero
#--------------------    
uso_intern_mujeres <- datos %>%
    group_by(depto) %>%
    summarise(
        total_resp = sum(factor, na.rm = TRUE),
        usan_intern_mujeres     = sum(if_else(uso_internet == 1 & sexo == 2 , factor, 0), na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(pct_uso_internet_mujeres = 100 * usan_intern_mujeres / total_resp) %>%
    arrange(depto)
#variable
var_uso_intern_mujeres <- round(uso_intern_mujeres$pct_uso_internet_mujeres,2)

# Indicador 3: Acceso a telefono movil
#--------------------    
acceso_tel_mov <- datos %>%
    group_by(depto) %>%
    summarise(
        total_resp = sum(factor, na.rm = TRUE),
        pers_con_mov = sum(if_else(tiene_celular == 1 , factor, 0), na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(pct_tiene_celular = 100 * pers_con_mov / total_resp) %>%
    arrange(depto)
#variable
var_uso_acceso_tel_mov <- round(acceso_tel_mov$pct_tiene_celular,2)

# Indicador 4: porcentaje de población rural por departamento
#--------------------    
poblacion_rural <- datos %>%
    group_by(depto) %>%
    summarise(
        total_resp = sum(factor, na.rm = TRUE),
        poblacion_rural = sum(if_else(area == 2 , factor, 0), na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(pct_rural = 100 * poblacion_rural / total_resp) %>%
    arrange(depto)
#variable
pct_rural <- round(poblacion_rural$pct_rural,2)

#----------------------------
#base de datos hogares (ENCOVI)
#----------------------------
hogares_clean <- read_csv("output/hogares_clean.csv")
head(hogares_clean)

# Indicador 5: Proporción de hogares con acceso a internet
#----------------------------
hogares_internet <- hogares_clean %>%
  group_by(depto) %>%
  summarise(
    total_resp = sum(factor, na.rm = TRUE),
    hogares_internet     = sum(if_else(internet_residencial == 1, factor, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(pct_hogares_internet= 100 * hogares_internet / total_resp) %>%
  arrange(depto)

#Variable
propor_hogares_internet<- round(hogares_internet$pct_hogares_internet,2)

#indicador 6: Proporción de hogares con computadora
#----------------------------
hogares_compu <- hogares_clean %>%
  group_by(depto) %>%
  summarise(
    total_resp = sum(factor, na.rm = TRUE),
    hogares_compu     = sum(if_else(tiene_equipamiento == 1, factor, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(pct_hogares_compu = 100 * hogares_compu / total_resp) %>%
  arrange(depto)
#Variable
propor_hogares_computadora<- round(hogares_compu$pct_hogares_compu,2)

#----------------------------
#base de datos externas SIT (Radiobases)
#----------------------------
datos_rb <- read_csv("output/radiobases_lineas_telef.csv")
datos_pib <- read_csv("output/PIB_per_capita.csv")
head(datos_rb)


# ----------------------------------
# Indicador 7: radiobases por cada 100k
# Indicador 8: líneas fijas por cada 1k
# Indicador 9: PIB per capita
#----------------------------


df_radio_pib <- left_join(datos_rb, datos_pib, join_by(departamento))
df_radio_pib <- df_radio_pib |>
  mutate(
      rb_por_100k = round(radiobases_2023 / poblacion * 1e5,2),
      lineas_por_1k = round(lineas_fijas_2023 / poblacion * 1e3,2),
      pib_per_capita_rounded = round(PIB_per_capita, 2)
  )
df_radio_pib <- df_radio_pib |> select(rb_por_100k, 
                                       lineas_por_1k,
                                       pib_per_capita_rounded)
names(df_radio_pib) <- c("rb_por_100k", "lineas_por_1k", "pib_per_capita")


# ----------------------------
# 4) Guardando variables en un data frame
# ----------------------------
df_indicadores <- data.frame(depto = uso_intern$depto,
                             var_uso_intern,
                             var_uso_intern_mujeres,
                             var_uso_acceso_tel_mov,
                             pct_rural,
                             propor_hogares_internet,
                             propor_hogares_computadora)

df_indicadores <- cbind(df_indicadores, df_radio_pib)



# ----------------------------
# 5) Creando diccionario de variables 
# ----------------------------
diccionario_variables <- data.frame(
  variable = c("depto",
               "var_uso_intern",
               "var_uso_intern_mujeres",
               "var_uso_acceso_tel_mov",
               "pct_rural",
               "propor_hogares_internet",
               "propor_hogares_computadora",
               "rb_por_100k",
               "lineas_por_1k",
               "pib_per_capita"),
  descripcion = c("Departamento",
                  "Proporción de personas que usan internet por departamento",
                  "Proporción de mujeres que usan internet por departamento",
                  "Proporción de personas con acceso a teléfono móvil por departamento",
                  "Porcentaje de población rural por departamento",
                  "Proporción de hogares con acceso a internet por departamento",
                  "Proporción de hogares con computadora por departamento",
                  "Radiobases por cada 100k habitantes",
                  "Líneas fijas por cada 1k habitantes",
                  "PIB per capita")
)



# ----------------------------
# 5) Exportando en documento de excel xls
# ----------------------------
??write_xlsx


write_xlsx(
list("indicadores_basicos" = df_indicadores,
  "diccionario_variables" = diccionario_variables),
path = "output/indicadores_acceso_tecnologico.xlsx",format_headers = TRUE
)

# ----------------------------

