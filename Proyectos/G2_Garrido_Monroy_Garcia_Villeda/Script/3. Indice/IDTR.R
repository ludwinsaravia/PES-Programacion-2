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

getwd()
setwd("C:/Users/Javie/OneDrive/Escritorio/PROGRAMACIÓN II/programa-II/Proyecto/")
# ----------------------------
# 2) Cargas de datos
# ----------------------------
#leer excel con libreria readxl
datos <- read_excel("output/IDX.xlsx")
datos
# ----------------------------
# 3) Calculo de promedio ponderado del indice
# ---------------------------
ind_cols <- setdiff(names(datos), "depto")
ponderadores <- c(0.18, 0.10, 0.1, 0.12, 0.15, 0.1, 0.1, 0.05, 0.1)
IDTR <- datos %>%
  rowwise(depto) %>%
  mutate(IDTR = sum(c_across(all_of(ind_cols)) * ponderadores, na.rm = TRUE)) %>%
  ungroup() %>%
  select(depto, IDTR)

# ----------------------------
# 4) exportando el Indice a un archivo de excel con nombre de pestaña
# ---------------------------

write_xlsx(IDTR, "output/IDTR.xlsx")

# --------------------------------------------
# Código para agregar fila de ponderaciones y columna IDTR al archivo final
# --------------------------------------------
setwd("C:/Users/Javie/OneDrive/Escritorio/PROGRAMACIÓN II/programa-II/Proyecto/")
setwd("C:/Users/Javie/OneDrive/Escritorio/PROGRAMACIÓN II/programa-II/Proyecto/")

# 1) Cargar y limpiar encabezados
df <- read_excel("output/IDX.xlsx")
names(df) <- str_squish(names(df))

# 2) Variables y pesos
ind_cols <- c("var_uso_intern","var_uso_intern_mujeres","var_uso_acceso_tel_mov",
              "pct_rural","propor_hogares_internet","propor_hogares_computadora",
              "rb_por_100k","lineas_por_1k","pib_per_capita")
w <- c(0.18,0.10,0.1,0.12,0.15,0.10,0.10,0.05,0.10); names(w) <- ind_cols
sum(w)
# 3) IDTR
X <- df %>% select(all_of(ind_cols)) %>% mutate(across(everything(), ~coalesce(as.numeric(.x), 0)))
df <- df %>%
  mutate(IDTR = round(as.numeric(as.matrix(X) %*% w), 2)) %>%
  select(depto, all_of(ind_cols), IDTR)

# 4) Fila de ponderaciones y combinación (forzamos mismo tipo en depto)
pesos <- tibble(depto = "Ponderaciones") %>%
  bind_cols(as_tibble_row(w)) %>%
  mutate(IDTR = NA_real_)

tabla <- bind_rows(pesos, df %>% mutate(depto = as.character(depto)))

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
# 6) Exportando en documento de excel xls
# ----------------------------
write_xlsx(
  list("indicadores_basicos" = tabla,
       "diccionario_variables" = diccionario_variables),
  path = "output/IDX_y_IDTR.xlsx",format_headers = TRUE
)
