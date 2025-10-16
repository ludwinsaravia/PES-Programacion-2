library(readxl)
library(dplyr)
library(stringr)
library(magrittr)
library(tidyverse)
library(lubridate)
library(readr)
library(stringr)

# Es buena practica comenzar por limpiar el entorno de trabajo, cerrar
# cualquier gráfico abierto y limpiar la consola
graphics.off()
rm(list=ls())
cat("\014")
setwd("/Users/stefanivilleda/Desktop/Programación II/Proyecto/programa-II/Proyecto/")
#Carga de bases de datos


# ============================
# 1)lectura de archivos
# ============================

# Radiobases: la hoja "2023" trae 2 filas cabecera -> usar skip = 2
radiobases <- read_excel("./input/Radiobases.xlsx", sheet = "2023", skip = 2)

# Telefonía fija: hoja por defecto; tiene columna "Departamento" y columna "2023"
telefonia  <- read_excel("./input/Telefonía fija.xlsx")

# ============================
# 2) para limpiar depto
# ============================
# Quita tildes (via iconv), borra apóstrofes, compacta espacios y pone sin acentos ni apostrofes
?stringr
?iconv
#iconv(x, from = "", to = "", sub = NA, mark = TRUE, toRaw = FALSE)
#https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/iconv


to_title_no_accents <- function(x){
    x %>%
        iconv(to = "ASCII//TRANSLIT") %>%      # "Quiché" -> "Quiche", "Petén" -> "Peten"
        # 2) Elimina apóstrofes y comillas 
        str_replace_all("[`´'’‘]", "") %>%      # "Pet'en" -> "Peten"
        # 3) Pasa todo a minúsculas para normalizar
        tolower() %>%
        # 4) Compacta espacios:
        str_squish() %>%
        # 5) Aplica el title que a cada palabra inicia en mayúscula, resto minúsculas
        str_to_title()
}

# Casos especiales y filas que no utilizo 
fix_special_deptos <- function(dep){
    dep %>%
        str_replace(regex("^El\\s+", ignore_case = TRUE), "") %>%  # "El Peten" -> "Peten"
        na_if("Departamento")                                      # elimina filas cabecera
}

# diccionario para presentar con tildes bonitas
lookup_pretty <- tibble::tibble(
    departamento     = c(
        "Alta Verapaz","Baja Verapaz","Chimaltenango","Chiquimula","El Progreso",
        "Escuintla","Guatemala","Huehuetenango","Izabal","Jalapa","Jutiapa",
        "Peten","Quetzaltenango","Quiche","Retalhuleu","Sacatepequez","San Marcos",
        "Santa Rosa","Solola","Suchitepequez","Totonicapan","Zacapa"
    ),
    departamento_limpio = c(
        "Alta Verapaz","Baja Verapaz","Chimaltenango","Chiquimula","El Progreso",
        "Escuintla","Guatemala","Huehuetenango","Izabal","Jalapa","Jutiapa",
        "Petén","Quetzaltenango","Quiché","Retalhuleu","Sacatepéquez","San Marcos",
        "Santa Rosa","Sololá","Suchitepéquez","Totonicapán","Zacapa"
    )
)

# ============================
# 3) Radiobases 2023 por departamento
# ============================
radiobases_2023 <- radiobases %>%
    select(1:3) %>%                                    # nos quedamos con las 3 primeras cols
    `colnames<-`(c("departamento","municipio","radiobases")) %>%
    select(departamento, municipio, radiobases) %>%
    mutate(
        departamento = to_title_no_accents(departamento),
        departamento = fix_special_deptos(departamento),
        radiobases   = suppressWarnings(as.numeric(radiobases))
    ) %>%
    filter(!is.na(departamento), departamento != "Total") %>%
    group_by(departamento) %>%
    summarise(radiobases_2023 = sum(radiobases, na.rm = TRUE), .groups = "drop")

# ============================
# 4) Telefonía fija 2023 por departamento
# ============================
telefonia_2023 <- telefonia %>%
    select(Departamento, `2023`) %>%
    rename(departamento = Departamento, lineas_fijas_2023 = `2023`) %>%
    mutate(
        departamento      = to_title_no_accents(departamento),
        departamento      = fix_special_deptos(departamento),
        lineas_fijas_2023 = as.numeric(lineas_fijas_2023)
    ) %>%
    filter(!is.na(departamento), departamento != "Total") %>%
    group_by(departamento) %>%
    summarise(lineas_fijas_2023 = sum(lineas_fijas_2023, na.rm = TRUE), .groups = "drop")

# ============================
# 5) Unión por departamento
#     - full_join para ver faltantes de cualquiera de las dos
# ============================
tabla_2023 <- radiobases_2023 %>%
    full_join(telefonia_2023, by = "departamento") %>%
    arrange(departamento)

# ============================
# 6) Presentación con tildes
# ============================
tabla_2023_final <- tabla_2023 %>%
    left_join(lookup_pretty, by = "departamento") %>%
    mutate(departamento = coalesce(departamento_limpio, departamento)) %>%
    select(-departamento_limpio)

tabla_2023_final

#Cambiar nombres
tabla_2023_final<- tabla_2023_final %>%
    mutate(departamento = recode(departamento,
                          "Guatemala" = 1,
                          "Progreso" = 2,
                          "Sacatepéquez" = 3,
                          "Chimaltenango" = 4,
                          "Escuintla" = 5,
                          "Santa Rosa" = 6,
                          "Sololá" = 7,
                          "Totonicapán" = 8,
                          "Quetzaltenango" = 9,
                          "Suchitepéquez" = 10,
                          "Retalhuleu" = 11,
                          "San Marcos" = 12,
                          "Huehuetenango" = 13,
                          "Quiché" = 14,
                          "Baja Verapaz" = 15,
                          "Alta Verapaz" = 16,
                          "Petén" = 17,
                          "Izabal" = 18,
                          "Zacapa" = 19,
                          "Chiquimula" = 20,
                          "Jalapa" = 21,
                          "Jutiapa" = 22))

#ordenando:
tabla_2023_final <- tabla_2023_final %>%
    arrange(departamento)



write_csv(tabla_2023_final, "./output/radiobases_lineas_telef.csv")
