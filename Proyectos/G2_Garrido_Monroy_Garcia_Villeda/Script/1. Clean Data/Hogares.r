# Es buena practica cargar las librerías que se utilizaran al inicio
library(readxl)
library(dplyr)
library(stringr)
library(magrittr)
library(tidyverse)
library(lubridate)
library(readr)


# Es buena practica comenzar por limpiar el entorno de trabajo, cerrar
# cualquier gráfico abierto y limpiar la consola
graphics.off()
rm(list=ls())
cat("\014")
setwd("/Users/stefanivilleda/Desktop/Programación II/Proyecto/programa-II/Proyecto/")
#Carga de base Hogares

hogares <- read_excel("./input/ENCOVI_hogares.xlsx")
equipamiento <- read_excel("./input/Equipamiento.xlsx")


#Ver las variables
names(hogares)
names(equipamiento)

#Depurar solo las variables que necesito 
variables_hog <- hogares %<>% 
    select(DEPTO, AREA, FACTOR, NO_HOGAR, P01D20D, P01D20C)

variables_equip <- equipamiento %<>% 
    select(DEPTO, AREA, FACTOR, NO_HOGAR,P01J01A, DESC_EQUIPA)

head(variables_hog)
head(variables_equip)

#cambiar nombres de variables
variables_hog <- variables_hog |>
    rename(
        depto = DEPTO,
        area = AREA,
        factor = FACTOR,
        no_hogar = NO_HOGAR,
        internet_residencial = P01D20D,
        telefono_celular = P01D20C
    )

variables_equip <- variables_equip |>
    rename(
        depto = DEPTO,
        area = AREA,
        factor = FACTOR,
        no_hogar = NO_HOGAR,
        tiene_equipamiento = P01J01A,
        desc_equip = DESC_EQUIPA
    )


#Asegurar que las claves sean del mismo tipo
variables_hog <- variables_hog %>%
    mutate(across(c(depto, area, no_hogar), as.character))

variables_equip <- variables_equip %>%
    mutate(across(c(depto, area, no_hogar), as.character))

variables_hog
variables_equip


#A la base de equipamiento filtrarla para quedarme con las filas que respondieron 1 en tiene_equipamiento 
# y que la descripción del equipo sea computadora de escritorio o computadora laptop
variables_equip <- variables_equip %>%
    filter(tiene_equipamiento == 1 & 
               (str_detect(desc_equip, regex("Computadora de escritorio", ignore_case = TRUE)) |
                    str_detect(desc_equip, regex("Computadora laptop", ignore_case = TRUE))))

#Quiero que si el hogar se repite para computadora de escritorio o computadora laptop, se una en una sola fila como computadora de escritorio
variables_equip <- variables_equip %>%
    group_by(depto, area, factor, no_hogar) %>%
    summarise(
        tiene_equipamiento = first(tiene_equipamiento),
        desc_equip = paste(unique(desc_equip), collapse = " y ")
    ) %>%
    ungroup()
variables_equip


#Ahora voy a unir las dos bases, utilizando toda la de variables_hog quiero agregar las variables de tiene_equipamiento y desc_equip según el núm hogares
base_final <- variables_hog %>%
    left_join(variables_equip, by = c("depto", "area", "factor", "no_hogar"))
base_final

write_csv(base_final, "./output/hogares_clean.csv")

