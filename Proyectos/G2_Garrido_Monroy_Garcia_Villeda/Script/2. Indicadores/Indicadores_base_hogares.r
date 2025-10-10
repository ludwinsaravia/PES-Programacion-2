
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
setwd("/Users/stefanivilleda/Desktop/Programación II/Proyecto/programa-II/Proyecto/output/")
#Carga de base Hogares

hogares_clean <- read_csv("hogares_clean.csv")
head(hogares_clean)


#Indicadores
#Hogares por departamento con internet residencial, total de hogares y porporción pero sumando el factor 
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
propor_hogares_internet<- hogares_internet$pct_hogares_internet
   
#Hogares por departamento con computadora, total de hogares y porporción
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
propor_hogares_computadora<- hogares_compu$pct_hogares_compu
