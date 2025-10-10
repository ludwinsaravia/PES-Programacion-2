library(readxl)
library(dplyr)
library(stringr)
library(magrittr)
library(tidyverse)
library(lubridate)
library(readr)

graphics.off()
rm(list=ls())
cat("\014")
setwd("/Users/stefanivilleda/Desktop/Programación II/Proyecto/programa-II/Proyecto/output/")
#Carga de base

datos <- read_csv("radiobases_lineas_telef.csv")
head(datos)

#calcular % de radiobases por departamento
datos <- datos %>%
    mutate(
        pct_radiobases = datos$radiobases_2023 / sum(datos$radiobases_2023),
        pct_radiobases= round(pct_radiobases * 100, 2)   
    )
datos

#Creando la variable
pct_radiobases <- datos$pct_radiobases

#Calcular % de lineas fijas por departamento
tabla_final <- datos %>%
    mutate(
        pct_lineas_fijas = datos$lineas_fijas_2023 / sum(datos$lineas_fijas_2023),
        pct_lineas_fijas= round(pct_lineas_fijas * 100, 2)   
    )
tabla_final

#Creando variable de lineas fijas
pct_lineas_fijas <- tabla_final$pct_lineas_fijas
