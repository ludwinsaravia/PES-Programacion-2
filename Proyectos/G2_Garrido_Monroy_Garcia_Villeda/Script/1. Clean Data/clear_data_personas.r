# =========================================================
# Proyecto final — Limpieza de base ENCOVI (personas)
# Autor: [tu nombre]
# Fecha: Sys.Date()
# Descripción: Selección, renombre y exportación de variables clave
# =========================================================

# ----------------------------
# 1) Librerías y limpieza de entorno
# ----------------------------
graphics.off()
rm(list=ls())
cat("\014")


library(dplyr)
library(stringr)
library(readxl)
library(tidyverse)

# ----------------------------
# 1) rutas y carga de datos
# ----------------------------
getwd()
#setwd("'/Users/paulogarridogrijalva/Documents/GitHub/programa-II/Proyecto/")

personas_encovi <- read_excel("input/ENCOVI_personas.xlsx")

# ----------------------------
# 4) Selección y renombre de variables
# ----------------------------

personas_encovi <- personas_encovi %>%
    select(DEPTO, AREA, FACTOR, PPA02, P12A01, P12A04, P12A08) %>%
    rename(
        depto         = DEPTO,
        area          = AREA,
        factor        = FACTOR,
        sexo          = PPA02,
        uso_celular   = P12A01,
        tiene_celular = P12A04,
        uso_internet  = P12A08
    )

# ----------------------------
# 6) Exportación a CSV (UTF-8, sin rownames)
# ----------------------------
write.csv(personas_encovi, "output/personas_encovi.csv", row.names = FALSE)
    











