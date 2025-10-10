library(readxl)
library(dplyr)
library(stringr)
library(magrittr)
library(tidyverse)
library(lubridate)
library(readr)
library(writexl)

graphics.off()
rm(list=ls())
cat("\014")
setwd("/Users/stefanivilleda/Desktop/Programación II/Proyecto/programa-II/Proyecto/output/")
#Carga de base

datos <- read_excel("indicadores_acceso_tecnologico.xlsx")
head(datos)

# función min-max a escala 0-100
minmax_norm <- function(x, better = c("higher", "lower"), na.rm = TRUE) {
    better <- match.arg(better)
    if (na.rm) x_non_na <- x[!is.na(x)] else x_non_na <- x
    xmin <- min(x_non_na, na.rm = TRUE)
    xmax <- max(x_non_na, na.rm = TRUE)
    if (is.na(xmin) || is.na(xmax) || (xmax - xmin) == 0) {
        res <- rep(0, length(x))
    } else {
        if (better == "higher") {
            res <- (x - xmin) / (xmax - xmin) * 100
        } else {
            res <- (xmax - x) / (xmax - xmin) * 100
        }
    }
    res[is.na(x)] <- NA
    return(res)
}

# aplicar la normalización a la tabla
datos_normalizados <- datos %>%
    mutate(
        var_uso_cel_idx = round(minmax_norm(var_uso_cel, better = "higher", na.rm = TRUE), 2),
        var_uso_intern_idx = round(minmax_norm(var_uso_intern, better = "higher", na.rm = TRUE), 2),
        var_uso_intern_mujeres_idx = round(minmax_norm(var_uso_intern_mujeres, better = "higher", na.rm = TRUE), 2),
        var_uso_acceso_tel_mov_idx = round(minmax_norm(var_uso_acceso_tel_mov, better = "higher", na.rm = TRUE), 2),
        pct_rural_idx = round(minmax_norm(pct_rural, better = "higher", na.rm = TRUE), 2),
        propor_hogares_internet_idx = round(minmax_norm(propor_hogares_internet, better = "higher", na.rm = TRUE), 2),
        propor_hogares_computadora_idx = round(minmax_norm(propor_hogares_computadora, better = "higher", na.rm = TRUE), 2),
        pct_radiobases_idx = round(minmax_norm(pct_radiobases, better = "higher", na.rm = TRUE), 2),
        pct_lineas_fijas_idx = round(minmax_norm(pct_lineas_fijas, better = "higher", na.rm = TRUE), 2),
        )

#Seleccionar solo los indices
idx_final<- datos_normalizados %>%
    select(depto,var_uso_cel_idx,var_uso_intern_idx,var_uso_intern_mujeres_idx,
           var_uso_acceso_tel_mov_idx,pct_rural_idx, propor_hogares_internet_idx, 
           propor_hogares_computadora_idx,pct_radiobases_idx,pct_lineas_fijas_idx
           )
idx_final

#Calcular el promedio simple por fila
idx_final <- idx_final %>%
    mutate(
        indice_conectividad = rowMeans(select(., -1), na.rm = TRUE) %>% 
            round(2)
    )

#Generar el Excel con los índices finales
 write_xlsx(idx_final, "idx_final.xlsx")
