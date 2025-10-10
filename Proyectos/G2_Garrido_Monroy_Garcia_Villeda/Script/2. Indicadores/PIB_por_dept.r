library(readxl)
library(dplyr)
library(tidyverse)


graphics.off()
rm(list=ls())
cat("\014")
setwd("/Users/stefanivilleda/Desktop/Programación II/Proyecto/programa-II/Proyecto/output/")
#Carga de base

datos <- read_csv("PIB_per_capita.csv")
head(datos)

#crear otra variable en una columna a la derecha que multiplique PIB per capita por poblacion
datos <- datos %>%
    mutate(
        PIB_depto = PIB_per_capita*poblacion
    )
datos

#Calcular % de PIB por departamento
datos <- datos %>%
    mutate(
        pct_PIB = datos$PIB_depto / sum(datos$PIB_depto),
        pct_PIB= round(pct_PIB * 100, 2)   
    )
datos

#Creando variable
pct_PIB_dpto <- datos$pct_PIB


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
datos <- datos %>%
    mutate(
        PIB_per_capita_idx = round(minmax_norm(PIB_per_capita, better = "higher", na.rm = TRUE), 2)
    )

datos <- datos %>%
    mutate(
        pct_PIB_idx = round(minmax_norm(pct_PIB, better = "higher", na.rm = TRUE), 2)
    )

# mostrar las primeras filas para verificar
head(datos)

