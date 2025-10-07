# Limpieza y visualización de datos
# Caso practico: Analizando los componentes del IPC
# Paso 2: Seleccionar la muestra a analizar
#
# Ángelo Gutiérrez Daza
# 2025
#
# Programación II
# Programa de Estudios Superiores
# Banco de Guatemala
#
# Código probado utilizando la versión 4.5.1 de R

# ------------------------------------------------------------------------------
# 0) Inicialización
# ------------------------------------------------------------------------------

# Librerías usadas
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(stringr)
library(readxl)
library(magrittr)

# Limpiar entorno de trabajo
graphics.off()
rm(list=ls())
cat("\014")

# ------------------------------------------------------------------------------
# 1) Carga de datos
# ------------------------------------------------------------------------------

# Comenzemos por cargar el CSV con los datos limpios
# Utilizaremos la función read_csv() del paquete readr
# Ver: https://readr.tidyverse.org/reference/read_delim.html
ipc_data <- read_csv("./output/clean_data.csv")


# ------------------------------------------------------------------------------
# 2) Seleccionar datos
# ------------------------------------------------------------------------------

# Comenzamos por seleccionar los datos que queremos analizar
# Vamos a resringir el análisis a los datos a nivel nacional

selected_sample <- ipc_data %>% 
    select( 
        t_date      ,
        id_item     ,
        id_grupo    ,
        id_nivel    ,
        descr_grupo ,
        descr       ,
        ipc         ,
    )


# Para evitar problmas de comparabilidad con el cambio de la canasta
# del IPC en 2024, vamos a seleccionar solo los años antes de 2024
selected_sample %<>% filter( year(t_date) <= 2023 )

# Finalmente, vamos a enfocarnos en el análisis a nivel de "item", y el 
# general, así que vamos a filtrar los datos para quedarnos solo con esos
selected_sample %<>% filter( id_nivel %in% c("Item", "General") )

# Finalmente, ordenemos los datos por fecha y código
  
# Veamos la estructura de los datos seleccionados
glimpse(selected_sample)

# ------------------------------------------------------------------------------
# 3) Exportar la muestra seleccionada
# ------------------------------------------------------------------------------

write_csv(selected_sample, "./output/selected_data.csv")
