# Limpieza y visualización de datos
# Caso practico: Analizando los componentes del IPC
# Análisis de datos 1: Estadísticas descriptivas
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
library(modelsummary)

# Limpiar entorno de trabajo
graphics.off()
rm(list = ls())
cat("\014")

# ------------------------------------------------------------------------------
# 1) Cargar y limpiar datos
# ------------------------------------------------------------------------------

ipc_data <- read_csv("./input/clean_data.csv")

# Vamos a restringir el análisis a los datos a nivel nacional

selected_sample <- ipc_data %>%
    select(
        t_date,
        id_item,
        id_grupo,
        id_nivel,
        descr_grupo,
        descr,
        ipc,
    )


# Para evitar problmas de comparabilidad con el cambio de la canasta
# del IPC en 2024, vamos a seleccionar solo los años antes de 2024
selected_sample %<>% filter(year(t_date) <= 2023)

# Finalmente, vamos a enfocarnos en el análisis a nivel de "Grupo", y el
# general, así que vamos a filtrar los datos para quedarnos solo con esos
selected_sample %<>% filter(id_nivel %in% c("General", "Grupo"))

# ------------------------------------------------------------------------------
# 2) Construir indices de inflación anual
# ------------------------------------------------------------------------------

# Calculemos las inflaciones
ipc_data <- selected_sample %>%
    group_by(id_item) %>%
    arrange(t_date) %>%
    mutate(
        ipc_lag12 = lag(ipc, n = 12, order_by = t_date),
        inf12m = (ipc / ipc_lag12 - 1) * 100
    ) %>%
    ungroup()

ipc_data %<>% filter(!is.na(inf12m))

# Y escribamos como tabla
inf12m_tab <- ipc_data %>%
    select(t_date, id_item, inf12m) %>%
    pivot_wider(names_from = id_item, values_from = inf12m)

# Creemos una tabla con los nombres de las columnas
grupo_list <- ipc_data %>%
    filter(year(t_date) == 2023) %>%
    select(id_grupo = id_item, descr_grupo = descr, id_nivel) %>%
    distinct()

# Añadamos una variable como categórica para indicar el mes
dataTab <- inf12m_tab %<>% mutate(mes = forcats::as_factor(month(t_date)))

# Finalmente, usemos algunos nombres más dscriptivos
colnames(dataTab) <- c(
    "Fecha",
    "General",
    "Alimentos",
    "Alcohol",
    "Ropa",
    "Vivienda",
    "Muebles",
    "Salud",
    "Transporte",
    "Comunicaciones",
    "Recreacion",
    "Educacion",
    "Restaurantes",
    "Otros",
    "Mes"
)


# ------------------------------------------------------------------------------
# 4)  Estadísticas descriptivas de la inflación por grupo de gasto
# ------------------------------------------------------------------------------

# Existen varias librerías para calcular estadísticas descriptivas de forma
# sencilla. Una de las más completas es modelsummary, la cual contiene una
# familia de funciones para crear tablas de resumen de datos y resultados de
# modelos estadísticos.

# Comenzemos por utilizar la función datasummary_skim, para obtener un resumen
# estadístico de las variables en un data frame

# Para resumir todas las variables....
datasummary_skim(dataTab)

# ...solo las continuas
datasummary_skim(dataTab, type = "numeric")


# ... o las discretas (categóricas)
datasummary_skim(dataTab, type = "categorical")


# También podemos construir matrices de correlación entre las variables
datasummary_correlation(dataTab)


# Podemos usar la función datasummary para crear tablas personalizadas
# de estadísticas descriptivas. Por ejemplo, para obtener la media, mediana,
# desviación estándar, mínimo y máximo de las variables continuas:
datasummary(
    `General` + `Alcohol` ~ Mean + Median + SD + Min + Max,
    data = dataTab
)

# Y podemos customizar aún más la tabla, añadiendo títulos y subtítulos y
# cambiando el nombre de las columnas
formula <- (`Inflación General` = General) + (`Alcohol y Tabaco` = Alcohol)  ~ Mean + Median + SD + Min + Max
datasummary(
    formula,
    data = dataTab,
    title = "Análisis de la Inflación Anual por Grupo de Gasto",
    notes = "Datos mensuales de 2010 a 2023"
)

# Finalmente, podemos exportar nuestras tablas
datasummary(
    formula,
    data = dataTab,
    title = "Análisis de la Inflación Anual por Grupo de Gasto",
    notes = "Datos mensuales de 2010 a 2023",
    output = "./output/tabla_resumen.png"
)


# O como código de Latex y Markdown
datasummary(
    formula,
    data = dataTab,
    title = "Análisis de la Inflación Anual por Grupo de Gasto",
    notes = "Datos mensuales de 2010 a 2023",
    output = "markdown"
)


datasummary(
    formula,
    data = dataTab,
    title = "Análisis de la Inflación Anual por Grupo de Gasto",
    notes = "Datos mensuales de 2010 a 2023",
    output = "latex"
)