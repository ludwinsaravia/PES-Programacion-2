# Taller 4: Limpieza y visualización de datos
# Carga, limpieza y visualización de datos del censo de 2018
#
# Ángelo Gutiérrez Daza
# Programación II
# Programa de Estudios Superiores
# Banco de Guatemala
# 2025

# Limpiemos
rm(list = ls())
graphics.off()
set.seed(123)

# Librerías usadas
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(stringr)
library(magrittr)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(forcats)
library(modelsummary)

library(tidyverse)
library(duckdb)
library(arrow)


# 1 - Carga y limpieza de datos --------------------------------------------------------------------

# In-memory database
censo_con <- dbConnect(duckdb::duckdb())
# Register the CSV file as a table in DuckDB with lenient parsing
dbExecute(censo_con, "CREATE VIEW censo AS SELECT * FROM read_csv_auto('./input/censo.csv', ignore_errors=true, null_padding=true)")
my_data_db <- tbl(censo_con, "censo")

# Filtremos las variables de interes
selected_data <- my_data_db %>%
    select(
        departamento  = "DEPARTAMENTO",
        municipio     = "MUNICIPIO",
        cod_municipio = "COD_MUNICIPIO",
        sexo          = "PCP6",
        pcp7          = "PCP7",
        aneduca       = "ANEDUCA",
        pea           = "PEA",
        ocupado       = "POCUPA",
        desocupado    = "PDESOC"
    )

# Veamos la estructura de los datos
glimpse(selected_data)

# Reemplazemos los NA en pea, ocupado y desocupado por 0
selected_data <- selected_data %>%
    mutate(
        pea        = coalesce(as.numeric(case_when(str_trim(pea) == "" ~ NA_character_, TRUE ~ pea)), 0),
        ocupado    = coalesce(as.numeric(case_when(str_trim(ocupado) == "" ~ NA_character_, TRUE ~ ocupado)), 0),
        desocupado = coalesce(as.numeric(case_when(str_trim(desocupado) == "" ~ NA_character_, TRUE ~ desocupado)), 0)
    )

# Reemplazemos el numero de departamento por su nombre usando case_when
selected_data <- selected_data %>%
    mutate(
        departamento = case_when(
            departamento == 1  ~ "Guatemala",
            departamento == 2  ~ "El Progreso",
            departamento == 3  ~ "Sacatepéquez",
            departamento == 4  ~ "Chimaltenango",
            departamento == 5  ~ "Escuintla",
            departamento == 6  ~ "Santa Rosa",
            departamento == 7  ~ "Sololá",
            departamento == 8  ~ "Totonicapán",
            departamento == 9  ~ "Quetzaltenango",
            departamento == 10 ~ "Suchitepéquez",
            departamento == 11 ~ "Retalhuleu",
            departamento == 12 ~ "San Marcos",
            departamento == 13 ~ "Huehuetenango",
            departamento == 14 ~ "Quiché",
            departamento == 15 ~ "Baja Verapaz",
            departamento == 16 ~ "Alta Verapaz",
            departamento == 17 ~ "Petén",
            departamento == 18 ~ "Izabal",
            departamento == 19 ~ "Zacapa",
            departamento == 20 ~ "Chiquimula",
            departamento == 21 ~ "Jalapa",
            departamento == 22 ~ "Jutiapa",
            TRUE ~ as.character(departamento)
        )
    )

# Reemplacemos el numero de sexo por su nombre usando case_when
selected_data <- selected_data %>%
    mutate(
        sexo = case_when(
            sexo == 1 ~ "M",
            sexo == 2 ~ "F",
            TRUE ~ as.character(sexo)
        )
    )

# Veamos la estructura de los datos nuevamente
glimpse(selected_data)

# Exportemos el dataset limpio, eliminemos el anterior y liberemos memoria
#selected_data_collected <- selected_data %>% collect()
#write_csv(selected_data_collected, "./input/censo_submuestra.csv")
#rm(list = ls())
#gc()
#selected_data <- read_csv("./input/censo_submuestra.csv")

# 2 - Número de mujeres y hombres ------------------------------------------------------------------

# Ahora, en una tabla, mostramos el número de hombres y el número de mujeres por
# departamento en Guatemala.


# Opción 1: Usando dplyr y tidyr
tabla_sexo <- selected_data %>%
    group_by(departamento, sexo) %>%
    summarise(n = n()) %>%
    pivot_wider(names_from = sexo, values_from = n, values_fill = 0) %>%
    rename(Hombre = M, Mujer = F) %>%
    arrange(departamento)

# Añadamos total y reordenemos de mayor a menor total
tabla_sexo <- tabla_sexo %>%
    mutate(Total = Hombre + Mujer) %>%
    arrange(desc(Total)) %>%
    select(departamento, Hombre, Mujer, Total) %>% 
    collect()


# 3 - Estimemos la relacion entre tasa de desempleo y años de educacion promedio por municipio -----


# Primero, creemos una tabla resumen por municipio
tabla_municipio <- selected_data %>%
    group_by(municipio, cod_municipio) %>%
    filter(pea == 1) %>%
    summarise(
        aneduca_prom = mean(aneduca, na.rm = TRUE),
        pea_total    = sum(pea, na.rm = TRUE),
        desocupado   = sum(desocupado, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
        tasa_desempleo = desocupado / pea_total
    ) %>%
    arrange(desc(tasa_desempleo))

# Usemos OLS para estimar la relacion entre tasa de desempleo y años de educacion promedio
modelo <- lm(tasa_desempleo ~ aneduca_prom, data = tabla_municipio)
summary(modelo)

# Repitamos el ejercicio, ponderando por PEA total del municipio
modelo_ponderado <- lm(tasa_desempleo ~ aneduca_prom, data = tabla_municipio, weights = pea_total)
summary(modelo_ponderado)   
