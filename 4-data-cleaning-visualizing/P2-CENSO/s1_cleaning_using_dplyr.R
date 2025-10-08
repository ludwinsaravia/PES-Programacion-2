# Limpieza y visualización de datos
# Caso practico: Analizando datos del censo de 2018
# Este archivo: Análisis y limpieza usando dplyr
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
cat("\014") 


# Librerías usadas
library(dplyr)
library(readr)
library(magrittr)
library(forcats)
library(tictoc)
library(tidyr)

# Iniciar cronómetro
tictoc::tic("Script completo")


# Cinco tareas: 
# 1 - Carga y limpieza de datos
# 2 - Cálculo de número de hogares por vivienda
# 3 - Cálculo de número de personas por hogar
# 4 - Cálculo de número de mujeres y hombres por municipio
# 5 - Filtrado de población económicamente activa
# 6 - Cálculo de tasa de desempleo y años de educación promedio por municipio
# 7 - Cálculo de matriz de flujos migratorios entre departamentos (2013-2018)

# 1 - Carga y limpieza de datos --------------------------------------------------------------------

# Usando readr::read:csv 
# censo <- read_csv("./input/censo.csv", n_max = 1000)
censo <- read_csv("./input/censo.csv")

# Filtremos las variables de interes
selected_data <- censo %>%
    select(
        departamento      = "DEPARTAMENTO",
        municipio         = "MUNICIPIO",
        departamento_2013 = "PCP11_B",
        municipio_2013    = "RESCINGEO",
        vivienda_id       = "NUM_VIVIENDA",
        hogar_id          = "NUM_HOGAR",
        persona_id        = "PCP1",
        sexo              = "PCP6",
        edad              = "PCP7",
        aneduca           = "ANEDUCA",
        pea               = "PEA",
        ocupado           = "POCUPA",
        desocupado        = "PDESOC"
    )

                        "NGDP_R",
                        "NGDP_RPCH",	
                        "NGDP_D",	
                        "PCPI",	
                        "PCPIPCH",
                        "PCPIE"	,	
                        "PCPIEPCH"


# Veamos la estructura de los datos nuevamente
glimpse(selected_data)

# Reemplazemos los NA en pea, ocupado y desocupado por 0
selected_data <- selected_data %>%
    mutate(
        pea        = if_else(is.na(pea)       , 0, pea       ),
        ocupado    = if_else(is.na(ocupado)   , 0, ocupado   ),
        desocupado = if_else(is.na(desocupado), 0, desocupado)
    )

# Reemplazemos el numero de departamento por su nombre usando recode
selected_data <- selected_data %>%
    mutate(
        departamento = recode(
            departamento,
            `1`  = "Guatemala",
            `2`  = "El Progreso",
            `3`  = "Sacatepéquez",
            `4`  = "Chimaltenango",
            `5`  = "Escuintla",
            `6`  = "Santa Rosa",
            `7`  = "Sololá",
            `8`  = "Totonicapán",
            `9`  = "Quetzaltenango",
            `10` = "Suchitepéquez",
            `11` = "Retalhuleu",
            `12` = "San Marcos",
            `13` = "Huehuetenango",
            `14` = "Quiché",
            `15` = "Baja Verapaz",
            `16` = "Alta Verapaz",
            `17` = "Petén",
            `18` = "Izabal",
            `19` = "Zacapa",
            `20` = "Chiquimula",
            `21` = "Jalapa",
            `22` = "Jutiapa"
        ),
        departamento_2013 = recode(
            departamento_2013,
            `1`  = "Guatemala",
            `2`  = "El Progreso",
            `3`  = "Sacatepéquez",
            `4`  = "Chimaltenango",
            `5`  = "Escuintla",
            `6`  = "Santa Rosa",
            `7`  = "Sololá",
            `8`  = "Totonicapán",
            `9`  = "Quetzaltenango",
            `10` = "Suchitepéquez",
            `11` = "Retalhuleu",
            `12` = "San Marcos",
            `13` = "Huehuetenango",
            `14` = "Quiché",
            `15` = "Baja Verapaz",
            `16` = "Alta Verapaz",
            `17` = "Petén",
            `18` = "Izabal",
            `19` = "Zacapa",
            `20` = "Chiquimula",
            `21` = "Jalapa",
            `22` = "Jutiapa"
        )
    )

# Reemplacemos el numero de sexo por su nombre usando recode
selected_data <- selected_data %>%
    mutate(
        sexo = recode(
            sexo,
            `1` = "Hombre",
            `2` = "Mujer"
        )
    )

rm(censo)
gc()

# 2 - Cálculo de número de hogares por vivienda ----------------------------------------------------

n_hogares_por_vivienda <- selected_data %>%
    group_by(vivienda_id) %>%
    summarise(n_hogares = n_distinct(hogar_id)) %>%
    ungroup()

# 3 - Cálculo de número de personas por hogar ------------------------------------------------------

n_personas_por_hogar <- selected_data %>%
    group_by(hogar_id) %>%
    summarise(n_personas = n_distinct(persona_id)) %>%
    ungroup()


# 4 - Cálculo de número de mujeres y hombres por municipio ----------------------------------------

tabla_sexo <- selected_data %>%
    group_by(municipio, sexo) %>%
    summarise(n = n()) %>%
    pivot_wider(names_from = sexo, values_from = n, values_fill = 0) 

# 5 - Filtrado de población económicamente activa ---------------------------------------------------
pea_data <- selected_data %>%
    filter(pea == 1)

# 6 - Cálculo de tasa de desempleo y años de educación promedio por municipio ----------------------
tabla_municipio <- selected_data %>%
    group_by(municipio) %>%
    filter(pea == 1) %>%
    summarise(
        aneduca_prom = mean(aneduca, na.rm = TRUE),
        pea_total    = sum(pea, na.rm = TRUE),
        desocupado   = sum(desocupado, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
        tasa_desempleo = 100 * desocupado / pea_total
    ) %>%
    arrange(desc(tasa_desempleo))

# 7 - Cálculo de matriz de flujos migratorios entre departamentos (2013-2018) ----------------------
matriz_flujos <- selected_data %>%
    filter(departamento != departamento_2013) %>%
    group_by(departamento, departamento_2013) %>%
    summarise(n = n()) %>%
    pivot_wider(names_from = departamento, values_from = n, values_fill = 0) %>% 
    arrange(departamento_2013)

# Finalizar cronómetro y mostrar tiempo total
tictoc::toc()