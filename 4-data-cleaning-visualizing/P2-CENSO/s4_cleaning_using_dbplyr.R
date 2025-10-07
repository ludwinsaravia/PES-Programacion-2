# Limpieza y visualización de datos
# Caso practico: Analizando datos del censo de 2018
# Este archivo: Análisis y limpieza usando dbplyr con DuckDB backend
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
library(duckdb)
library(DBI)
library(dplyr)
library(dbplyr)
library(tictoc)
library(tidyr)

# Iniciar cronómetro
tictoc::tic("Script completo - dbplyr con DuckDB")

# Cinco tareas: 
# 1 - Conexión a DuckDB y creación de tabla virtual
# 2 - Cálculo de número de hogares por vivienda
# 3 - Cálculo de número de personas por hogar
# 4 - Cálculo de número de mujeres y hombres por municipio
# 5 - Filtrado de población económicamente activa
# 6 - Cálculo de tasa de desempleo y años de educación promedio por municipio
# 7 - Cálculo de matriz de flujos migratorios entre departamentos (2013-2018)

# 1 - Conexión a DuckDB y creación de tabla virtual ----------------------------------------------

# Crear conexión a DuckDB
con <- dbConnect(duckdb::duckdb(), ":memory:")

# Registrar la tabla CSV directamente en DuckDB usando dbplyr
censo_raw <- tbl(con, sql("
SELECT 
    CAST(DEPARTAMENTO AS INTEGER) as departamento,
    MUNICIPIO as municipio,
    CAST(CASE WHEN PCP11_B IS NULL OR TRIM(PCP11_B) = '' THEN '0' ELSE PCP11_B END AS INTEGER) as departamento_2013,
    RESCINGEO as municipio_2013,
    NUM_VIVIENDA as vivienda_id,
    NUM_HOGAR as hogar_id,
    PCP1 as persona_id,
    CAST(PCP6 AS INTEGER) as sexo,
    CAST(PCP7 AS INTEGER) as edad,
    CAST(CASE WHEN ANEDUCA IS NULL OR TRIM(ANEDUCA) = '' THEN '0' ELSE ANEDUCA END AS DOUBLE) as aneduca,
    CAST(CASE WHEN PEA IS NULL OR TRIM(PEA) = '' THEN '0' ELSE PEA END AS INTEGER) as pea,
    CAST(CASE WHEN POCUPA IS NULL OR TRIM(POCUPA) = '' THEN '0' ELSE POCUPA END AS INTEGER) as ocupado,
    CAST(CASE WHEN PDESOC IS NULL OR TRIM(PDESOC) = '' THEN '0' ELSE PDESOC END AS INTEGER) as desocupado
FROM read_csv('./input/censo.csv', 
    header=true,
    ignore_errors=true,
    null_padding=true,
    auto_detect=true)
"))

# Ahora usamos dplyr para limpiar y transformar los datos
selected_data <- censo_raw %>%
    mutate(
        # Mapear departamentos a nombres
        departamento = case_when(
            departamento == 1 ~ "Guatemala",
            departamento == 2 ~ "El Progreso",
            departamento == 3 ~ "Sacatepéquez",
            departamento == 4 ~ "Chimaltenango",
            departamento == 5 ~ "Escuintla",
            departamento == 6 ~ "Santa Rosa",
            departamento == 7 ~ "Sololá",
            departamento == 8 ~ "Totonicapán",
            departamento == 9 ~ "Quetzaltenango",
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
            TRUE ~ "Desconocido"
        ),
        # Mapear departamento 2013 a nombres
        departamento_2013 = case_when(
            departamento_2013 == 1 ~ "Guatemala",
            departamento_2013 == 2 ~ "El Progreso",
            departamento_2013 == 3 ~ "Sacatepéquez",
            departamento_2013 == 4 ~ "Chimaltenango",
            departamento_2013 == 5 ~ "Escuintla",
            departamento_2013 == 6 ~ "Santa Rosa",
            departamento_2013 == 7 ~ "Sololá",
            departamento_2013 == 8 ~ "Totonicapán",
            departamento_2013 == 9 ~ "Quetzaltenango",
            departamento_2013 == 10 ~ "Suchitepéquez",
            departamento_2013 == 11 ~ "Retalhuleu",
            departamento_2013 == 12 ~ "San Marcos",
            departamento_2013 == 13 ~ "Huehuetenango",
            departamento_2013 == 14 ~ "Quiché",
            departamento_2013 == 15 ~ "Baja Verapaz",
            departamento_2013 == 16 ~ "Alta Verapaz",
            departamento_2013 == 17 ~ "Petén",
            departamento_2013 == 18 ~ "Izabal",
            departamento_2013 == 19 ~ "Zacapa",
            departamento_2013 == 20 ~ "Chiquimula",
            departamento_2013 == 21 ~ "Jalapa",
            departamento_2013 == 22 ~ "Jutiapa",
            TRUE ~ "Desconocido"
        ),
        # Mapear sexo a nombres
        sexo = case_when(
            sexo == 1 ~ "Hombre",
            sexo == 2 ~ "Mujer",
            TRUE ~ "Desconocido"
        )
    )

# 2 - Cálculo de número de hogares por vivienda ----------------------------------------------------

n_hogares_por_vivienda <- selected_data %>%
    group_by(vivienda_id) %>%
    summarise(n_hogares = n_distinct(hogar_id)) %>%
    arrange(vivienda_id) %>%
    collect()

# 3 - Cálculo de número de personas por hogar ------------------------------------------------------

n_personas_por_hogar <- selected_data %>%
    group_by(hogar_id) %>%
    summarise(n_personas = n_distinct(persona_id)) %>%
    arrange(hogar_id) %>%
    collect()

# 4 - Cálculo de número de mujeres y hombres por municipio ----------------------------------------

tabla_sexo <- selected_data %>%
    group_by(municipio, sexo) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(municipio, sexo) %>%
    collect()

# Convertir a formato ancho usando dplyr
tabla_sexo_wide <- selected_data %>%
    group_by(municipio, sexo) %>%
    summarise(n = n(), .groups = "drop") %>%
    pivot_wider(names_from = sexo, values_from = n, values_fill = 0) %>%
    arrange(municipio) %>%
    collect()

# 5 - Filtrado de población económicamente activa ---------------------------------------------------

pea_count <- selected_data %>%
    filter(pea == 1) %>%
    summarise(total_pea = n()) %>%
    collect()

cat("Población Económicamente Activa:", pea_count$total_pea, "personas\n")

# 6 - Cálculo de tasa de desempleo y años de educación promedio por municipio ----------------------

tabla_municipio <- selected_data %>%
    filter(pea == 1) %>%
    group_by(municipio) %>%
    summarise(
        aneduca_prom = mean(aneduca, na.rm = TRUE),
        pea_total = sum(pea, na.rm = TRUE),
        desocupado = sum(desocupado, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(tasa_desempleo = 100 * desocupado / pea_total) %>%
    arrange(desc(tasa_desempleo)) %>%
    collect()

# 7 - Cálculo de matriz de flujos migratorios entre departamentos (2013-2018) ----------------------

# Contar flujos migratorios usando dplyr
flujos_migratorios <- selected_data %>%
    filter(
        departamento != departamento_2013,
        !is.na(departamento_2013),
        !is.na(departamento),
        departamento_2013 != "Desconocido",
        departamento != "Desconocido"
    ) %>%
    group_by(departamento_2013, departamento) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(departamento_2013, departamento) %>%
    collect()

# Crear matriz de flujos usando pivot_wider
matriz_flujos <- flujos_migratorios %>%
    pivot_wider(
        names_from = departamento, 
        values_from = n, 
        values_fill = 0
    ) %>%
    arrange(departamento_2013)

# Mostrar algunas estadísticas finales
total_registros <- selected_data %>%
    summarise(total = n()) %>%
    collect()

cat("Total de registros procesados:", total_registros$total, "\n")

# Top 10 municipios con mayor tasa de desempleo
municipios_con_mayor_desempleo <- selected_data %>%
    filter(pea == 1) %>%
    group_by(municipio) %>%
    summarise(
        pea_total = sum(pea, na.rm = TRUE),
        desocupado = sum(desocupado, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    filter(pea_total >= 100) %>%  # Solo municipios con al menos 100 personas en PEA
    mutate(tasa_desempleo = 100 * desocupado / pea_total) %>%
    arrange(desc(tasa_desempleo)) %>%
    collect() %>%  # Collect first, then use head()
    head(10) %>% 
    select(municipio, tasa_desempleo)

print("Top 10 municipios con mayor tasa de desempleo:")
print(municipios_con_mayor_desempleo)


# Finalizar cronómetro y mostrar tiempo total
tictoc::toc()

# Veamos algunas consultas SQL generadas por dbplyr ------------------------------------------------

selected_data %>%
    filter(pea == 1) %>%
    summarise(total_pea = n()) %>%
    show_query()

selected_data %>%
    filter(pea == 1) %>%
    group_by(municipio) %>%
    summarise(
        aneduca_prom = mean(aneduca, na.rm = TRUE),
        pea_total = sum(pea, na.rm = TRUE),
        desocupado = sum(desocupado, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(tasa_desempleo = 100 * desocupado / pea_total) %>%
    arrange(desc(tasa_desempleo)) %>%
    show_query()

# Cerrar conexión a DuckDB
dbDisconnect(con)

