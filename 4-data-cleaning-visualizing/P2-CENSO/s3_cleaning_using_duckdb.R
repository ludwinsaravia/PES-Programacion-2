# Limpieza y visualización de datos
# Caso practico: Analizando datos del censo de 2018
# Este archivo: Análisis y limpieza usando DuckDB (sin cargar todo en RAM)
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
library(tictoc)
library(dplyr)

# Iniciar cronómetro
tictoc::tic("Script completo - DuckDB")

# Cinco tareas: 
# 1 - Conexión a DuckDB y creación de vista de datos
# 2 - Cálculo de número de hogares por vivienda
# 3 - Cálculo de número de personas por hogar
# 4 - Cálculo de número de mujeres y hombres por municipio
# 5 - Filtrado de población económicamente activa
# 6 - Cálculo de tasa de desempleo y años de educación promedio por municipio
# 7 - Cálculo de matriz de flujos migratorios entre departamentos (2013-2018)

# 1 - Conexión a DuckDB y creación de vista de datos ----------------------------------------------

# Crear conexión a DuckDB (base de datos en memoria)
con <- dbConnect(duckdb::duckdb(), ":memory:")

# Crear una vista que selecciona solo las columnas necesarias y las renombra
dbExecute(con, "
CREATE VIEW selected_data AS 
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
")

# Crear vista con datos limpiados (departamentos y sexo mapeados)
dbExecute(con, "
CREATE VIEW cleaned_data AS
SELECT 
    CASE departamento
        WHEN 1 THEN 'Guatemala'
        WHEN 2 THEN 'El Progreso'
        WHEN 3 THEN 'Sacatepéquez'
        WHEN 4 THEN 'Chimaltenango'
        WHEN 5 THEN 'Escuintla'
        WHEN 6 THEN 'Santa Rosa'
        WHEN 7 THEN 'Sololá'
        WHEN 8 THEN 'Totonicapán'
        WHEN 9 THEN 'Quetzaltenango'
        WHEN 10 THEN 'Suchitepéquez'
        WHEN 11 THEN 'Retalhuleu'
        WHEN 12 THEN 'San Marcos'
        WHEN 13 THEN 'Huehuetenango'
        WHEN 14 THEN 'Quiché'
        WHEN 15 THEN 'Baja Verapaz'
        WHEN 16 THEN 'Alta Verapaz'
        WHEN 17 THEN 'Petén'
        WHEN 18 THEN 'Izabal'
        WHEN 19 THEN 'Zacapa'
        WHEN 20 THEN 'Chiquimula'
        WHEN 21 THEN 'Jalapa'
        WHEN 22 THEN 'Jutiapa'
        ELSE 'Desconocido'
    END as departamento,
    municipio,
    CASE departamento_2013
        WHEN 1 THEN 'Guatemala'
        WHEN 2 THEN 'El Progreso'
        WHEN 3 THEN 'Sacatepéquez'
        WHEN 4 THEN 'Chimaltenango'
        WHEN 5 THEN 'Escuintla'
        WHEN 6 THEN 'Santa Rosa'
        WHEN 7 THEN 'Sololá'
        WHEN 8 THEN 'Totonicapán'
        WHEN 9 THEN 'Quetzaltenango'
        WHEN 10 THEN 'Suchitepéquez'
        WHEN 11 THEN 'Retalhuleu'
        WHEN 12 THEN 'San Marcos'
        WHEN 13 THEN 'Huehuetenango'
        WHEN 14 THEN 'Quiché'
        WHEN 15 THEN 'Baja Verapaz'
        WHEN 16 THEN 'Alta Verapaz'
        WHEN 17 THEN 'Petén'
        WHEN 18 THEN 'Izabal'
        WHEN 19 THEN 'Zacapa'
        WHEN 20 THEN 'Chiquimula'
        WHEN 21 THEN 'Jalapa'
        WHEN 22 THEN 'Jutiapa'
        ELSE 'Desconocido'
    END as departamento_2013,
    municipio_2013,
    vivienda_id,
    hogar_id,
    persona_id,
    CASE sexo
        WHEN 1 THEN 'Hombre'
        WHEN 2 THEN 'Mujer'
        ELSE 'Desconocido'
    END as sexo,
    edad,
    aneduca,
    pea,
    ocupado,
    desocupado
FROM selected_data
")

# 2 - Cálculo de número de hogares por vivienda ----------------------------------------------------

n_hogares_por_vivienda <- dbGetQuery(con, "
SELECT 
    vivienda_id,
    COUNT(DISTINCT hogar_id) as n_hogares
FROM cleaned_data
GROUP BY vivienda_id
ORDER BY vivienda_id
")

# 3 - Cálculo de número de personas por hogar ------------------------------------------------------

n_personas_por_hogar <- dbGetQuery(con, "
SELECT 
    hogar_id,
    COUNT(DISTINCT persona_id) as n_personas
FROM cleaned_data
GROUP BY hogar_id
ORDER BY hogar_id
")

# 4 - Cálculo de número de mujeres y hombres por municipio ----------------------------------------

tabla_sexo <- dbGetQuery(con, "
SELECT 
    municipio,
    sexo,
    COUNT(*) as n
FROM cleaned_data
GROUP BY municipio, sexo
ORDER BY municipio, sexo
")

# Convertir a formato ancho usando DuckDB pivot (sintaxis alternativa)
tabla_sexo_wide <- dbGetQuery(con, "
WITH sexo_counts AS (
    SELECT municipio, sexo, COUNT(*) as n
    FROM cleaned_data
    GROUP BY municipio, sexo
)
SELECT 
    municipio,
    SUM(CASE WHEN sexo = 'Hombre' THEN n ELSE 0 END) as Hombre,
    SUM(CASE WHEN sexo = 'Mujer' THEN n ELSE 0 END) as Mujer,
    SUM(CASE WHEN sexo = 'Desconocido' THEN n ELSE 0 END) as Desconocido
FROM sexo_counts
GROUP BY municipio
ORDER BY municipio
")

# 5 - Filtrado de población económicamente activa ---------------------------------------------------

pea_count <- dbGetQuery(con, "
SELECT COUNT(*) as total_pea
FROM cleaned_data
WHERE pea = 1
")

cat("Población Económicamente Activa:", pea_count$total_pea, "personas\n")

# 6 - Cálculo de tasa de desempleo y años de educación promedio por municipio ----------------------

tabla_municipio <- dbGetQuery(con, "
SELECT 
    municipio,
    AVG(aneduca) as aneduca_prom,
    SUM(pea) as pea_total,
    SUM(desocupado) as desocupado,
    100.0 * SUM(desocupado) / SUM(pea) as tasa_desempleo
FROM cleaned_data
WHERE pea = 1
GROUP BY municipio
ORDER BY tasa_desempleo DESC
")

# 7 - Cálculo de matriz de flujos migratorios entre departamentos (2013-2018) ----------------------

# Contar flujos migratorios
flujos_migratorios <- dbGetQuery(con, "
SELECT 
    departamento_2013,
    departamento,
    COUNT(*) as n
FROM cleaned_data
WHERE departamento != departamento_2013
    AND departamento_2013 IS NOT NULL
    AND departamento IS NOT NULL
    AND departamento_2013 != 'Desconocido'
    AND departamento != 'Desconocido'
GROUP BY departamento_2013, departamento
ORDER BY departamento_2013, departamento
")

# Crear matriz de flujos usando CASE WHEN (más compatible)
matriz_flujos <- dbGetQuery(con, "
WITH flujos AS (
    SELECT 
        departamento_2013,
        departamento,
        COUNT(*) as n
    FROM cleaned_data
    WHERE departamento != departamento_2013
        AND departamento_2013 IS NOT NULL
        AND departamento IS NOT NULL
        AND departamento_2013 != 'Desconocido'
        AND departamento != 'Desconocido'
    GROUP BY departamento_2013, departamento
)
SELECT 
    departamento_2013,
    SUM(CASE WHEN departamento = 'Guatemala' THEN n ELSE 0 END) as Guatemala,
    SUM(CASE WHEN departamento = 'El Progreso' THEN n ELSE 0 END) as \"El Progreso\",
    SUM(CASE WHEN departamento = 'Sacatepéquez' THEN n ELSE 0 END) as \"Sacatepéquez\",
    SUM(CASE WHEN departamento = 'Chimaltenango' THEN n ELSE 0 END) as Chimaltenango,
    SUM(CASE WHEN departamento = 'Escuintla' THEN n ELSE 0 END) as Escuintla,
    SUM(CASE WHEN departamento = 'Santa Rosa' THEN n ELSE 0 END) as \"Santa Rosa\",
    SUM(CASE WHEN departamento = 'Sololá' THEN n ELSE 0 END) as \"Sololá\",
    SUM(CASE WHEN departamento = 'Totonicapán' THEN n ELSE 0 END) as \"Totonicapán\",
    SUM(CASE WHEN departamento = 'Quetzaltenango' THEN n ELSE 0 END) as Quetzaltenango,
    SUM(CASE WHEN departamento = 'Suchitepéquez' THEN n ELSE 0 END) as \"Suchitepéquez\",
    SUM(CASE WHEN departamento = 'Retalhuleu' THEN n ELSE 0 END) as Retalhuleu,
    SUM(CASE WHEN departamento = 'San Marcos' THEN n ELSE 0 END) as \"San Marcos\",
    SUM(CASE WHEN departamento = 'Huehuetenango' THEN n ELSE 0 END) as Huehuetenango,
    SUM(CASE WHEN departamento = 'Quiché' THEN n ELSE 0 END) as \"Quiché\",
    SUM(CASE WHEN departamento = 'Baja Verapaz' THEN n ELSE 0 END) as \"Baja Verapaz\",
    SUM(CASE WHEN departamento = 'Alta Verapaz' THEN n ELSE 0 END) as \"Alta Verapaz\",
    SUM(CASE WHEN departamento = 'Petén' THEN n ELSE 0 END) as \"Petén\",
    SUM(CASE WHEN departamento = 'Izabal' THEN n ELSE 0 END) as Izabal,
    SUM(CASE WHEN departamento = 'Zacapa' THEN n ELSE 0 END) as Zacapa,
    SUM(CASE WHEN departamento = 'Chiquimula' THEN n ELSE 0 END) as Chiquimula,
    SUM(CASE WHEN departamento = 'Jalapa' THEN n ELSE 0 END) as Jalapa,
    SUM(CASE WHEN departamento = 'Jutiapa' THEN n ELSE 0 END) as Jutiapa
FROM flujos
GROUP BY departamento_2013
ORDER BY departamento_2013
")

# Mostrar algunas estadísticas finales
total_registros <- dbGetQuery(con, "SELECT COUNT(*) as total FROM cleaned_data")
cat("Total de registros procesados:", total_registros$total, "\n")

municipios_con_mayor_desempleo <- dbGetQuery(con, "
SELECT municipio, ROUND(tasa_desempleo, 2) as tasa_desempleo
FROM (
    SELECT 
        municipio,
        100.0 * SUM(desocupado) / SUM(pea) as tasa_desempleo
    FROM cleaned_data
    WHERE pea = 1
    GROUP BY municipio
    HAVING SUM(pea) >= 100  -- Solo municipios con al menos 100 personas en PEA
)
ORDER BY tasa_desempleo DESC
LIMIT 10
")

print("Top 10 municipios con mayor tasa de desempleo:")
print(municipios_con_mayor_desempleo)

# Cerrar conexión a DuckDB
dbDisconnect(con)

# Finalizar cronómetro y mostrar tiempo total
tictoc::toc()