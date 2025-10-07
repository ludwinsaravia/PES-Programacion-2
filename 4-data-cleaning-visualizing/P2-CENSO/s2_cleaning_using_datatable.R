# Limpieza y visualización de datos
# Caso practico: Analizando datos del censo de 2018
# Este archivo: Análisis y limpieza usando data.table
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
library(data.table)
library(tictoc)

# Iniciar cronómetro
tictoc::tic("Script completo - data.table")

# Cinco tareas: 
# 1 - Carga y limpieza de datos
# 2 - Cálculo de número de hogares por vivienda
# 3 - Cálculo de número de personas por hogar
# 4 - Cálculo de número de mujeres y hombres por municipio
# 5 - Filtrado de población económicamente activa
# 6 - Cálculo de tasa de desempleo y años de educación promedio por municipio
# 7 - Cálculo de matriz de flujos migratorios entre departamentos (2013-2018)

# 1 - Carga y limpieza de datos --------------------------------------------------------------------

# Usando data.table::fread (más rápido que read_csv)
# censo <- fread("./input/censo.csv", nrows = 1000)
censo <- fread("./input/censo.csv", 
               select = c(
                   "DEPARTAMENTO",
                   "MUNICIPIO",
                   "PCP11_B",
                   "RESCINGEO",
                   "NUM_VIVIENDA",
                   "NUM_HOGAR",
                   "PCP1",
                   "PCP6",
                   "PCP7", 
                   "ANEDUCA",
                   "PEA",
                   "POCUPA",
                   "PDESOC") )


# Seleccionar y renombrar variables de interés
selected_data <- censo[, .(
    departamento      = DEPARTAMENTO,
    municipio         = MUNICIPIO,
    departamento_2013 = PCP11_B,
    municipio_2013    = RESCINGEO,
    vivienda_id       = NUM_VIVIENDA,
    hogar_id          = NUM_HOGAR,
    persona_id        = PCP1,
    sexo              = PCP6,
    edad              = PCP7,
    aneduca           = ANEDUCA,
    pea               = PEA,
    ocupado           = POCUPA,
    desocupado        = PDESOC
)]

# Veamos la estructura de los datos
str(selected_data)

# Reemplazar NA en pea, ocupado y desocupado por 0
selected_data[is.na(pea), pea := 0]
selected_data[is.na(ocupado), ocupado := 0]
selected_data[is.na(desocupado), desocupado := 0]

# Crear mapeo de departamentos
dept_mapping <- c(
    "1"  = "Guatemala",
    "2"  = "El Progreso", 
    "3"  = "Sacatepéquez",
    "4"  = "Chimaltenango",
    "5"  = "Escuintla",
    "6"  = "Santa Rosa",
    "7"  = "Sololá",
    "8"  = "Totonicapán",
    "9"  = "Quetzaltenango",
    "10" = "Suchitepéquez",
    "11" = "Retalhuleu",
    "12" = "San Marcos",
    "13" = "Huehuetenango",
    "14" = "Quiché",
    "15" = "Baja Verapaz",
    "16" = "Alta Verapaz",
    "17" = "Petén",
    "18" = "Izabal",
    "19" = "Zacapa",
    "20" = "Chiquimula",
    "21" = "Jalapa",
    "22" = "Jutiapa"
)

# Mapear departamentos a nombres
selected_data[, departamento := dept_mapping[as.character(departamento)]]
selected_data[, departamento_2013 := dept_mapping[as.character(departamento_2013)]]

# Mapear sexo a nombres
selected_data[sexo == 1, sexo := "Hombre"]
selected_data[sexo == 2, sexo := "Mujer"]

# Limpiar memoria
rm(censo)
gc()

# 2 - Cálculo de número de hogares por vivienda ----------------------------------------------------

n_hogares_por_vivienda <- selected_data[, .(n_hogares = uniqueN(hogar_id)), by = vivienda_id]

# 3 - Cálculo de número de personas por hogar ------------------------------------------------------

n_personas_por_hogar <- selected_data[, .(n_personas = uniqueN(persona_id)), by = hogar_id]

# 4 - Cálculo de número de mujeres y hombres por municipio ----------------------------------------

# Método data.table para crear tabla cruzada
tabla_sexo <- selected_data[, .N, by = .(municipio, sexo)]
tabla_sexo_wide <- dcast(tabla_sexo, municipio ~ sexo, value.var = "N", fill = 0)

# 5 - Filtrado de población económicamente activa ---------------------------------------------------

pea_data <- selected_data[pea == 1]

# 6 - Cálculo de tasa de desempleo y años de educación promedio por municipio ----------------------

tabla_municipio <- selected_data[pea == 1, .(
    aneduca_prom = mean(aneduca, na.rm = TRUE),
    pea_total    = sum(pea, na.rm = TRUE),
    desocupado   = sum(desocupado, na.rm = TRUE)
), by = municipio]

# Calcular tasa de desempleo y ordenar
tabla_municipio[, tasa_desempleo := 100 * desocupado / pea_total]
setorder(tabla_municipio, -tasa_desempleo)

# 7 - Cálculo de matriz de flujos migratorios entre departamentos (2013-2018) ----------------------

# Filtrar migrantes y contar flujos
flujos_migratorios <- selected_data[departamento != departamento_2013, .N, 
                                    by = .(departamento_2013, departamento)]

# Crear matriz de flujos (convertir a formato ancho)
matriz_flujos <- dcast(flujos_migratorios, departamento_2013 ~ departamento, 
                       value.var = "N", fill = 0)

# Ordenar por departamento de origen
setorder(matriz_flujos, departamento_2013)

# Finalizar cronómetro y mostrar tiempo total
tictoc::toc()