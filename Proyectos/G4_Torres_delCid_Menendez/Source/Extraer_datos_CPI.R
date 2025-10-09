# ==============================================================================
# Script para Filtrar IPC República (Código 0) y Extraer Columnas Regionales
#
# Descripción:
# Este script carga los datos del IPC de múltiples años, filtra para
# conservar únicamente las filas correspondientes al nivel nacional ("República",
# Código 0), y extrae las columnas de Año, Mes, el índice nacional (Rep) y los
# índices de las ocho regiones.
#
# MODIFICACIÓN CLAVE:
# Antes de exportar, los datos se reestructuran (pivotan) para consolidar
# los índices de 'Rep' y 'Reg I' a 'Reg VIII' en dos nuevas columnas:
# 'Region' y 'IPC'.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0) Inicialización y Carga de Librerías
# ------------------------------------------------------------------------------

# Cargar las librerías necesarias
# Se añade 'tidyr' para la función pivot_longer
library(dplyr)
library(readr)
library(readxl)
library(magrittr)
library(tidyr)
library(stringr) # <-- AÑADIDO: Librería necesaria para str_to_title()

# Limpiar el entorno de trabajo, gráficos y consola
graphics.off()
rm(list = ls())
cat("\014")


# ------------------------------------------------------------------------------
# 1) Carga de Todos los Datos Anuales
# ------------------------------------------------------------------------------

# NOTA: Asegúrate de tener la carpeta "./input" con los archivos Excel del IPC.

# Nombres de las columnas originales en los archivos Excel
def_names <- c("t_year", "t_month", "id_item", "descr", "ipc", "ipc_r_1",
               "ipc_r_2", "ipc_r_3", "ipc_r_4", "ipc_r_5", "ipc_r_6",
               "ipc_r_7", "ipc_r_8")

# Tipos de datos para cada columna para una carga correcta
def_type <- c("numeric", "text", "text", "text", "numeric", "numeric",
              "numeric", "numeric", "numeric", "numeric", "numeric",
              "numeric", "numeric")

# Definir el rango de años a procesar
start_year <- 2021
end_year <- 2023

# Crear una lista vacía para almacenar los data frames completos de cada año
lista_datos_completos <- list()

# Bucle para leer cada archivo y guardarlo en la lista
for (iYear in start_year:end_year) {
    # NOTA: Se asume que los archivos .xls son leídos correctamente por read_excel
    file_path <- paste0("input/IPC_", iYear, ".xls")
    cat(paste("Cargando archivo:", file_path, "\n"))
    
    # Leer el archivo y agregarlo a la lista
    lista_datos_completos[[as.character(iYear)]] <- read_excel(
        file_path, col_types = def_type, col_names = def_names
    )
}


# ------------------------------------------------------------------------------
# 2) Consolidar, Filtrar y Seleccionar
# ------------------------------------------------------------------------------

# Unir todos los data frames de la lista en una sola tabla grande
datos_consolidados <- bind_rows(lista_datos_completos)

# Eliminar las filas completamente vacías
datos_consolidados %<>% filter(!is.na(t_year))

# -- INICIO DE LA LÓGICA CLAVE (Filtrar y Seleccionar) --
# Proceso intermedio: Filtrar por Código "0" y luego seleccionar las columnas deseadas
datos_intermedios <- datos_consolidados %>%
    
    # <--- MODIFICACIÓN PARA CAPITALIZAR MESES --->
    # Asegura que la columna t_month esté en formato "Enero", "Febrero", etc.
    mutate(
        t_month = str_to_title(t_month) 
    ) %>%
    # <--- FIN DE MODIFICACIÓN --->
    
    # Paso 1: Filtrar para obtener solo el IPC General (Código "0").
    filter(id_item == "0" | is.na(id_item)) %>%
    
    # Paso 2: Seleccionar y renombrar las columnas de interés.
    select(
        Anio = t_year,
        Mes = t_month,
        Rep = ipc,
        `Reg I` = ipc_r_1,
        `Reg II` = ipc_r_2,
        `Reg III` = ipc_r_3,
        `Reg IV` = ipc_r_4,
        `Reg V` = ipc_r_5,
        `Reg VI` = ipc_r_6,
        `Reg VII` = ipc_r_7,
        `Reg VIII` = ipc_r_8
    ) %>%
    
    # Paso 3: Ordenar por año para asegurar un orden cronológico.
    arrange(Anio)

# Se usa pivot_longer para transformar las columnas de IPC ('Rep', 'Reg I'...)
# en filas, creando la columna 'Region' con los nombres y la columna 'IPC' con los valores.

datos_finales <- datos_intermedios %>%
    pivot_longer(
        # Especifica qué columnas quieres pivotar. Se usa -c() para pivotar
        # todas las columnas EXCEPTO Anio y Mes (las columnas identificadoras).
        cols = -c(Anio, Mes),
        # Nombre de la nueva columna que contendrá los nombres de las regiones (Rep, Reg I, etc.)
        names_to = "Region",
        # Nombre de la nueva columna que contendrá los valores del IPC
        values_to = "IPC"
    )


# Crear el directorio de salida si no existe
if (!dir.exists("./output")) {
    dir.create("./output")
}

# Exportar la base de datos final a un archivo CSV
write_csv(datos_finales, "./output/ipc_republica_y_regiones.csv")
