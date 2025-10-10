# ---------------------------------------------------------------------------------
# Script para extracción, limpieza y validación final de datos de afiliados al IGSS
# ---------------------------------------------------------------------------------

# Limpieza inicial del entorno de trabajo
graphics.off(); rm(list=ls()); cat("\014")

# 1. CARGA DE LIBRERÍAS
# ---------------------------------------------------------------------------------
library(readxl)
library(tidyverse) 

# ---------------------------------------------------------------------------------
# 2. DEFINICIÓN DE PARÁMETROS Y DICCIONARIO DE DEPARTAMENTOS
# ---------------------------------------------------------------------------------

# *** VARIABLES CLAVE: DEFINE EL RANGO DE AÑOS A INCLUIR ***
ANIO_INICIO <- 2021
ANIO_FIN <- 2023 

file_path <- "input/IGSS.xlsm"
sheet_name <- "C5"

# *** DICCIONARIO PARA CORRECCIÓN DE DEPARTAMENTOS ***
# Creamos un data frame de referencia para el mapeo.
Nombre_Deptos <- tibble::tribble(
    ~Codigo_Depto, ~Departamento,
    "1", "Guatemala",
    "2", "El Progreso",
    "3", "Sacatepéquez",
    "4", "Chimaltenango",
    "5", "Escuintla",
    "6", "Santa Rosa",
    "7", "Sololá",
    "8", "Totonicapán",
    "9", "Quetzaltenango",
    "10", "Suchitepéquez",
    "11", "Retalhuleu",
    "12", "San Marcos",
    "13", "Huehuetenango",
    "14", "Quiché",
    "15", "Baja Verapaz",
    "16", "Alta Verapaz",
    "17", "Petén",
    "18", "Izabal",
    "19", "Zacapa",
    "20", "Chiquimula",
    "21", "Jalapa",
    "22", "Jutiapa"
)

# ---------------------------------------------------------------------------------
# 3. LECTURA, REESTRUCTURACIÓN Y MAPEO DE DATOS
# ---------------------------------------------------------------------------------

# Leemos el archivo omitiendo las primeras 4 filas.
afiliados_raw <- read_excel(file_path, sheet = sheet_name, skip = 4)

# 3.1. Renombrar Columnas y Preparación para el Mapeo
afiliados_pre_long <- afiliados_raw %>%
    # Renombramos la columna del Código y la de Nombre, que R lee como genéricas.
    # Asumimos que la columna 1 es el CÓDIGO (la clave que necesitamos).
    rename(Codigo_Depto = 1, Nombre_Original = 2) %>% 
    
    # Convertimos el Código a texto para unirlo con el diccionario.
    mutate(Codigo_Depto = as.character(Codigo_Depto)) %>%
    
    # Filtramos filas de totales o vacías usando la columna de Código.
    filter(!is.na(Codigo_Depto), 
           Codigo_Depto %in% Nombre_Deptos$Codigo_Depto) %>%
    
    # Eliminamos la columna de Nombre_Original ya que usaremos el diccionario.
    select(-Nombre_Original)

# 3.2. Transformación a Formato Largo (Tidy Data Structure)
afiliados_long <- afiliados_pre_long %>%
    pivot_longer(
        cols = -Codigo_Depto, 
        names_to = "Anio",
        values_to = "Numero_Afiliados"
    )

# 3.3. Mapeo de Nombres de Departamentos y Limpieza Final de Estructura
afiliados_mapeados <- afiliados_long %>%
    # Unimos la data con el diccionario para obtener los nombres estandarizados.
    left_join(Nombre_Deptos, by = "Codigo_Depto") %>%
    
    # Eliminamos la columna de código, ya no es necesaria.
    select(-Codigo_Depto) %>%
    
    # Reordenamos las columnas a (Departamento, Año, Afiliados)
    select(Anio, Departamento, Numero_Afiliados)

# ---------------------------------------------------------------------------------
# 4. VALIDACIONES Y FILTRADO POR RANGO DE AÑOS (afiliados_final)
# ---------------------------------------------------------------------------------
afiliados_final <- afiliados_mapeados %>%
    mutate(
        # 4.1. Conversión de Año a numérico
        Anio = suppressWarnings(as.numeric(sub("^X\\_?", "", Anio))), 
        # 4.2. Conversión de Afiliados a numérico
        Numero_Afiliados = suppressWarnings(as.numeric(Numero_Afiliados))
    ) %>%
    # 4.3. Filtra la data: Mantiene solo filas con Año y Afiliados válidos (no NA)
    filter(!is.na(Numero_Afiliados), 
           !is.na(Anio)) %>%
    
    # Filtrado por el rango de años definido
    filter(Anio >= ANIO_INICIO & Anio <= ANIO_FIN)

# ---------------------------------------------------------------------------------
# 5. EXPORTACIÓN A CSV
# ---------------------------------------------------------------------------------

# Guarda los datos limpios en un archivo CSV en tu Directorio de Trabajo.
write.csv(afiliados_final, "output/afiliados_igss_Dpto.csv", row.names = FALSE)

