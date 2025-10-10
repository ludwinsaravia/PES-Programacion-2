graphics.off(); rm(list=ls()); cat("\014") # Limpia el entorno y la consola

library(openxlsx)
library(dplyr)
library(tibble)
library(tidyr)
library(readr) 
library(stringr)

# --- 1. CONFIGURACIÓN Y TABLAS DE REFERENCIA ---

# Definimos el año de la ENCOVI
ANIO_PROCESO <- 2023 

# Rutas de Archivo (CORREGIDAS A XLSX)
RUTA_ENCOVI <- "input/ENCOVI_2023.xlsx"
RUTA_POBLACION <- "output/Base_Poblacion_Limpia.xlsx" 
HOJA_POBLACION <- "Total" 

cols_encovi <- c("DEPTO", "P06B01") 
# La columna deseada es la '2023' del archivo de población
cols_poblacion_deseadas <- c("Departamento", "2023") 

# Definición de la tabla de códigos de departamentos (Sin cambios)
Nombre_Deptos <- tibble::tribble(
    ~Codigo_Depto, ~Departamento,
    "1", "Guatemala", "2", "El Progreso", "3", "Sacatepéquez", "4", "Chimaltenango",
    "5", "Escuintla", "6", "Santa Rosa", "7", "Sololá", "8", "Totonicapán",
    "9", "Quetzaltenango", "10", "Suchitepéquez", "11", "Retalhuleu", "12", "San Marcos",
    "13", "Huehuetenango", "14", "Quiché", "15", "Baja Verapaz", "16", "Alta Verapaz",
    "17", "Petén", "18", "Izabal", "19", "Zacapa", "20", "Chiquimula",
    "21", "Jalapa", "22", "Jutiapa"
)

# --- 2. EXTRACCIÓN Y LIMPIEZA DE ENCOVI ---

hdr_e <- read.xlsx(RUTA_ENCOVI, sheet = 1, rows = 1, colNames = TRUE) 
idx_e <- match(cols_encovi, names(hdr_e))
df_encovi <- read.xlsx(xlsxFile = RUTA_ENCOVI, sheet = 1, cols = idx_e, colNames = TRUE)

df_encovi_limpio <- df_encovi %>%
    drop_na(DEPTO, P06B01) %>%
    rename(Codigo_Depto = DEPTO) %>%
    mutate(Codigo_Depto = as.character(Codigo_Depto)) %>%
    left_join(Nombre_Deptos, by = "Codigo_Depto") %>%
    select(
        DEPTO = Departamento, 
        P06B01               
    )

# --- 3. AGRUPACIÓN Y PIVOTEO DE INDICADORES ---

df_agrupado <- df_encovi_limpio %>%
    pivot_longer(
        cols = c(P06B01), 
        names_to = "VARIABLE",
        values_to = "VALOR" 
    ) %>%
    group_by(DEPTO, VARIABLE, VALOR) %>%
    summarise(
        CONTEO_REGISTROS = n(), 
        .groups = 'drop'
    ) %>%
    mutate(
        INDICADOR_FINAL = paste(VARIABLE, VALOR, sep = "_")
    ) %>%
    pivot_wider(
        id_cols = DEPTO, 
        names_from = INDICADOR_FINAL, 
        values_from = CONTEO_REGISTROS, 
        values_fill = 0 
    )

# -------------------------------------------------------------------
## 4. EXTRACCIÓN Y UNIÓN DE LA POBLACIÓN 2023 (CORREGIDO)
# -------------------------------------------------------------------

# Cargamos el encabezado para obtener el índice de la columna '2023'
hdr_p <- read.xlsx(RUTA_POBLACION, sheet = HOJA_POBLACION, rows = 1, colNames = TRUE) 
idx_p <- match(cols_poblacion_deseadas, names(hdr_p))

# Lectura del XLSX
df_poblacion_2023 <- read.xlsx(
    xlsxFile = RUTA_POBLACION,
    sheet = HOJA_POBLACION,
    cols = idx_p,
    colNames = TRUE
) %>%
    rename(
        DEPTO = Departamento,
        POBLACION_2023 = `2023`
    ) %>%
    mutate(DEPTO = str_trim(DEPTO))

df_agrupado_con_poblacion <- df_agrupado %>%
    left_join(df_poblacion_2023, by = "DEPTO") %>%
    relocate(POBLACION_2023, .after = DEPTO)


# -------------------------------------------------------------------
## 5. CÁLCULO DE MÉTRICAS CLAVE (TOTAL, %Analfabetismo y AÑO)
# -------------------------------------------------------------------

df_final_calculado <- df_agrupado_con_poblacion %>%
    
    # Asegurar que las columnas existan y sean numéricas
    mutate(
        P06B01_1 = as.numeric(P06B01_1),
        P06B01_2 = as.numeric(P06B01_2)
    ) %>%
    
    # 5.1. Calcular el total y el porcentaje de Analfabetismo
    mutate(
        TOTAL = P06B01_1 + P06B01_2, 
        `%Analfabetismo` = round((P06B01_2 / TOTAL) * 100, 2),
        
        # 5.2. Agregar la columna ANIO con el valor 2023
        ANIO = ANIO_PROCESO 
    ) %>%
    
    # 5.3. Reordenar las columnas: ANIO debe ir primero
    relocate(ANIO, .before = DEPTO) %>%
    relocate(TOTAL, `%Analfabetismo`, .after = P06B01_2) # Reordenar métricas


# -------------------------------------------------------------------
## 6. EXPORTAR RESULTADO
# -------------------------------------------------------------------

OUTPUT_FILE <- "encovi_analisis_analfabetismo_2023.csv"
write_csv(df_final_calculado, paste0("output/", OUTPUT_FILE))
