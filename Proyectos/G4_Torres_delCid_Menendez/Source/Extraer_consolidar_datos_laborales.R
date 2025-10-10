graphics.off(); rm(list=ls()); cat("\014")

library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(readr)

# --- 1. DEFINICIÓN DE ARCHIVOS Y COLUMNAS CLAVE ---

FILE_POBLACION_PATH <- "output/Base_Poblacion_Limpia.xlsx"
FILE_MINTRAB_PATH <- "input/MIN_TRABAJO.xlsx"

# Columnas de datos laborales a estimar, *excluyendo POBLACION_DESOCUPADA*
# para evitar ambigüedad con el nuevo cálculo 'DESEMPLEADOS'.
LABOR_COLS_BASE <- c("FUERZA_DE_TRABAJO", "POBLACION_OCUPADA", "INFORMALIDAD", "FORMALIDAD")

# --- 2. FUNCIÓN PARA CÁLCULO DE PROPORCIONES DE POBLACIÓN (SIN CAMBIOS) ---

calculate_population_proportions <- function(file_path) {
    
    pob_raw <- read_excel(path = file_path, sheet = 1) %>%
        # Renombrar para consistencia: Asumimos la columna de Departamento es la segunda
        rename(DEPARTAMENTO = 2) 
    
    pob_long <- pob_raw %>%
        select(-matches("^[Cc][Oo][Dd]")) %>%
        pivot_longer(
            cols = starts_with("20"), 
            names_to = "ANIO",
            values_to = "POBLACION"
        ) %>%
        mutate(ANIO = as.numeric(ANIO))
    
    pob_proporciones <- pob_long %>%
        group_by(ANIO) %>%
        mutate(
            PROPORCION_POBLACION = POBLACION / sum(POBLACION, na.rm = TRUE)
        ) %>%
        ungroup()
    
    return(pob_proporciones)
}

# --- 3. FUNCIÓN PARA LECTURA Y LIMPIEZA DE DATOS DEL MINISTERIO DE TRABAJO (MODIFICADA) ---

clean_mintrab_data <- function(file_path) {
    
    # Leer el archivo con los encabezados en la primera fila
    mintrab_data <- read_excel(
        path = file_path,
        sheet = 1,
        col_names = TRUE
    ) %>%
        # Seleccionar y renombrar las columnas requeridas
        select(
            ANIO,  
            FUERZA_DE_TRABAJO = `FUERZA DE TRABAJO`,
            POBLACION_OCUPADA = `POBLACION OCUPADA`,
            POBLACION_DESOCUPADA = `POBLACION DESOCUPADA`,
            INFORMALIDAD,
            FORMALIDAD
        ) %>%
        mutate(ANIO = as.numeric(ANIO)) %>%
        
        # *** AÑADIDO: CÁLCULO DE LA TASA DE DESEMPLEO ABIERTO NACIONAL ***
        # Se calcula la tasa ya que no se encontró columna explícita con ese nombre.
        mutate(
            TASA_DESEMPLEO = POBLACION_DESOCUPADA / FUERZA_DE_TRABAJO
        ) %>%
        # Se remueve POBLACION_DESOCUPADA, ya que el cálculo DESEMPLEADOS la reemplazará
        select(-POBLACION_DESOCUPADA)
    
    return(mintrab_data)
}

# -------------------------------------------------------------------
## --- 4. EJECUCIÓN, CONSOLIDACIÓN, REDONDEO Y LIMPIEZA FINAL (MODIFICADA) ---
# -------------------------------------------------------------------

# Obtener las tablas limpias
df_pob <- calculate_population_proportions(FILE_POBLACION_PATH)
df_mintrab <- clean_mintrab_data(FILE_MINTRAB_PATH)

# Se redefine LABOR_COLS para evitar problemas de compatibilidad en la función `across`
# Se incluye POBLACION_DESOCUPADA aquí *solo* si estaba en el original, pero se remueve en clean_mintrab_data.
LABOR_COLS_ESTIMAR <- c(LABOR_COLS_BASE, "POBLACION_DESOCUPADA_TEMP") 

# Unir (Join) ambas tablas por ANIO
df_consolidado <- df_pob %>%
    left_join(df_mintrab, by = "ANIO") %>%
    
    # 4.1. Calcular las estimaciones departamentales usando la proporción
    mutate(
        # Aplicar la fórmula: Valor Nacional * Proporción Departamental
        FUERZA_DE_TRABAJO = FUERZA_DE_TRABAJO * PROPORCION_POBLACION,
        POBLACION_OCUPADA = POBLACION_OCUPADA * PROPORCION_POBLACION,
        INFORMALIDAD = INFORMALIDAD * PROPORCION_POBLACION,
        FORMALIDAD = FORMALIDAD * PROPORCION_POBLACION
        # POBLACION_DESOCUPADA no se calcula aquí para usar la tasa nacional
    ) %>%
    
    # 4.2. Redondear las estimaciones de población laboral a enteros
    # Se usa LABOR_COLS_BASE, ya que POBLACION_DESOCUPADA fue excluida antes.
    mutate(across(all_of(LABOR_COLS_BASE), round, 0)) %>%
    
    # *** AÑADIDO: CÁLCULO DE LA COLUMNA DESEMPLEADOS ***
    # Cálculo solicitado: DESEMPLEADOS = round(FUERZA_DE_TRABAJO * TASA_DESEMPLEO, 0)
    mutate(
        DESEMPLEADOS = round(FUERZA_DE_TRABAJO * TASA_DESEMPLEO, 0)
    ) %>%
    
    # 4.3. REMOCIÓN DE NAs (Limpieza estricta)
    # Se verifica NA solo en las columnas base y la nueva.
    drop_na(c(PROPORCION_POBLACION, all_of(LABOR_COLS_BASE), DESEMPLEADOS)) %>%
    
    # 5. Ordenar las columnas finales
    select(
        ANIO,
        DEPARTAMENTO,
        POBLACION,
        PROPORCION_POBLACION,
        FUERZA_DE_TRABAJO,
        POBLACION_OCUPADA,
        INFORMALIDAD,
        FORMALIDAD,        
        TASA_DESEMPLEO,       # Tasa Nacional (se repite por departamento)
        DESEMPLEADOS         # Estimación por Tasa Nacional (nueva columna)
        
    )

# -------------------------------------------------------------------
# --- 5. EXPORTAR RESULTADO ---
# -------------------------------------------------------------------

# Nombre del archivo de salida
OUTPUT_FILE <- "datos_laborales_departamentales_consolidados.csv"

# Guardar el data frame consolidado en un archivo CSV en la carpeta 'output'
write_csv(df_consolidado, paste0("output/", OUTPUT_FILE), na = "")

