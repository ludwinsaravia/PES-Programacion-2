
graphics.off(); rm(list=ls()); cat("\014")


library(dplyr)
library(readr)
library(stringr)

# --- 1. DEFINICIÓN DE ARCHIVOS ---

FILE_AFILIADOS <- "output/afiliados_igss_Dpto.csv"
FILE_CONSOLIDADO <- "output/datos_laborales_departamentales_consolidados.csv"

# --- 2. LECTURA Y PREPARACIÓN DE DATOS ---

# 2.1. Leer los datos de Afiliados IGSS
df_afiliados <- read_csv(FILE_AFILIADOS, show_col_types = FALSE) %>%
    # Limpiar y renombrar las columnas para una unión consistente
    rename(
        ANIO = Anio,
        DEPARTAMENTO = Departamento
    ) %>%
    # Asegurarse de que el año sea numérico
    mutate(ANIO = as.numeric(ANIO))

# 2.2. Leer los datos consolidados (incluye FUERZA_DE_TRABAJO)
df_consolidado <- read_csv(FILE_CONSOLIDADO, show_col_types = FALSE) %>%
    # Seleccionar solo las columnas necesarias para la unión
    select(ANIO, DEPARTAMENTO, POBLACION_OCUPADA)


# --- 3. CRUCE DE DATOS Y CÁLCULO ---

df_resultado <- df_afiliados %>%
    # Unir (Join) la tabla de afiliados con la Fuerza de Trabajo
    # La unión se realiza por ANIO y DEPARTAMENTO
    left_join(df_consolidado, by = c("ANIO", "DEPARTAMENTO")) %>%
    
    # 3.1. Calcular el Porcentaje de Afiliados IGSS
    mutate(
        # Fórmula: (Número_Afiliados / FUERZA_DE_TRABAJO) * 100
        Porcentaje_Afiliados_IGSS = (Numero_Afiliados / POBLACION_OCUPADA) * 100
    ) %>%
    
    # 3.2. Redondeo opcional del porcentaje para mejor visualización
    mutate(
        Porcentaje_Afiliados_IGSS = round(Proporcion_Afiliados_IGSS, 2)
    )

# --- 4. EXPORTAR RESULTADO ---

OUTPUT_FILE <- "output/afiliados_igss_calculado.csv"

# Guardar el data frame resultante en un nuevo archivo CSV
write_csv(df_resultado, OUTPUT_FILE)

