graphics.off(); rm(list=ls()); cat("\014")

# --------------------------------------------------------------------------
# 0) INICIALIZACIÓN Y CARGA DE LIBRERÍAS
# --------------------------------------------------------------------------
# NOTA: Asegúrate de que esta ruta sea correcta en tu entorno.
setwd("C:/Users/David del Cid/Desktop/Programacion II/PROGRA2/PROYECTO") 


library(dplyr)
library(tidyr)
library(readxl) # <--- CORRECCIÓN CLAVE: Usamos readxl para archivos XLSX
library(tidyverse) 

# --------------------------------------------------------------------------
# 1) CARGA, LIMPIEZA Y SELECCIÓN DE DATOS DE POBLACIÓN (BASE LIMPIA XLSX)
# --------------------------------------------------------------------------

# *** RUTA DEL ARCHIVO BASE XLSX ***
# (Asumo que el archivo se llama Base_Poblacion_Limpia.xlsx y está en "input/")
file_name <- "output/Base_Poblacion_Limpia.xlsx" 
sheet_name <- "Total" # <--- Asumo que la primera pestaña se llama "Total" o es la primera hoja

# Leemos la pestaña "Total" del archivo XLSX.
poblacion_raw <- read_excel(
    path = file_name,
    sheet = sheet_name,
    col_names = TRUE
)

# 1.1. Limpieza y Reestructuración a formato largo (long format)
poblacion_tidy <- poblacion_raw %>%
    
    # Eliminar la columna de Código ("Cod")
    select(-Cod) %>% 
    
    # Renombrar la columna de departamento
    rename(DEPARTAMENTO = Departamento) %>%
    
    # Convertir de formato ancho a formato largo (Pivoteo)
    pivot_longer(
        # Selecciona todas las columnas EXCEPTO DEPARTAMENTO (los años: 2020, 2021, etc.)
        cols = -DEPARTAMENTO,  
        names_to = "Anio",
        values_to = "Poblacion_Total"
    ) %>%
    
    # Limpieza final de tipos de datos
    mutate(
        Anio = as.numeric(Anio),
        Poblacion_Total = as.numeric(Poblacion_Total)
    )

# --------------------------------------------------------------------------
# 2) CÁLCULO DE PESOS POBLACIONALES (CPI) POR AÑO Y DEPARTAMENTO
# --------------------------------------------------------------------------

# Diccionario de Regiones (Mantenemos tu mapa original)
mapa_regiones <- c(
    "Guatemala"     = "Reg. I", "El Progreso"   = "Reg. III", "Sacatepéquez"  = "Reg. V", 
    "Chimaltenango" = "Reg. V", "Escuintla"     = "Reg. V", "Santa Rosa"    = "Reg. IV", 
    "Sololá"        = "Reg. VI", "Totonicapán"   = "Reg. VI", "Quetzaltenango"= "Reg. VI", 
    "Suchitepéquez" = "Reg. VI", "Retalhuleu"    = "Reg. VI", "San Marcos"    = "Reg. VI", 
    "Huehuetenango" = "Reg. VII", "Quiché"        = "Reg. VII", "Baja Verapaz"  = "Reg. II", 
    "Alta Verapaz"  = "Reg. II", "Petén"         = "Reg. VIII", "Izabal"        = "Reg. III", 
    "Zacapa"        = "Reg. III", "Chiquimula"    = "Reg. III", "Jalapa"        = "Reg. IV", 
    "Jutiapa"       = "Reg. IV"
)

# Calculamos los pesos (CPI) para CADA AÑO
base_censo_CPI_anual <- poblacion_tidy %>%
    
    # 2.1. Asignar la región (la región no cambia por año)
    mutate(
        Region = mapa_regiones[DEPARTAMENTO]
    ) %>%
    
    # 2.2. Agrupar por AÑO y REGIÓN para calcular el Total de Personas Regional por AÑO
    # Esto asegura que el denominador sea correcto para cada año.
    group_by(Anio, Region) %>%
    mutate(
        Total_Personas_Region = sum(Poblacion_Total)
    ) %>%
    ungroup() %>%
    
    # 2.3. Renombrar la columna de población y calcular la Proporción Regional (Peso CPI)
    mutate(
        Total_Personas_Dpto = Poblacion_Total, 
        Proporcion_Regional = Total_Personas_Dpto / Total_Personas_Region
    ) %>%
    
    # 2.4. Reordenar las columnas al formato final, AGREGANDO "Anio"
    # El orden es: Anio, DEPARTAMENTO, Total_Personas_Dpto, Region, Total_Personas_Region, Proporcion_Regional
    select(
        Anio,                              
        DEPARTAMENTO, 
        Total_Personas_Dpto, 
        Region, 
        Total_Personas_Region, 
        Proporcion_Regional
    ) %>%
    arrange(Anio, DEPARTAMENTO) # Ordenar por año y departamento

# --------------------------------------------------------------------------
# 3) EXPORTACIÓN DE CPI ANUAL
# --------------------------------------------------------------------------

if (!dir.exists("./output")) {
    dir.create("./output")
}

# El archivo de salida incluye la palabra "Anual" y mantiene la compatibilidad con el formato CSV
write_csv(base_censo_CPI_anual, "./output/base_poblacion_CPI_Anual.csv")
