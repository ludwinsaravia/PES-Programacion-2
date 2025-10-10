graphics.off(); rm(list=ls()); cat("\014")


# --------------------------------------------------------------------------
# 0) INICIALIZACIÓN Y CARGA DE LIBRERÍAS
# --------------------------------------------------------------------------
# NOTA: Mantengo el setwd que tenías, pero asegúrate de que sea la ruta correcta
setwd("C:/Users/David del Cid/Desktop/Programacion II/PROGRA2/PROYECTO") 


library(shiny)
library(readxl)
library(jsonlite)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(leaflet)
library(data.table) # Aseguramos la carga de data.table para fread


# --------------------------------------------------------------------------
# 1) CARGA Y SELECCIÓN DE DATOS DEL CENSO
# --------------------------------------------------------------------------
censo <- data.table::fread(
    "input/PERSONA - BDP.csv",
    encoding = "UTF-8", 
    # Seleccionamos DEPARTAMENTO, PEA y otras variables si son necesarias
    select = c("DEPARTAMENTO","PCP6","PCP7","PCP17_A","ANEDUCA","PEA","POCUPA","PDESOC","PEI")
)

# --------------------------------------------------------------------------
# 2) MAPEO Y LIMPIEZA INICIAL
# --------------------------------------------------------------------------

Nombre_Deptos <- c(
    "1" = "Guatemala", 
    "2" = "El Progreso", 
    "3" = "Sacatepéquez", 
    "4" = "Chimaltenango",
    "5" = "Escuintla",
    "6" = "Santa Rosa",
    "7" = "Sololá",
    "8" = "Totonicapán",
    "9" = "Quetzaltenango",
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

mapa_regiones <- c(
    "Guatemala"     = "Reg. I",
    "El Progreso"     = "Reg. III",
    "Sacatepéquez"    = "Reg. V",
    "Chimaltenango"   = "Reg. V",
    "Escuintla"     = "Reg. V",
    "Santa Rosa"    = "Reg. IV",
    "Sololá"    = "Reg. VI",
    "Totonicapán"   = "Reg. VI",
    "Quetzaltenango"  = "Reg. VI",
    "Suchitepéquez"   = "Reg. VI",
    "Retalhuleu"    = "Reg. VI",
    "San Marcos"    = "Reg. VI",
    "Huehuetenango"   = "Reg. VII",
    "Quiché"    = "Reg. VII",
    "Baja Verapaz"    = "Reg. II",
    "Alta Verapaz"    = "Reg. II",
    "Petén"     = "Reg. VIII",
    "Izabal"      = "Reg. III",
    "Zacapa"      = "Reg. III",
    "Chiquimula"    = "Reg. III",
    "Jalapa"      = "Reg. IV",
    "Jutiapa"     = "Reg. IV"
)

Sexo <- c(
    "1" = "Hombre",
    "2" = "Mujer"
)

# Aplicar mapeo de códigos a nombres
censo <- censo %>%
    mutate(
        DEPARTAMENTO = Nombre_Deptos[as.character(DEPARTAMENTO)],
        PCP6 = Sexo[as.character(PCP6)])

unique(censo$DEPARTAMENTO)


# --------------------------------------------------------------------------
# 3) GENERACIÓN DE BASE CENSO PEA (TOTAL por Departamento)
# --------------------------------------------------------------------------

# Filtramos las filas que tienen un departamento válido y algún valor en PEA.
# Agrupamos por DEPARTAMENTO y sumamos todos los registros que tienen un valor en PEA,
# asumiendo que cualquier valor en la columna PEA indica una persona activa.

base_censo_PEA_Total <- censo %>%
    filter(!is.na(DEPARTAMENTO)) %>%
    group_by(DEPARTAMENTO) %>% 
    summarise(
        # Contamos cuántas personas tienen un valor no-NA en la columna PEA.
        # Esto nos da el total de la población clasificada como PEA por el censo.
        PEA_Depto = sum(!is.na(PEA)), 
        .groups = 'drop'
    ) %>%
    arrange(DEPARTAMENTO)

# --------------------------------------------------------------------------
# 4) GENERACIÓN Y EXPORTACIÓN DE PEA TOTAL
# --------------------------------------------------------------------------

if (!dir.exists("./output")) {
    dir.create("./output")
}

write_csv(base_censo_PEA_Total, "./output/base_censo_PEA.csv")


# --------------------------------------------------------------------------
# 5) GENERACIÓN DE BASE CENSO CPI (Pesos Poblacionales)
# --------------------------------------------------------------------------

base_censo_CPI <- censo %>%
    group_by(DEPARTAMENTO) %>%
    summarise(
        Total_Personas_Dpto = n(),
        .groups = 'drop' 
    ) %>%
    
    # 1. Asignar la región usando el mapeo definido
    mutate(
        Region = mapa_regiones[DEPARTAMENTO]
    ) %>%
    
    # 2. Calcular el total de personas por región (para el denominador de la proporción)
    group_by(Region) %>%
    mutate(
        Total_Personas_Region = sum(Total_Personas_Dpto)
    ) %>%
    ungroup() %>%
    
    # 3. Calcular la Proporción Regional (Peso)
    mutate(
        Proporcion_Regional = Total_Personas_Dpto / Total_Personas_Region
    ) %>%
    
    # 4. Reordenar las columnas
    select(
        DEPARTAMENTO, 
        Total_Personas_Dpto, 
        Region, 
        Total_Personas_Region, 
        Proporcion_Regional
    ) %>%
    arrange(DEPARTAMENTO)

# --------------------------------------------------------------------------
# 6) EXPORTACIÓN DE CPI
# --------------------------------------------------------------------------

write_csv(base_censo_CPI, "./output/base_censo_CPI.csv")

