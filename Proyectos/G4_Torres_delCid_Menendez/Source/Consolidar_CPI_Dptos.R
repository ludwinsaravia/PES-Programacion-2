graphics.off(); rm(list=ls()); cat("\014")

# --------------------------------------------------------------------------
# 0) INICIALIZACIÓN Y CARGA DE LIBRERÍAS
# --------------------------------------------------------------------------
library(dplyr)
library(readr)
library(tidyr)
library(zoo) # Necesaria para funciones de series de tiempo como lag
library(stringr) # Necesaria para la limpieza de nombres de región

# --------------------------------------------------------------------------
# 1) CARGA DE DATOS LIMPIOS
# --------------------------------------------------------------------------

# Base 1: IPC consolidado por mes y región (generada por Extraer_datos_CPI.R)
ipc_consolidado <- read_csv("./output/ipc_republica_y_regiones.csv")

# Base 2: Pesos poblacionales anuales por departamento (base_poblacion_CPI_Anual.csv)
# Se espera que esta base contenga las columnas Anio, DEPARTAMENTO, Region y Proporcion_Regional
pesos_anuales_cpi <- read_csv("./output/base_poblacion_CPI_Anual.csv")

# --------------------------------------------------------------------------
# 2) CÁLCULO DE LA INFLACIÓN (VARIACIÓN INTERANUAL)
# --------------------------------------------------------------------------

# Orden de los meses para asegurar un cálculo de rezago (lag) correcto
orden_meses <- c(
    "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
    "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"
)

# 2.1. Preparación de la base IPC para el cálculo de la variación.
inflacion_base <- ipc_consolidado %>%
    
    # Asegurar que el Mes es un factor para el orden cronológico
    mutate(
        Mes_Num = match(Mes, orden_meses),
        Mes = factor(Mes, levels = orden_meses)
    ) %>%
    
    # Ordenar por REGIÓN, AÑO y MES para el rezago interanual (lag 12)
    arrange(Region, Anio, Mes_Num) %>%
    
    # Agrupar por REGIÓN
    group_by(Region) %>%
    
    # 2.2. Calcular el IPC del mismo mes del año anterior (IPC_Rezago)
    mutate(
        # Renombramos IPC a la columna solicitada para el resultado final
        IPC_Regional_Actual = IPC,
        # Calculamos el rezago de 12 meses
        IPC_Regional_Anterior = lag(IPC, n = 12, default = NA)
    ) %>%
    ungroup() %>%
    
    # 2.3. Calcular la Variación Interanual (Inflación)
    mutate(
        Inflacion_regional = ((IPC_Regional_Actual / IPC_Regional_Anterior) - 1)*100
    ) %>%
    
    # Seleccionar columnas de interés para la unión, incluyendo las de validación
    select(Anio, Mes, Region, IPC_Regional_Actual, IPC_Regional_Anterior, Inflacion_regional) %>%
    
    # Eliminar filas sin cálculo de variación (primeros 12 meses)
    filter(!is.na(Inflacion_regional))


# --------------------------------------------------------------------------
# 3) COMBINACIÓN DE PESOS ANUALES E INFLACIÓN MENSUAL
# --------------------------------------------------------------------------

# 3.1. Preparación de los pesos anuales (Se mantiene el detalle por DEPARTAMENTO)
pesos_limpios <- pesos_anuales_cpi %>%
    
    # Seleccionamos las claves de unión y el peso
    select(Anio, DEPARTAMENTO, Region, Proporcion_Regional) %>% 
    
    # Estandarizar el nombre de la región si es necesario (ej: "Reg. I" -> "Reg I")
    mutate(
        Region = str_replace(Region, "Reg\\.\\s", "Reg ")
    )

# 3.2. Unión de la inflación con los pesos.
base_ponderada <- inflacion_base %>%
    
    # Unir por AÑO y REGIÓN. Esto aplica el peso del departamento al año y región correctos.
    left_join(
        pesos_limpios,
        by = c("Anio", "Region") # La clave de unión para el dato anual y regional
    ) %>%
    
    # Filtramos la República (Rep) y filas sin peso, que no tienen DEPARTAMENTO
    filter(!is.na(DEPARTAMENTO))


# --------------------------------------------------------------------------
# 4) CÁLCULO DE LA CONTRIBUCIÓN PONDERADA
# --------------------------------------------------------------------------

base_final_contribucion <- base_ponderada %>%
    mutate(
        # Contribución = Peso (anual y departamental) * Inflación Interanual (mensual y regional)
        Contribucion_Inflacion_Depto = Proporcion_Regional * Inflacion_regional
    ) %>%
    
    # Seleccionar y reordenar las columnas de salida
    select(
        Anio,
        Mes,
        DEPARTAMENTO,
        Region,
        IPC_Regional_Actual,          # Nueva columna de validación
        IPC_Regional_Anterior,        # Nueva columna de validación
        Inflacion_regional,
        Proporcion_Regional,
        Contribucion_Inflacion_Depto
    ) %>%
    arrange(Anio, DEPARTAMENTO, Mes)


# --------------------------------------------------------------------------
# 5) EXPORTACIÓN DE LA BASE FINAL
# --------------------------------------------------------------------------

if (!dir.exists("./output")) {
    dir.create("./output")
}

# Exportar el resultado final
write_csv(base_final_contribucion, "./output/contribucion_inflacion_ponderada.csv")

