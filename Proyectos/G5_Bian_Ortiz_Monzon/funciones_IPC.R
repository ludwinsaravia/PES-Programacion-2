
# funciones_IPC.R
# Funciones para el procesamiento del IPC

# Importación de datos:
#ruta_archivo_2025 <- "Datos_IPC_2025.xls"
#ruta_archivo_2024 <- "Datos_IPC_2024.xls"
#ruta_ponderaciones <- "PONDERACIONES.xlsx"
ruta_archivo_2025 <- "Datos_IPC_2025.xls"
ruta_archivo_2024 <- "Datos_IPC_2024.xls"
ruta_ponderaciones <- "PONDERACIONES.xlsx"


procesar_datos_IPC <- function(ruta_2024, ruta_2025, ruta_ponderaciones) {
    # Cargar datos
    Datos_IPC_2025 <- read_excel(ruta_2025)
    Datos_IPC_2024 <- read_excel(ruta_2024)
    ponderaciones  <- read_excel(ruta_ponderaciones)
    
    # Unir datos
    IPC <- bind_rows(Datos_IPC_2024, Datos_IPC_2025)
    
    # Mapeo de mes a número
    mes_map <- c("enero"=1, "febrero"=2, "marzo"=3, "abril"=4, "mayo"=5, "junio"=6,
                 "julio"=7, "agosto"=8, "septiembre"=9, "octubre"=10, "noviembre"=11, "diciembre"=12)
    
    IPC_aux <- IPC %>%
        mutate(
            mes_minuscula = str_to_lower(Mes),
            mes_numero = mes_map[mes_minuscula],
            fecha = make_date(Año, mes_numero, 1)
        )
    
    # Filtrar códigos con 21 observaciones
    IPC_aux <- IPC_aux %>%
        group_by(Codigo) %>%
        filter(n() == 21) %>%
        ungroup()
    
    # Variaciones interanuales
    IPC_interanual <- IPC_aux %>%
        group_by(Codigo) %>%
        mutate(
            across(
                c(Republica, Region_I, Region_II, Region_III, Region_IV, 
                  Region_V, Region_VI, Region_VII, Region_VIII),
                ~ 100 * (.x / lag(.x, 12) - 1),
                .names = "{.col}_interanual"
            )
        ) %>%
        ungroup()
    
    # Unir con ponderaciones
    IPC_interanual_p <- left_join(
        IPC_interanual,
        ponderaciones %>% select(Codigo, ends_with("_peso")),
        by = "Codigo"
    )
    
    # Calcular incidencias
    regiones <- c("Republica", "Region_I", "Region_II", "Region_III", "Region_IV",
                  "Region_V", "Region_VI", "Region_VII", "Region_VIII")
    
    for(region in regiones) {
        col_incidencia <- paste0(region, "_incidencias")
        col_interanual <- paste0(region, "_interanual")
        col_peso <- paste0(region, "_peso")
        
        IPC_interanual_p[[col_incidencia]] <- (IPC_interanual_p[[col_peso]] / 100) * IPC_interanual_p[[col_interanual]]
    }
    
    # Etiquetar tipos
    IPC_interanual_p <- IPC_interanual_p %>%
        mutate(
            Codigo = as.character(Codigo),
            Numero_digitos = nchar(Codigo),
            Tipo = case_when(
                Codigo == "0" ~ "Inflacion_total",
                Codigo != "0" & Numero_digitos %in% c(1,2) ~ "Division_de_gasto",
                Numero_digitos %in% c(6,7) ~ "Producto",
                TRUE ~ "otro"
            )
        )
    
    return(IPC_interanual_p)
}

obtener_top_incidencias <- function(datos, mes_obj, region = "Republica") {
    region_incidencia <- paste0(region, "_incidencias")
    
    # Productos
    productos_mes <- datos %>%
        filter(Tipo == "Producto", fecha == mes_obj) %>%
        drop_na(all_of(region_incidencia))
    
    top_pos_prod <- productos_mes %>%
        select(Descripcion, valor = all_of(region_incidencia)) %>%
        arrange(desc(valor)) %>%
        slice_head(n = 10)
    
    top_neg_prod <- productos_mes %>%
        select(Descripcion, valor = all_of(region_incidencia)) %>%
        arrange(valor) %>%
        slice_head(n = 10)
    
    # Divisiones de gasto
    divisiones_orden <- datos %>%
        filter(Tipo == "Division_de_gasto", fecha == mes_obj) %>%
        select(Descripcion, valor = all_of(region_incidencia)) %>%
        arrange(desc(valor))
    
    return(list(
        top_pos_prod = top_pos_prod,
        top_neg_prod = top_neg_prod,
        divisiones_orden = divisiones_orden
    ))
}

calcular_metricas_resumen <- function(datos, mes_obj, region = "Republica") {
    region_incidencia <- paste0(region, "_incidencias")
    
    inflacion_total <- datos %>%
        filter(Tipo == "Inflacion_total", fecha == mes_obj) %>%
        pull(all_of(region_incidencia))
    
    top_division <- datos %>%
        filter(Tipo == "Division_de_gasto", fecha == mes_obj) %>%
        arrange(desc(.data[[region_incidencia]])) %>%
        slice(1)
    
    return(list(
        inflacion_total = inflacion_total,
        division_mayor_incidencia = top_division$Descripcion,
        valor_division_mayor = top_division[[region_incidencia]]
    ))
}

