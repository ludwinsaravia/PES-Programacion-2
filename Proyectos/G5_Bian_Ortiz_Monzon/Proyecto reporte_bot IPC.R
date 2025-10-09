# Librerías
graphics.off()
rm(list=ls())

# Paquetes
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(janitor)
library(stringr)
library(lubridate)
library(glue)

# Importación de datos:

#ruta_archivo_2025 <- "Datos_IPC_2025.xls"
#ruta_archivo_2024 <- "Datos_IPC_2024.xls"
#ruta_ponderaciones <- "PONDERACIONES.xlsx"
ruta_archivo_2025 <- "Datos_IPC_2025.xls"
ruta_archivo_2024 <- "Datos_IPC_2024.xls"
ruta_ponderaciones <- "PONDERACIONES.xlsx"



Datos_IPC_2025 <- read_excel(ruta_archivo_2025)
Datos_IPC_2024 <- read_excel(ruta_archivo_2024)
ponderaciones  <- read_excel(ruta_ponderaciones)


# Tratamiento de datos
IPC <- bind_rows(Datos_IPC_2024, Datos_IPC_2025) # Unimos datos de 2024 y 2025

# Mapeo de mes a número
mes_map <- c("enero"=1, "febrero"=2, "marzo"=3, "abril"=4, "mayo"=5, "junio"=6,
             "julio"=7, "agosto"=8, "septiembre"=9, "octubre"=10, "noviembre"=11, "diciembre"=12)

IPC_aux <- IPC %>% # 3 nuevas columnas:
    mutate( mes_minuscula  = str_to_lower(Mes), # Crea nueva columna con los meses en minúsculas
            mes_numero     = mes_map[mes_minuscula], # Se asocia el nombre "enero" con el 1, tipo BUSCARV en Excel
            fecha          = make_date(Año, mes_numero, 1) # fecha que empieza en el día 1 del mes x y el año z
    )

# Tabla de frecuencias por Codigo
tabla_de_frecuencias <- table(IPC_aux$Codigo)

# Esperamos que hayan 21 observaciones (12 de 2024 y 9 de 2025)
codigos_con_problema_base <- tabla_de_frecuencias[tabla_de_frecuencias != 21]

if (length(codigos_con_problema_base) == 0) {
    message("Todos los Códigos tienen 21 observaciones")
} else {
    message("Hay Códigos que no tienen 21 observaciones")
}

# Convertir la tabla de frecuencias a un data frame para mejor visualización
tab_df <- as.data.frame(tabla_de_frecuencias)
names(tab_df) <- c("Codigo", "n_obs")  # Renombramos columnas

# Nos quedamos solo con los que tienen 21 observaciones
IPC_aux <- IPC_aux %>%
    group_by(Codigo) %>%
    filter(n() == 21) %>% # Filtramos solo los que tienen 21 observaciones
    ungroup()

# Volvemos a sacar la tabla de frecuencias por Codigo
tabla_de_frecuencias <- table(IPC_aux$Codigo)

# Esperamos que hayan 21 observaciones (12 de 2024 y 9 de 2025)
codigos_con_problema_base <- tabla_de_frecuencias[tabla_de_frecuencias != 21]

if (length(codigos_con_problema_base) == 0) {
    message("Todos los Códigos tienen 21 observaciones")
} else {
    message("Hay Códigos que no tienen 21 observaciones")
}

# Vemos que efectivamente hayan 21 observaciones por código
tab_df <- as.data.frame(tabla_de_frecuencias)
names(tab_df) <- c("Codigo", "n_obs")  # Renombramos columnas

# Variaciones interanuales:

IPC_interanual <- IPC_aux %>%
    group_by(Codigo) %>% # A cada elemento único de la columna de "Codigo" le realiza la operación de variación interanual. Y lo hace para todas las columnas: República, Region I, etc.
    mutate(
        Republica_interanual   = 100 * (Republica   / lag(Republica,   12) - 1),
        Region_I_interanual    = 100 * (Region_I    / lag(Region_I,    12) - 1),
        Region_II_interanual   = 100 * (Region_II   / lag(Region_II,   12) - 1),
        Region_III_interanual  = 100 * (Region_III  / lag(Region_III,  12) - 1),
        Region_IV_interanual   = 100 * (Region_IV   / lag(Region_IV,   12) - 1),
        Region_V_interanual    = 100 * (Region_V    / lag(Region_V,    12) - 1),
        Region_VI_interanual   = 100 * (Region_VI   / lag(Region_VI,   12) - 1),
        Region_VII_interanual  = 100 * (Region_VII  / lag(Region_VII,  12) - 1),
        Region_VIII_interanual = 100 * (Region_VIII / lag(Region_VIII, 12) - 1)
    ) %>%
    ungroup()


# Poniendo los pesos a cada elemento
IPC_interanual_p <- left_join(IPC_interanual,
                              ponderaciones[,c("Codigo","Republica_peso","Region_I_peso","Region_II_peso","Region_III_peso",
                                               "Region_IV_peso","Region_V_peso","Region_VI_peso","Region_VII_peso","Region_VIII_peso")],
                              by = "Codigo")

# Incidencias = Variación interanual * peso /100
IPC_interanual_p$Republica_incidencias    <- (IPC_interanual_p$Republica_peso    / 100) * IPC_interanual_p$Republica_interanual
IPC_interanual_p$Region_I_incidencias     <- (IPC_interanual_p$Region_I_peso     / 100) * IPC_interanual_p$Region_I_interanual
IPC_interanual_p$Region_II_incidencias    <- (IPC_interanual_p$Region_II_peso    / 100) * IPC_interanual_p$Region_II_interanual
IPC_interanual_p$Region_III_incidencias   <- (IPC_interanual_p$Region_III_peso   / 100) * IPC_interanual_p$Region_III_interanual
IPC_interanual_p$Region_IV_incidencias    <- (IPC_interanual_p$Region_IV_peso    / 100) * IPC_interanual_p$Region_IV_interanual
IPC_interanual_p$Region_V_incidencias     <- (IPC_interanual_p$Region_V_peso     / 100) * IPC_interanual_p$Region_V_interanual
IPC_interanual_p$Region_VI_incidencias    <- (IPC_interanual_p$Region_VI_peso    / 100) * IPC_interanual_p$Region_VI_interanual
IPC_interanual_p$Region_VII_incidencias   <- (IPC_interanual_p$Region_VII_peso   / 100) * IPC_interanual_p$Region_VII_interanual
IPC_interanual_p$Region_VIII_incidencias  <- (IPC_interanual_p$Region_VIII_peso  / 100) * IPC_interanual_p$Region_VIII_interanual


# Etiquetamos la información como Inflación General, Divisiones de gasto y Productos
IPC_interanual_p <- IPC_interanual_p %>%
    mutate(
        Codigo = as.character(Codigo),
        Numero_digitos= nchar(Codigo),
        Tipo = case_when(
            Codigo == "0"                              ~ "Inflacion_total",   # IPC general
            Codigo != "0" & Numero_digitos %in% c(1,2) ~ "Division_de_gasto", # divisiones
            Numero_digitos %in% c(6,7)                 ~ "Producto",          # productos
            TRUE                                       ~ "otro"
        )
    )

# Verificamos que no hayan filas con "otro"
if (any(IPC_interanual_p$Tipo == "otro")) {
    warning("Hay filas con Tipo 'otro'. Revisa los datos.")
} else {
    message("No hay filas con Tipo 'otro'. Todo en orden.")
}
#####################################################################################################

# Top incidencias de productos a nivel nacional:
mes_obj <- as.Date("2025-09-01")  # Seleccionamos el mes que queremos analizar

productos_mes <- IPC_interanual_p %>% 
    filter(Tipo == "Producto", # Seleccionamos la etiqueta de productos para el mes objetivo
           fecha == mes_obj) %>%
    drop_na(Republica_incidencias) # Eliminar filas con NA en Republica_incidencias

# Top 10 incidencias positivas
top_pos_prod <- productos_mes %>%
    select(Descripcion, Republica_incidencias) %>%
    arrange(desc(Republica_incidencias)) %>%
    slice_head(n = 10)

# Top 10 incidencias negativas
top_neg_prod <- productos_mes %>%
    select(Descripcion, Republica_incidencias) %>%
    arrange(Republica_incidencias) %>%
    slice_head(n = 10)

# Top incidencias de Divisiones de Gasto
divisiones_orden <- IPC_interanual_p %>%
    filter(Tipo == "Division_de_gasto", fecha == mes_obj) %>%
    select(Descripcion, Republica_incidencias) %>%
    arrange(desc(Republica_incidencias))

#####################################################################################################
# Top incidencias a nivel de la región deseada

region_deseada <- "Region_I_incidencias"
mes_obj_region <- as.Date("2025-09-01")  # Seleccionamos el mes que queremos analizar

nombres_regiones <- c(
    "Republica_incidencias"   = "República",
    "Region_I_incidencias"    = "Región I",
    "Region_II_incidencias"   = "Región II",
    "Region_III_incidencias"  = "Región III",
    "Region_IV_incidencias"   = "Región IV",
    "Region_V_incidencias"    = "Región V",
    "Region_VI_incidencias"   = "Región VI",
    "Region_VII_incidencias"  = "Región VII",
    "Region_VIII_incidencias" = "Región VIII")
titulo_region <- nombres_regiones[[region_deseada]] %||% pretty_region_label(region_deseada)

# Mismo procedimiento anterior, a nivel regional
productos_mes_region <- IPC_interanual_p %>% # Seleccionamos los productos del mes objetivo
    filter(Tipo == "Producto",
           fecha == mes_obj_region) %>%
    drop_na(all_of(region_deseada)) # Eliminar filas con NA en región deseada

# Incidencias incidencias positivas
top_pos_prod_region <- productos_mes_region %>%
    transmute(Descripcion, valor = .data[[region_deseada]]) %>%
    arrange(desc(valor)) %>%
    slice_head(n = 10)

# Top 10 incidencias negativas
top_neg_prod_region <- productos_mes_region %>%
    transmute(Descripcion, valor = .data[[region_deseada]]) %>%
    arrange(valor) %>%
    slice_head(n = 10)

# Top incidencias de Divisiones de Gasto
divisiones_orden_region <- IPC_interanual_p %>%
    filter(Tipo == "Division_de_gasto",
           fecha == mes_obj_region) %>%
    transmute(Descripcion, valor = .data[[region_deseada]]) %>%
    arrange(desc(valor))

# Inflación General: Incidencias por división de gasto
ggplot(divisiones_orden %>% mutate(Descripcion = str_wrap(Descripcion, 35)), # Estética. Necesario para que las etiquetas largas no afecten el gráfico. Agrega saltos de líneas
       aes(x = reorder(Descripcion, Republica_incidencias), y = Republica_incidencias)) + # Reordena las barras de mayor a menor incidencia
    geom_col(fill = "#142994") +
    coord_flip() + # Rota el gráfico para mejor visualización (etiquetas en eje Y)
    labs(
        title = "Inflación General - Nacional",  # Título principal
        subtitle = paste("Incidencia por división de gasto -", format(mes_obj, "%B %Y")),  # Segundo título. Concatena sutítulo con el mes
        x = NULL, y = "Incidencia (p.p.)"  # Etiquetas de los ejes
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", size = 14), # Título en negrita tamaño 14
        plot.subtitle = element_text(size = 12) # Subtítulo tamaño 12
    )

# Inflación General: Incidencias POSITIVAS por producto
top_pos_prod %>%
    mutate(
        Descripcion = str_wrap(Descripcion, 40), # Parte las etiquetas largas en varias líneas
        intensidad = Republica_incidencias
    ) %>%
    ggplot(aes(x = reorder(Descripcion, Republica_incidencias), y = Republica_incidencias)) +
    geom_col(aes(fill = intensidad)) +
    coord_flip() +
    labs(
        title = "Inflación General - Nacional",
        subtitle = paste("Top 10 Incidencias Positivas por Producto -", format(mes_obj, "%B %Y")),
        x = NULL, y = "Incidencia (p.p.)", fill = NULL
    ) +
    scale_fill_gradientn(
        colors = c("#FAD2D2", "#F28B82", "#E53935"), # Gradiente de rojos
        trans = "sqrt"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 12),
        legend.position = "none"
    )


# Inflación General: Incidencias NEGATIVAS por producto
top_neg_prod %>%
    mutate(
        Descripcion = str_wrap(Descripcion, 40),
        intensidad = -Republica_incidencias
    ) %>%
    ggplot(aes(x = reorder(Descripcion, Republica_incidencias, decreasing = TRUE), y = Republica_incidencias)) +
    geom_col(aes(fill = intensidad)) +
    coord_flip() +
    labs(
        title = "Inflación General - Nacional",
        subtitle = paste("Top 10 Incidencias Negativas por Producto -", format(mes_obj, "%B %Y")),
        x = NULL, y = "Incidencia (p.p.)", fill = NULL
    ) +
    scale_fill_gradientn(
        colors = c("#C6E2F3", "#8FBFDA", "#5D9EC7", "#2E7FAF", "#0B5D7A"), # Gradiente de azules
        trans = "sqrt"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 12),
        legend.position = "none"
    )

# Inflación General por Region: Incidencias por división de gasto
ggplot(divisiones_orden_region %>% mutate(Descripcion = str_wrap(Descripcion, 35)),
       aes(x = reorder(Descripcion, valor), y = valor)) +
    geom_col(fill = "#142994") +
    coord_flip() +
    labs(
        title = glue("Inflación General - {titulo_region}"),
        subtitle = paste("Incidencia por división de gasto -", format(mes_obj_region, "%B %Y")),
        x = NULL, y = "Incidencia (p.p.)"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 12)
    )

# Inflación General por Región: Incidencias positivas por producto
top_pos_prod_region %>% 
    mutate(Descripcion = str_wrap(Descripcion, 40)) %>% 
    ggplot(aes(x = reorder(Descripcion, valor),
               y = valor,
               fill = valor)) +
    geom_col() +
    coord_flip() +
    labs(
        title    = glue("Inflación General - {titulo_region}"),
        subtitle = paste("Top 10 Incidencias Positivas por Producto -", format(mes_obj_region, "%B %Y")),
        x = NULL, y = "Incidencia (p.p.)", fill = NULL
    ) +
    scale_fill_gradientn(
        colors = c("#FAD2D2", "#F28B82", "#E53935"),
        trans = "sqrt"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 12),
        legend.position = "none"
    )

# Inflación General por Región: Incidencias negativas por producto
top_neg_prod_region %>%
    mutate(Descripcion = str_wrap(Descripcion, 40),
           intensidad = -valor) %>%
    ggplot(aes(x = reorder(Descripcion, valor, decreasing = TRUE),
               y = valor,
               fill = intensidad)) +
    geom_col() +
    coord_flip() +
    labs(
        title    = glue("Inflación General - {titulo_region}"),
        subtitle = paste("Top 10 Incidencias Negativas por Producto -", format(mes_obj_region, "%B %Y")),
        x = NULL, y = "Incidencia (p.p.)", fill = NULL
    ) +
    scale_fill_gradientn(
        colors = c("#C6E2F3", "#8FBFDA", "#5D9EC7", "#2E7FAF", "#0B5D7A"),
        trans = "sqrt"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 12),
        legend.position = "none"
    )
