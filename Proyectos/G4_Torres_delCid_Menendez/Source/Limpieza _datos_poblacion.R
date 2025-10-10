# ===============================================================
# Limpieza simple y robusta (3 hojas): Total / Hombres / Mujeres
# - Mantiene: Cod, Departamento, 2022, 2023, 2024
# - Detecta años por NOMBRE y por CONTENIDO en primeras filas del original
# - Filtra Cod ∈ {1..22}
# - Escribe un solo Excel con 3 pestañas
# ===============================================================

suppressPackageStartupMessages(library(openxlsx))

# ---- RUTAS ----
entrada <- "C:/Users/David del Cid/Desktop/Programacion II/PROGRA2/PROYECTO/input/Estimaciones_Proyecciones_Poblacion.xlsx"
salida  <- "C:/Users/David del Cid/Desktop/Programacion II/PROGRA2/PROYECTO/output/Base_Poblacion_Limpia.xlsx"

# ---- Parámetros ----
col_cod <- 1
col_dep <- 2
anios   <- c("2020","2021","2022","2023","2024")

tiene_anio <- function(x, y) any(grepl(paste0("\\b", y, "\\b"), trimws(as.character(x))))

limpiar <- function(archivo, hoja, col_cod = 1, col_dep = 2, anios = c("2020","2021","2022","2023","2024")) {
    # 1) Leer original sin encabezados
    df0 <- read.xlsx(archivo, sheet = hoja, colNames = FALSE)
    stopifnot(nrow(df0) > 0)
    
    # 2) Primera fila de datos: primer Cod entero 1..22
    Cod0 <- suppressWarnings(as.numeric(trimws(as.character(df0[[col_cod]]))))
    i_first <- which(!is.na(Cod0) & Cod0 %% 1 == 0 & Cod0 >= 1 & Cod0 <= 22)[1]
    if (is.na(i_first)) i_first <- 7  # respaldo
    
    # 3) Encabezado REAL: la fila anterior a i_first (si existe)
    if (i_first > 1) {
        header <- trimws(as.character(df0[i_first - 1, ]))
    } else {
        header <- paste0("Col", seq_len(ncol(df0)))
    }
    header[header == "" | is.na(header)] <- paste0("Col", which(header == "" | is.na(header)))
    
    # 4) Datos: desde i_first (no perdemos Guatemala)
    df <- df0[i_first:nrow(df0), , drop = FALSE]
    names(df) <- header
    
    # 5) Detectar columnas de años EN EL ORIGINAL (primeras filas)
    top_n <- min(max(15, i_first + 5), nrow(df0))
    idx_years <- setNames(rep(NA_integer_, length(anios)), anios)
    for (y in anios) {
        hit <- which(vapply(df0[1:top_n, , drop = FALSE], tiene_anio, logical(1), y = y))
        if (length(hit)) idx_years[y] <- hit[1]
    }
    
    # 6) Renombrar por índice las columnas de años detectadas
    for (y in anios) {
        j <- idx_years[[y]]
        if (!is.na(j) && j <= ncol(df)) names(df)[j] <- y
    }
    
    # 7) Asegurar Cod/Departamento (por nombre o índice)
    if (!"Cod" %in% names(df) && col_cod <= ncol(df))  names(df)[col_cod] <- "Cod"
    if (!"Departamento" %in% names(df) && col_dep <= ncol(df)) names(df)[col_dep] <- "Departamento"
    
    # 8) Subconjunto en orden: Cod, Departamento, 2022, 2023, 2024
    keep <- c("Cod", "Departamento")
    for (y in anios) {
        if (y %in% names(df)) {
            keep <- c(keep, y)
        } else if (!is.na(idx_years[[y]]) && idx_years[[y]] <= ncol(df)) {
            # incluir por índice y renombrar
            keep <- c(keep, names(df)[idx_years[[y]]])
            names(df)[idx_years[[y]]] <- y
        }
    }
    keep <- unique(keep[keep %in% names(df)])
    df <- df[, keep, drop = FALSE]
    
    # 9) Filtrar Cod ∈ {1..22}
    Cod <- suppressWarnings(as.numeric(trimws(as.character(df$Cod))))
    ok  <- !is.na(Cod) & Cod %% 1 == 0 & Cod >= 1 & Cod <= 22
    df  <- df[ok, , drop = FALSE]
    
    # 10) Convertir años a numérico
    for (y in anios) if (y %in% names(df))
        df[[y]] <- suppressWarnings(as.numeric(gsub(",", "", trimws(as.character(df[[y]])))))
    
    df
}

# ---- Procesar 3 hojas y escribir un solo archivo ----
hojas_in  <- c("Población - Total", "Población - Hombres", "Población - Mujeres")
hojas_out <- c("Total", "Hombres", "Mujeres")

wb <- createWorkbook()
for (i in seq_along(hojas_in)) {
    dat <- limpiar(entrada, hojas_in[i], col_cod, col_dep, anios)
    addWorksheet(wb, hojas_out[i])
    writeData(wb, hojas_out[i], dat, colNames = TRUE)
}
saveWorkbook(wb, salida, overwrite = TRUE)
cat("Listo. Escribí: ", salida, "\n")
