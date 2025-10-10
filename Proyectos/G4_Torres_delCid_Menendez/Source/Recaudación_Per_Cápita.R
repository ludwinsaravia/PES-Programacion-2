# ==========================================================
# SAT – Recaudación (ISR, IVA-suma, TOTAL original) + Proporciones + Población
# + Recaudacion_Per_Capita = TOTAL / Poblacion
# Lee IMPUESTOS_YYYY.xls[xm] desde 'input' y guarda UN SOLO CSV en 'output'
# ==========================================================

graphics.off(); rm(list=ls()); cat("\014")

library(readxl)
library(dplyr)
library(stringr)
library(stringi)
library(readr)
library(purrr)
library(tidyr)

# --------- RUTAS / PARAMETROS ----------
input_dir   <- "C:/Users/David del Cid/Desktop/Programacion II/PROGRA2/PROYECTO/input"
output_dir  <- file.path(dirname(input_dir), "output")   # carpeta hermana de 'input'
anio_ini    <- 2021
anio_fin    <- 2024
nombre_hoja <- NULL                                      # NULL = 1a hoja; o "Hoja1"
# ---------------------------------------

# ---------- Helpers ----------
norm  <- function(x) toupper(str_squish(stringi::stri_trans_general(as.character(x), "Latin-ASCII")))
token <- function(x) gsub("[^A-Z0-9]+", " ", norm(x))   # detectar "IVA...13", "TOTAL...34", etc.

# Whitelist: 22 departamentos (normalizados)
deptos_ok <- norm(c(
    "Alta Verapaz","Baja Verapaz","Chimaltenango","Chiquimula","El Progreso",
    "Escuintla","Guatemala","Huehuetenango","Izabal","Jalapa","Jutiapa","Petén",
    "Quetzaltenango","Quiché","Retalhuleu","Sacatepéquez","San Marcos","Santa Rosa",
    "Sololá","Suchitepéquez","Totonicapán","Zacapa"
))

# Detecta la fila de encabezados (celda "DESCRIPCIÓN")
find_header <- function(path, sheet){
    top <- suppressMessages(read_excel(path, sheet = sheet, col_names = FALSE, range = cell_rows(1:25)))
    ix <- which(apply(top, 1, \(r) any(str_detect(norm(r), "DESCRIPCION"))))
    if (!length(ix)) stop("No se encontró encabezado en: ", basename(path))
    ix[1]
}

leer_un_anio <- function(path){
    if (!file.exists(path)) return(NULL)
    sheet <- if (is.null(nombre_hoja)) excel_sheets(path)[1] else nombre_hoja
    
    hdr <- find_header(path, sheet)
    df  <- suppressMessages(read_excel(path, sheet = sheet, skip = hdr - 1, col_names = TRUE))
    
    nms <- names(df); nrm <- norm(nms); tkn <- token(nms)
    
    col_desc    <- which(nrm %in% c("DESCRIPCION","DESCRIPCIÓN"))[1]
    col_isr     <- which(nrm == "ISR")[1]
    cols_iva    <- which(str_detect(tkn, "\\bIVA\\b"))
    cols_total  <- which(str_detect(tkn, "\\bTOTAL\\b"))
    
    if (is.na(col_desc))     stop("Sin columna DESCRIPCIÓN en: ", basename(path))
    if (is.na(col_isr))      stop("Sin columna ISR en: ", basename(path))
    if (!length(cols_iva))   stop("Sin columnas IVA en: ", basename(path))
    if (!length(cols_total)) stop("Sin columna TOTAL en: ", basename(path))
    
    col_total <- tail(cols_total, 1)    # TOTAL final por departamento
    
    # Subconjunto + parseo numérico
    sub <- df[, c(col_desc, col_isr, cols_iva, col_total), drop = FALSE]
    names(sub)[1:2] <- c("DEPARTAMENTO","ISR")
    names(sub)[ncol(sub)] <- "TOTAL"
    
    sub <- sub %>% mutate(DEPARTAMENTO = as.character(DEPARTAMENTO)) %>%
        filter(!is.na(DEPARTAMENTO), nzchar(str_squish(DEPARTAMENTO)))
    
    num_cols <- setdiff(names(sub), "DEPARTAMENTO")
    sub[num_cols] <- lapply(sub[num_cols], \(v) parse_number(as.character(v),
                                                             locale = locale(decimal_mark=".", grouping_mark=",")))
    
    # Sumar columnas IVA → "IVA"
    iva_names <- setdiff(names(sub), c("DEPARTAMENTO","ISR","TOTAL"))
    sub <- sub %>%
        mutate(IVA = rowSums(across(all_of(iva_names)), na.rm = TRUE)) %>%
        select(DEPARTAMENTO, ISR, IVA, TOTAL)
    
    # Proporciones (si TOTAL > 0)
    valid <- is.finite(sub$TOTAL) & sub$TOTAL > 0
    sub <- sub %>%
        mutate(
            Proporcion_ISR  = if_else(valid, ISR / TOTAL,  NA_real_),
            Proporcion_IVA  = if_else(valid, IVA / TOTAL,  NA_real_),
            Proporcion_Otros_Impuestos = if_else(valid, 1 - Proporcion_ISR - Proporcion_IVA, NA_real_)
        ) %>%
        mutate(
            Proporcion_ISR  = round(pmin(pmax(Proporcion_ISR, 0), 1), 6),
            Proporcion_IVA  = round(pmin(pmax(Proporcion_IVA, 0), 1), 6),
            Proporcion_Otros_Impuestos = round(pmin(pmax(Proporcion_Otros_Impuestos, 0), 1), 6)
        )
    
    # Filtro ESTRICTO de departamentos + año
    anio <- parse_number(basename(path))
    sub %>%
        mutate(DEP_NORM = norm(DEPARTAMENTO)) %>%
        filter(DEP_NORM %in% deptos_ok) %>%
        select(-DEP_NORM) %>%
        mutate(Anio = anio) %>%
        relocate(DEPARTAMENTO, Anio)
}

# ---------- 1) Leer IMPUESTOS_YYYY desde 'input' ----------
archs_all <- list.files(input_dir, pattern = "^IMPUESTOS_\\d{4}\\.xls[xm]$", full.names = TRUE)
if (!length(archs_all)) stop("No se encontraron archivos IMPUESTOS_YYYY.xls[xm] en: ", input_dir)

yrs <- readr::parse_number(basename(archs_all))
archs <- archs_all[yrs >= anio_ini & yrs <= anio_fin]
if (!length(archs)) stop("No hay archivos en el rango ", anio_ini, "-", anio_fin, ".")

recaud <- purrr::map_dfr(archs, leer_un_anio) %>%
    filter(norm(DEPARTAMENTO) %in% deptos_ok, !is.na(Anio)) %>%
    distinct(DEPARTAMENTO, Anio, .keep_all = TRUE) %>%
    arrange(Anio, DEPARTAMENTO)

# ---------- 2) Añadir población desde output/Base_Poblacion_Limpia.xlsx ----------
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
pop_path <- file.path(output_dir, "Base_Poblacion_Limpia.xlsx")
if (!file.exists(pop_path)) stop("No se encontró 'Base_Poblacion_Limpia.xlsx' en: ", normalizePath(output_dir))

pop_raw <- read_excel(pop_path, sheet = 1, col_names = TRUE)

col_depto <- names(pop_raw)[grepl("^dep|depart", names(pop_raw), ignore.case = TRUE)][1]
if (is.na(col_depto)) col_depto <- "Departamento"
cols_anio <- names(pop_raw)[grepl("^\\d{4}$", names(pop_raw))]
if (!length(cols_anio)) stop("No se detectaron columnas de años (2020, 2021, ...).")

anios_disponibles <- sort(unique(recaud$Anio))

pop_long <- pop_raw %>%
    rename(DEPARTAMENTO = all_of(col_depto)) %>%
    pivot_longer(cols = all_of(cols_anio), names_to = "Anio", values_to = "Poblacion") %>%
    mutate(
        Anio = as.integer(Anio),
        DEPARTAMENTO = as.character(DEPARTAMENTO),
        Poblacion = parse_number(as.character(Poblacion),
                                 locale = locale(decimal_mark = ".", grouping_mark = ","))
    ) %>%
    filter(Anio %in% anios_disponibles,
           !is.na(DEPARTAMENTO), nzchar(str_squish(DEPARTAMENTO))) %>%
    mutate(DEP_NORM = norm(DEPARTAMENTO)) %>%
    filter(DEP_NORM %in% deptos_ok) %>%
    select(DEP_NORM, Anio, Poblacion)

final <- recaud %>%
    mutate(DEP_NORM = norm(DEPARTAMENTO)) %>%
    left_join(pop_long, by = c("DEP_NORM", "Anio")) %>%
    select(-DEP_NORM) %>%
    relocate(Poblacion, .after = dplyr::last_col()) %>%
    # NUEVA COLUMNA: Recaudación per cápita = TOTAL / Poblacion (si Poblacion > 0)
    mutate(
        Recaudacion_Per_Capita_TOTAL = if_else(
            is.finite(Poblacion) & Poblacion > 0,
            TOTAL*1000000 / Poblacion,
            NA_real_
        ),
        # *** INSERCIÓN SOLICITADA: IVA / TOTAL ***
        Recaudacion_Per_Capita_IVA = if_else(
            is.finite(Poblacion) & Poblacion > 0,
            IVA*1000000 / Poblacion,
            NA_real_
        )
    ) %>%
    relocate(Recaudacion_Per_Capita_TOTAL, .after = dplyr::last_col()) %>%
    relocate(Recaudacion_Per_Capita_IVA, .after = dplyr::last_col()) %>% # Mover la nueva columna al final
    filter(!is.na(DEPARTAMENTO), nzchar(str_squish(DEPARTAMENTO))) %>%
    arrange(Anio, DEPARTAMENTO)

# ---------- Guardar UN SOLO CSV ----------
outfile <- file.path(output_dir, "recaudacion_per_capita_departamento.csv")
write_csv(final, outfile, na = "")

