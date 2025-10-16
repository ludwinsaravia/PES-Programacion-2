
getwd()



rm(list=ls())
setwd("C:/Users/Javie/OneDrive/Escritorio/PROGRAMACIÓN II/programa-II/Proyecto")

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readxl)
library(tibble)
library(writexl)
    
# 1) Tabla de metas/pisos y dirección
# params <- tibble::tribble(
#     ~variable,                 ~better,    ~floor_F,   ~target_T,
#     "var_uso_intern",               "higher",   13.8,        93,
#     "var_uso_intern_mujeres",       "higher",   4.87,        91,
#     "var_uso_acceso_tel_mov",       "higher",   30.85,       94.39,
#     "pct_rural",                    "lower",    83.54,        15,
#     "propor_hogares_internet",      "higher",   2.79,        90.32,
#     "propor_hogares_computadora",   "higher",   3.44,        80.8,
#     "rb_por_100k",                  "higher",   36,          136.16,
#     "lineas_por_1k",                "higher",   8.4,         404.9,
#     "pib_per_capita",                "higher",   1168.69,     14654.9
# )

#America Latina y el Caribe
params <- tibble::tribble(
    ~variable,                 ~better,    ~floor_F,   ~target_T,
    "var_uso_intern",               "higher",   13.8,        77,
    "var_uso_intern_mujeres",       "higher",   4.87,        72,
    "var_uso_acceso_tel_mov",       "higher",   30.85,       83.4,
    "pct_rural",                    "lower",    83.54,        18.2,
    "propor_hogares_internet",      "higher",   2.79,        66.2,
    "propor_hogares_computadora",   "higher",   3.44,        42.91,
    "rb_por_100k",                  "higher",   36,          200,
    "lineas_por_1k",                "higher",   8.4,         404.9,
    "pib_per_capita",                "higher",   1168.69,     14654.9
)

getwd()

df <- read_excel("output/indicadores_acceso_tecnologico.xlsx")

# 2) Función distancia-a-meta (0–100 con recorte)
score_target <- function(x, better, F, T) {
    s <- ifelse(
        better == "higher",
        (x - F) / (T - F) * 100,
        (F - x) / (F - T) * 100
    )
    pmin(pmax(s, 0), 100)  # recorta a [0,100]
}

# 3) Calcular subíndices 0–100 por variable
# df: data.frame con depto y las columnas de indicadores en %
idt_long <- df %>%
    tidyr::pivot_longer(-depto, names_to = "variable", values_to = "valor") %>%
    dplyr::left_join(params, by = "variable") %>%
    dplyr::mutate(ind_var = score_target(valor, better, floor_F, target_T)) %>%
    dplyr::select(depto, variable, ind_var)


# 4) Volver a ancho y agregar índice compuesto
idt <- idt_long %>%
    pivot_wider(names_from = variable, values_from = ind_var)

# (a) Media aritmética simple
# media aritmética (todas las columnas excepto 'depto')
# idt <- idt_wide %>%
#     mutate(
#         IDT_media = rowMeans(pick(-depto), na.rm = TRUE)
#     )
# 
# # media geométrica (log sobre cada columna, luego promedio y exponenciar)
# eps <- 1e-6
# idt <- idt %>%
#     mutate(
#         IDT_geo = exp(rowMeans(across(-depto, ~ log((.x + eps) / 100)),
#                                na.rm = TRUE)) * 100
#   )

write_xlsx(idt,
    path = "output/IDX.xlsx",format_headers = TRUE
)

