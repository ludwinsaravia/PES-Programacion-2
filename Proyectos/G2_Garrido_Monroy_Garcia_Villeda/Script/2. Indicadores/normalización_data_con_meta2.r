library(tidyverse)

# --------------------------------------------------------------------------
# 1) se definen las metas 
# --------------------------------------------------------------------------
tb_metas <- tribble::tribble(
    ~df_indicadores                 ~better,    ~floor_F,   ~target_T,
    "var_uso_cel",                  "higher",   0,          95,
    "var_uso_intern",               "higher",   0,          95,
    "var_uso_intern_mujeres",       "higher",   0,          95,
    "var_uso_acceso_tel_mov",       "higher",   0,          95,
    "pct_rural",                    "higher",   0,          95,
    "propor_hogares_internet",      "higher",   0,          95,
    "propor_hogares_computadora",   "higher",   0,          95,
    "rb_por_100k",                  "higher",   0,          80,
    "lineas_por_1k",                "higher",   0,          800,
    "pib_percapita",                "higher",   0,          1
)


# --------------------------------------------------------------------------
# 2) método de normalización min - max 
# --------------------------------------------------------------------------

calc_indice_minmax <- function(x, better, min, max) {
    
    indice <- 0
    
    if (min == max) return(NA)
    
    if (better == "higher") {
        indice <- (x - min) / (max - min) * 100
    } else {
        indices <- (max - x) / (max - min) * 100
    }
    
    
}

