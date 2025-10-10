library(tidyverse)
library(scales)
library(gt)

rm(list=ls())
cat("\014")
graphics.off()


df_idx <- read_xlsx("output/idx.xlsx")

df_idx <- df_idx |>
    mutate( depto = recode(
        depto,
        `1`  = "Guatemala",
        `2`  = "El Progreso",
        `3`  = "Sacatepéquez",
        `4`  = "Chimaltenango",
        `5`  = "Escuintla",
        `6`  = "Santa Rosa",
        `7`  = "Sololá",
        `8`  = "Totonicapán",
        `9`  = "Quetzaltenango",
        `10` = "Suchitepéquez",
        `11` = "Retalhuleu",
        `12` = "San Marcos",
        `13` = "Huehuetenango",
        `14` = "Quiché",
        `15` = "Baja Verapaz",
        `16` = "Alta Verapaz",
        `17` = "Petén",
        `18` = "Izabal",
        `19` = "Zacapa",
        `20` = "Chiquimula",
        `21` = "Jalapa",
        `22` = "Jutiapa"
    )
)


# -------- 1) Identificar Mejor y Peor por suma total --------


ponderadores <- c(0.18,0.10,0.1,0.12,0.15,0.10,0.10,0.05,0.10)
df_idx <- df_idx |>
    mutate(
        across(2:10, ~ .x * ponderadores[cur_column() == names(df_idx)[-1]])
    )


cols_vars <- setdiff(names(df_idx), "depto")

df_tot <- df_idx |>
    mutate(total = rowSums(across(all_of(cols_vars)), na.rm = TRUE))


best  <- df_tot |> slice_max(total, n = 1, with_ties = FALSE) |> pull(depto)
worst <- df_tot |> slice_min(total, n = 1, with_ties = FALSE) |> pull(depto)

# Etiquetas de variables
labels_vars <- c(
    var_uso_intern               = "Uso internet (total)",
    var_uso_intern_mujeres       = "Uso internet (mujeres)",
    var_uso_acceso_tel_mov       = "Acceso tel. móvil",
    pct_rural                    = "% urbanización (o 1-% rural)",
    propor_hogares_internet      = "Hogares con internet",
    propor_hogares_computadora   = "Hogares con computadora",
    rb_por_100k                  = "Redes/antenas por 100k",
    lineas_por_1k                = "Líneas por 1k",
    pib_per_capita               = "PIB per cápita (norm.)"
)

df_best_worst <- df_idx |>
    filter(depto %in% c(best, worst)) 

tab <- df_best_worst %>%
    pivot_longer(-depto, names_to = "indicador", values_to = "valor") %>%
    mutate(indicador = recode(indicador, !!!labels_vars)) %>%
    group_by(indicador) %>%
    summarise(
        Mejor        = max(valor, na.rm = TRUE),
        Peor         = min(valor, na.rm = TRUE),
        `Brecha abs.`= abs(Mejor - Peor),
        .groups = "drop"
    ) %>%
    arrange(desc(`Brecha abs.`))

gt(tab) %>%
    tab_header(title = md("**Brecha relativa: Mejor vs Peor**")) %>%
    cols_label(indicador = "Indicador") %>%
    cols_align(columns = indicador, align = "left") %>%
    cols_align(columns = c(Mejor, Peor, `Brecha abs.`), align = "center") %>%
    fmt_percent(columns = c(Mejor, Peor, `Brecha abs.`),
                decimals = 1, scale_values = FALSE) %>%
    tab_options(table.width = pct(100))
