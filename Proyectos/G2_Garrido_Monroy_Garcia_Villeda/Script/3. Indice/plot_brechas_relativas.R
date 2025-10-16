library(tidyverse)
library(scales)

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
cols_vars <- setdiff(names(df_idx), "depto")

df_tot <- df_idx |>
    mutate(total = rowSums(across(all_of(cols_vars)), na.rm = TRUE))

best  <- df_tot |> slice_max(total, n = 1, with_ties = FALSE) |> pull(depto)
worst <- df_tot |> slice_min(total, n = 1, with_ties = FALSE) |> pull(depto)

# (Opcional) etiquetas bonitas para las variables
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

# -------- 2) Construir tabla Mejor vs Peor por indicador --------
tabla_brechas <- df_idx |>
    filter(depto %in% c(best, worst)) |>
    pivot_longer(all_of(cols_vars), names_to = "indicador", values_to = "valor") |>
    mutate(grupo = if_else(depto == best, "Mejor", "Peor")) |>
    select(indicador, grupo, valor) |>
    pivot_wider(names_from = grupo, values_from = valor) |>
    mutate(
        indicador = recode(indicador, !!!labels_vars),
        brecha_abs = Mejor - Peor,
        brecha_rel = if_else(Mejor > 0, brecha_abs / Mejor, NA_real_)   # respecto al Mejor
    ) |>
    arrange(desc(brecha_rel)) |>
    # Formatos amigables
    mutate(
        Mejor      = number(Mejor, accuracy = 0.01),
        Peor       = number(Peor,  accuracy = 0.01),
        `Brecha abs.` = number(brecha_abs, accuracy = 0.01),
        `Brecha rel.` = percent(brecha_rel, accuracy = 0.1)
    ) |>
    select(Indicador = indicador, Mejor, Peor, `Brecha abs.`, `Brecha rel.`)

tabla_brechas

library(gt)
x11();tabla_brechas |>
    gt() |>
    tab_header(title = md("**Brecha relativa: Mejor vs Peor**")) |>
    cols_align(everything(), align = "center")

