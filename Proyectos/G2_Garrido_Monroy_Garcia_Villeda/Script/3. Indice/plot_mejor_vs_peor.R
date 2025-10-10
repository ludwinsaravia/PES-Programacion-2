rm(list = ls())
getwd()
cat("\014")
library(tidyverse)
library(readxl)
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

ponderadores <- c(0.18, 0.10, 0.1, 0.12, 0.15, 0.1, 0.1, 0.05, 0.1)
sum(ponderadores)

df_idx <- df_idx |>
    mutate(
        across(2:10, ~ .x * ponderadores[cur_column() == names(df_idx)[-1]])
    )


library(tidyverse)
library(scales)

# --- 1) Elegir mejor y peor por suma total ---
cols_vars <- setdiff(names(df_idx), "depto")

df_tot <- df_idx |>
    mutate(total = rowSums(across(all_of(cols_vars)), na.rm = TRUE))

best  <- df_tot |> slice_max(total, n = 1, with_ties = FALSE)
worst <- df_tot |> slice_min(total, n = 1, with_ties = FALSE)

# --- 2) Data para comparar: una barra por variable (Mejor vs Peor) ---
df_comp <- df_idx |>
    filter(depto %in% c(best$depto, worst$depto)) |>
    pivot_longer(all_of(cols_vars), names_to = "variable", values_to = "valor") |>
    mutate(
        grupo = if_else(depto == best$depto, "Mejor", "Peor")
    ) |>
    select(grupo, variable, valor)

# (Opcional) ordenar variables por diferencia (Mejor - Peor), de mayor a menor
ord_vars <- df_comp |>
    pivot_wider(names_from = grupo, values_from = valor) |>
    mutate(diff = Mejor - Peor) |>
    arrange(desc(diff)) |>
    pull(variable)

df_comp <- df_comp |>
    mutate(variable = factor(variable, levels = ord_vars))

# (Opcional) etiquetas más legibles
labels_vars <- c(
    var_uso_intern               = "Uso internet (total)",
    var_uso_intern_mujeres       = "Uso internet (mujeres)",
    var_uso_acceso_tel_mov       = "Acceso tel. móvil",
    pct_rural                    = "% Urbanización",
    propor_hogares_internet      = "Hogares con internet",
    propor_hogares_computadora   = "Hogares con computadora",
    rb_por_100k                  = "Redes/antenas por 100k",
    lineas_por_1k                = "Líneas por 1k",
    pib_per_capita               = "PIB per cápita (norm.)"
)
df_comp <- df_comp |> mutate(variable = recode(variable, !!!labels_vars))

list_colores <- c(
    "#b3aea1",
    "#cedde4",
    "#599dbc",
    "#d5b491",
    "#90776e",
    "#654848",
    "#7fa5b1",
    "#1c556c",
    "#0e2432"
)

# --- 3) Plot: barras lado a lado (dodge) por indicador ---
col_mejor <- "#1c556c"  # verde muted
col_peor  <- "#d5b491"  # rojo muted

p_cmp <- ggplot(df_comp, aes(x = variable, y = valor, fill = grupo)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.7, alpha = 0.95) +
    # etiqueta numérica arriba de cada barra
    geom_text(aes(label = scales::label_number(accuracy = 0.01)(valor)),
              position = position_dodge(width = 0.75),
              vjust = -0.5, size = 3.4) +
    scale_fill_manual(values = c(Mejor = col_mejor, Peor = col_peor), name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(
        title = paste0("Comparación por indicador: Mejor (", best$depto, ") vs Peor (", worst$depto, ")"),
        x = NULL, y = "valor de indice"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "top",
        plot.margin = margin(5, 10, 18, 10)
    )

# x11();p_cmp
# --- 4) Guardar en buena resolución (opcional) ---
ggsave("output/mejor_vs_peor.jpeg", p_cmp, width = 8, height = 6, units = "in", dpi = 300)
