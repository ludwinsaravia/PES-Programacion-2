rm(list = ls())
getwd()
cat("\014")
library(tidyverse)
library(readxl)
library(scales)

df_idx <- read_xlsx("output/idx.xlsx")
df_idtr <- read_xlsx("output/idtr.xlsx")


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

library(scales)

# --- Paleta "muted" inspirada en seaborn (9 variables) ---
sns_muted <- c(
    "#4C72B0", "#55A868", "#C44E52", "#8172B3", "#CCB974",
    "#64B5CD", "#937860", "#DA8BC3", "#8C8C8C"
)

list_colores <- c(
    "#f66d9b", "#9561e2", "#6574cd", "#3490dc", "#4dc0b5",
    "#38c172", "#ffed4a", "#f6993f", "#e3342f"
)

list_colores <- c(
    "#ccdbd6",
    "#a8af98",
    "#43ae9e",
    "#e7b24f",
    "#cd6e1b",
    "#e6d8cf",
    "#d7c5ad",
    "#a8afbb",
    "#7a8b9f"
)

list_colores <- c(
    "#e4e9ed",
    "#cedde4",
    "#599dbc",
    "#d5b491",
    "#90776e",
    "#654848",
    "#7fa5b1",
    "#1c556c",
    "#0e2432"
)





# ------------------------------
# 1) Pasar df_idx a formato largo
# ------------------------------
df_long <- df_idx |>
    pivot_longer(
        -depto,
        names_to  = "variable",
        values_to = "valor"
    )

# (Opcional) Etiquetas más legibles para la leyenda
labels_vars <- c(
    var_uso_intern               = "Uso internet (total)",
    var_uso_intern_mujeres       = "Uso internet (mujeres)",
    var_uso_acceso_tel_mov       = "Acceso tel. móvil",
    pct_rural                    = "% rural",
    propor_hogares_internet      = "Hogares con internet",
    propor_hogares_computadora   = "Hogares con computadora",
    rb_por_100k                  = "Redes/antenas por 100k habit.",
    lineas_por_1k                = "Líneas por cada 1k habit.",
    pib_per_capita               = "PIB per cápita (norm.)"
)

df_long <- df_long |>
    mutate(variable = recode(variable, !!!labels_vars))

# ------------------------------
# 2) Ordenar departamentos por total (suma de indicadores)
# ------------------------------
df_plot <- df_long |>
    group_by(depto) |>
    mutate(total_depto = sum(valor, na.rm = TRUE)) |>
    ungroup() |>
    mutate(depto = reorder(depto, total_depto))

# ------------------------------
# 3) Graficar (elige modo: "stack" o "fill")
# ------------------------------
modo <- "stack"   # "fill" = composición 100%; usa "stack" si quieres valores crudos

p <- ggplot(df_plot, aes(x = depto, y = valor, fill = variable)) +
    geom_col(position = modo, width = 0.82, alpha = 0.85) +
    scale_fill_manual(values = list_colores, name = "Indicador") +
    scale_y_continuous(
        labels = if (modo == "fill") percent_format(accuracy = 1) else label_number(accuracy = 0.01),
        expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
        title    = "Composición de indicadores por departamento",
        subtitle = if (modo == "fill") "Barras normalizadas al 100%" else "Barras apiladas (valores)",
        x = NULL,
        y = if (modo == "fill") "Porcentaje" else "Valor",
        caption = "Fuente: elaboración propia"
    ) +
    theme_seaborn()

x11();p
# ggsave("barras_seaborn_df_idx.png", p, width = 12, height = 7, dpi = 300)