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



# 1) Suma por departamento (cima de cada barra)
tops <- df_idx |>
    mutate(total_depto = rowSums(across(-depto), na.rm = TRUE)) |>
    select(depto, total_depto)

# 2) Formato largo para las barras + mismo orden por total
df_long <- df_idx |>
    pivot_longer(-depto, names_to = "variable", values_to = "valor") |>
    left_join(tops, by = "depto") |>
    mutate(depto = reorder(depto, total_depto))   # ordena por total

# 3) Datos de la línea (cimas) con el mismo orden de factor
df_tops <- tops |>
    mutate(depto = factor(depto, levels = levels(df_long$depto))) |>
    arrange(depto)


labels_vars <- c(
    var_uso_intern               = "Uso internet (total)",
    var_uso_intern_mujeres       = "Uso internet (mujeres)",
    var_uso_acceso_tel_mov       = "Acceso tel. móvil",
    pct_rural                    = "% Urbanización",
    propor_hogares_internet      = "Hogares con internet",
    propor_hogares_computadora   = "Hogares con computadora",
    rb_por_100k                  = "Redes/antenas por 100k habit.",
    lineas_por_1k                = "Líneas por cada 1k habit.",
    pib_per_capita               = "PIB per cápita (norm.)"
)

df_long <- df_long |>
    mutate(variable = recode(variable, !!!labels_vars))


# 4) Gráfico: barras apiladas (valores), línea y puntos en la cima + etiqueta
plot_incidencia <- ggplot(df_long, aes(x = depto, y = valor, fill = variable)) +
    geom_col(position = "stack", width = 0.82, alpha = 0.9) +
    # línea que conecta las cimas
    geom_line(data = df_tops,
              aes(x = depto, y = total_depto, group = 1),
              inherit.aes = FALSE, linewidth = 1.3, color = "#0e2432") +
    # punto en la cima
    geom_point(data = df_tops,
               aes(x = depto, y = total_depto),
               inherit.aes = FALSE, size = 2.8, color = "white",
               fill = "#0e2432", shape = 21, stroke = 1.1) +
    # número encima de la cima
    geom_text(data = df_tops,
              aes(x = depto, y = total_depto, label = round(total_depto, 2)),
              inherit.aes = FALSE, vjust = -0.6, size = 3.4) +
    scale_fill_manual(values = list_colores, name = "Indicador") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +  # espacio para la etiqueta
    labs(
        title = "Composición de indicadores por departamento",
        subtitle = "Barras normalizadas al 100%",
        x = NULL, y = "IDTR"
    )+ 
    theme_minimal()+
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)  # <- aquí
    ) 
#x11();plot_incidencia
ggsave("output/plot_incidencia_por_depto.JPEG", plot_incidencia, width = 16, height = 9)

