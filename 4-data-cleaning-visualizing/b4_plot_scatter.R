# Limpieza y visualización de datos
# Caso practico: Analizando los componentes del IPC
# Análisis 4: Scatterplot de inflaciones anuales entre dos años
#
# Ángelo Gutiérrez Daza
# 2025
#
# Programación II
# Programa de Estudios Superiores
# Banco de Guatemala
#
# Código probado utilizando la versión 4.5.1 de R

# ------------------------------------------------------------------------------
# 0) Inicialización
# ------------------------------------------------------------------------------

# Librerías usadas
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(stringr)
library(readxl)
library(magrittr)

# Limpiar entorno de trabajo
graphics.off()
rm(list = ls())
cat("\014")

# ------------------------------------------------------------------------------
# 1) Carga de datos
# ------------------------------------------------------------------------------

# Comenzemos por cargar el CSV con la muestra seleccionada
ipc_data <- read_csv("./output/selected_data.csv")


# ------------------------------------------------------------------------------
# 2) Construir nuevamente los indices de inflación anual
# ------------------------------------------------------------------------------

# Para cada item y fecha, vamos a calcular la inflación anual
ipc_data %<>%
    group_by(id_item) %>%
    arrange(t_date) %>%
    mutate(
        ipc_lag12 = lag(ipc, n = 12, order_by = t_date),
        inf12m = (ipc / ipc_lag12 - 1) * 100
    ) %>%
    ungroup()

# El comando lag, parte de la librería dplyr, permite obtener el valor de una
# variable en un periodo anterior. En este caso, estamos obteniendo el valor del
# IPC 12 meses antes (n = 12), ordenando los datos por fecha (order_by = t_date)
# dentro de cada grupo (id_item)

# Como hemos sacado el valor de 12 meses antes, los primeros 12 meses de cada item
# tendrán un valor NA en la variable inflacion_anual. Podemos eliminar esos valores
ipc_data %<>% filter(!is.na(inf12m))


# ------------------------------------------------------------------------------
# 4) Graficar la inflacion obervada de items en uno o dos periodos usando scatters
# ------------------------------------------------------------------------------

# Enfoquémonos en la inflación de los items en diciembre de cada año
inf12m_plot <- ipc_data %>%
    filter(id_nivel == "Item", month(t_date) == 12) %>%
    mutate(t_year = year(t_date)) %>%
    select(t_year, id_item, inf12m)

# Seleccionamos dos años y llevamos la tabla a formato ancho
inf12m_plot <- inf12m_plot %>%
    filter(t_year %in% c(2014, 2015)) %>%
    pivot_wider(names_from = t_year, values_from = inf12m, names_prefix = "inf12m_")

# a) Gráfico rápido ------------------------------------------------------------

# Comenzamos por graficar la evolución de la inflación anual del IPC general

plot_simple <- inf12m_plot %>%
    ggplot() +
    geom_point(aes(x = inf12m_2014, y = inf12m_2015), color = "blue", size = 3)
plot_simple

ggsave(file = "./output/scatter_simple.pdf", plot = plot_simple)


# b) Gráfico normal ------------------------------------------------------------

plot_normal <- inf12m_plot %>%
    ggplot() +
    geom_point(aes(x = inf12m_2014, y = inf12m_2015),
        color = "darkblue", size = 3, stroke = 1, shape = 21,
        fill = "cadetblue", , alpha = 0.8
    ) +
    labs(
        title = "Inflación anual del por item: 2014 vs 2015",
        x = "Inflación Anual por Item 2014 (%)",
        y = "Inflación Anual por Item 2015 (%)",
        caption = "Fuente: Ángelo, con Datos del INE"
    ) +
    xlim(-50, 105) +
    ylim(-50, 105) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12)
    )

plot_normal

ggsave(file = "./output/scatter_normal.pdf", plot = plot_normal)


# c) Gráfico más elaborado -----------------------------------------------------

plot_fancy <- inf12m_plot %>%
    ggplot(aes(x = inf12m_2014, y = inf12m_2015)) +
    geom_point(
        color = "darkblue", size = 3, stroke = 1, shape = 21,
        fill = "cadetblue", , alpha = 0.8
    ) +
    labs(
        title = "Inflación anual del por item: 2014 vs 2015",
        x = "Inflación Anual por Item 2014 (%)",
        y = "Inflación Anual por Item 2015 (%)",
        caption = "Fuente: Ángelo, con Datos del INE"
    ) +
    xlim(-2.5, 5) +
    ylim(-2.5, 5) +
    geom_smooth(method = loess, color = "darkorange", fill = "lightsalmon", se = TRUE) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12)
    )

plot_fancy

ggsave(file = "./output/scatter_fancy.pdf", plot = plot_fancy)
