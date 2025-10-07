# Limpieza y visualización de datos
# Caso practico: Analizando los componentes del IPC
# Análisis 1: Evolución de la inflación anual agregada vs la mediana
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
# 2) Construir indices de inflación anual
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
# 3) Construir tabla de inflaciones anuales por item
# ------------------------------------------------------------------------------

# Ahora, vamos a construir una tabla que tenga la inflación anual de cada item
# en cada fecha. Para hacerlo, utilizaremos la función pivot_wider() del paquete
# tidyr. Ver: https://tidyr.tidyverse.org/reference/pivot_wider.html

# Queremos una tabla donde cada fila sea una fecha, y cada columna un item
# El valor en cada celda será la inflación anual de ese item en esa fecha
# Para hacerlo, utilizamos la función pivot_wider(), indicando que las columnas
# se deben crear a partir de los valores de la variable id_item (names_from)
# y que los valores en las celdas deben ser los de la variable inflacion_anual

inf12m_tab <- ipc_data %>%
    select(t_date, id_item, inf12m) %>%
    pivot_wider(names_from = id_item, values_from = inf12m)

# Creemos una tabla con los nombres de las columnas
grupo_list <- ipc_data %>%
    filter(year(t_date) == 2023) %>%
    select(id_grupo = id_item, descr_grupo = descr) %>%
    distinct()

# Podemos también calcular la inflación mediana, el Q1 y el Q3
inf12m_tab %<>%
    rowwise() %>%
    mutate(
        inf12m_med = median(c_across(-t_date), na.rm = TRUE),
        inf12m_q1 = quantile(c_across(-t_date), probs = 0.25, na.rm = TRUE),
        inf12m_q3 = quantile(c_across(-t_date), probs = 0.75, na.rm = TRUE)
    )

# ------------------------------------------------------------------------------
# 4) Graficar la evolución a través del tiempo usando ggplot2
# ------------------------------------------------------------------------------

# Seleccionemos los datos a graficar
inf12m_plot <- inf12m_tab %>%
    select(t_date, inf12m = `0`, inf12m_med, inf12m_q1, inf12m_q3) %>%
    filter(year(t_date) >= 2013, year(t_date) <= 2023)


# a) Gráfico rápido ------------------------------------------------------------

# Comenzamos por graficar la evolución de la inflación anual del IPC general

# Utilizaremos la función ggplot() del paquete ggplot2
# Ver: https://ggplot2.tidyverse.org/reference/ggplot.html

plot_simple <- inf12m_plot %>%
    ggplot() +
    geom_line(aes(x = t_date, y = inf12m), linewidth = 1.5, linetype = "solid")

plot_simple

ggsave(file = "./output/time_series_simple.pdf", plot = plot_simple)


# b) Gráfico normal ------------------------------------------------------------

# Ahora utilicemos la misma librería para graficar, al mismo tiempo,
# la inflación anual del IPC general y la mediana de los items y añadir
# algunas mejoras estéticas

plot_normal <- inf12m_plot %>%
    ggplot() +
    geom_line(aes(x = t_date, y = inf12m), 
              color = "red", linewidth = 1.5, linetype = "solid") +
    geom_line(aes(x = t_date, y = inf12m_med), 
              color = "darkblue", linewidth = 1.2, linetype = "dashed")

plot_normal

ggsave(file = "./output/time_series_normal.pdf", plot = plot_normal)


# c) Gráfico más elaborado -----------------------------------------------------

# Podemos utilizar las funciones de ggplot2 para hacer un gráfico más elaborado
# con títulos, etiquetas, leyendas, etc.

plot_fancy <- inf12m_plot %>%
    ggplot() +
    geom_ribbon(aes(x = t_date, ymin = inf12m_q1, ymax = inf12m_q3),
                fill = "#5B92AB", alpha = 0.5
    ) +
    geom_line(aes(x = t_date, y = inf12m_med),
              color = "#0C2746", linewidth = 1.5, linetype = "solid"
    ) +
    geom_line(aes(x = t_date, y = inf12m),
              color = "#8B0000", linewidth = 1.5, linetype = "solid"
    ) +
    labs(
        title    = "Evolución de la inflación anual del IPC general vs la mediana de los items",
        subtitle = "Con banda intercuartílica (IQR) de los items",
        x        = "Fecha",
        y        = "Inflación Anual (%)",
        caption  = "Fuente: Ángelo, con Datos del INE"
    ) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    theme_minimal() +
    theme(
        plot.title       = element_text(size = 16, face = "bold"),
        plot.subtitle    = element_text(size = 12),
        axis.title.x     = element_text(size = 12),
        axis.title.y     = element_text(size = 12),
        axis.text.x      = element_text(angle = 45, hjust = 1),
        legend.position  = "top"
    ) +
    ylim(-1, 11) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8)

plot_fancy

ggsave(file = "./output/time_series_fancy.pdf", plot = plot_fancy)

# Las medidas acá aproximan la propuesta de "inflación mediana" de la Reserva
# Federal de Cleveland y varios investigadores. Sin embargo, ellos calculan 
# una mediana ponderada, basada en el peso de cada item en el IPC. Aquí hemos
# calculado una mediana simple, sin ponderar. Ver:
# - https://www.clevelandfed.org/indicators-and-data/median-pce-inflation
# - https://www.nber.org/system/files/working_papers/w31032/w31032.pdf