# Limpieza y visualización de datos
# Caso practico: Analizando los componentes del IPC
# Análisis 3: Distribución de la inflación anual de los items del IPC a través del tiempo
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
library(magrittr)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(forcats)


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
# 4) Graficar la distribución de cada dos años usando box plots
# ------------------------------------------------------------------------------

# Enfoquémonos en la inflación de los items en diciembre de cada año
inf12m_plot <- ipc_data %>%
    filter(id_nivel == "Item", month(t_date) == 12) %>%
    mutate(t_year = year(t_date)) %>%
    filter(t_year %in% c(2014,2016,2018,2020,2022)) %>% 
    select(t_year, id_item, inf12m)


# a) Box plot   ----------------------------------------------------------------

# Comenzamos por graficar un box plot de la distribución en 2019

box_plot <- inf12m_plot %>%
    ggplot(aes(x = factor(t_year), y = inf12m, fill = factor(t_year))) +
    geom_boxplot(alpha = 0.3, outliers = FALSE) +
    labs(
        title = "Distribución de la inflación anual de los items del IPC",
        x = "Año",
        y = "Inflación anual (%)"
    ) +
    theme_minimal() +
    theme(legend.position="none") +
    scale_fill_brewer(palette = "Dark2")
    
box_plot

ggsave(file = "./output/boxplot_simple.pdf", plot = box_plot)


# b) Ridge plot: fancy   ------------------------------------------------------

min_x <- -15
max_x <- 15
ridge_plot <- inf12m_plot %>%
    mutate(inf12m = if_else(inf12m < min_x, min_x, inf12m)) %>%
    mutate(inf12m = if_else(inf12m > max_x, max_x, inf12m)) %>%    
    ggplot(aes(x = inf12m, y = factor(t_year), fill = factor(t_year))) +
    geom_density_ridges(alpha = 0.7, scale = 1.5) +
    labs(
        title = "Distribución de la inflación anual de los items del IPC",
        x = "Inflación anual (%)",
        y = "Año"
    ) +
    theme_ipsum_pub() +
    theme(legend.position="none") +
    scale_fill_brewer(palette = "Dark2")

ridge_plot

ggsave(file = "./output/ridgeplot_fancy.pdf", plot = ridge_plot)


# c) Ridge plot: also fancy   --------------------------------------------------

min_x <- -20
max_x <- 20
ridge_plot_2 <- inf12m_plot %>%
    mutate(inf12m = if_else(inf12m < min_x, min_x, inf12m)) %>%
    mutate(inf12m = if_else(inf12m > max_x, max_x, inf12m)) %>%    
    ggplot(aes(x = inf12m, y = fct_rev(factor(t_year)), fill = ..x..)) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    scale_fill_viridis(name = "Inflación Anual", option = "C") +
    labs(title = 'Distribución de la inflación anual de los items del IPC', x = 'Inflación Anual (%)') +
    theme_ipsum() +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    )

ridge_plot_2

ggsave(file = "./output/ridgeplot_fancy_too.pdf", plot = ridge_plot_2)