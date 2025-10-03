# Limpieza y visualización de datos
# Caso practico: Analizando los componentes del IPC
# Análisis 2: Distribución de la inflación anual de los items del IPC
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
# 4) Graficar la distribución de items en uno o dos periodos usando histogramas
# ------------------------------------------------------------------------------

# Enfoquémonos en la inflación de los items en diciembre de cada año
inf12m_plot <- ipc_data %>%
    filter(id_nivel == "Item", month(t_date) == 12) %>%
    mutate(t_year = year(t_date)) %>%
    select(t_year, id_item, inf12m)


# a) Histograma simple  --------------------------------------------------------

# Comenzamos por graficar un histograma de la distribución en 2019

hist_simple <- inf12m_plot %>%
    filter(t_year == 2019) %>%
    ggplot() +
    geom_histogram(aes(x = inf12m), bins = 20, fill = "lightblue", color = "darkblue")

hist_simple

ggsave(file = "./output/histogram_simple.pdf", plot = hist_simple)


# b) Histograma mas bonito  ----------------------------------------------------

# Hagamos algunos ajustes y centremoslo

min_x <- -15
max_x <- 15

hist_fancy <- inf12m_plot %>%
    filter(t_year == 2019) %>%
    mutate(inf12m = if_else(inf12m < min_x, min_x, inf12m)) %>%
    mutate(inf12m = if_else(inf12m > max_x, max_x, inf12m)) %>%
    ggplot() +
    geom_histogram(aes(x = inf12m), bins = 20, fill = "lightblue", color = "darkblue") +
    labs(
        title = "Distribución de la inflación anual de los items del IPC en 2019",
        x = "Inflación Anual (%)",
        y = "Frecuencia"
    ) +
    xlim(min_x - 1, max_x + 1) +
    theme_classic()

hist_fancy

ggsave(file = "./output/histogram_fancy.pdf", plot = hist_fancy)


# c) Histograma aún mas bonito  ----------------------------------------------------

min_x <- -15
max_x <- 15

hist_propio <- inf12m_plot %>%
    filter(t_year %in% c(2019, 2023)) %>%
    mutate(inf12m = if_else(inf12m < min_x, min_x, inf12m)) %>%
    mutate(inf12m = if_else(inf12m > max_x, max_x, inf12m)) %>%
    ggplot() +
    geom_histogram(aes(x = inf12m, y = after_stat(density), fill = factor(t_year)),
        bins = 40, color = "darkblue", position = "identity", alpha = 0.4
    ) +
        scale_fill_manual(
            name = "Año",
            values = c("2019" = "cadetblue", "2023" = "darkorange")
        ) +
    labs(
        title = "Distribución de la inflación anual de los items del IPC en 2019 y 2023",
        x = "Inflación Anual (%)",
        y = "Frecuencia (%)"
    ) +
    xlim(min_x - 1, max_x + 1)+ 
    theme_linedraw()

hist_propio

ggsave(file = "./output/histogram_propio.pdf", plot = hist_propio)


# d) Usando densidades kernel --------------------------------------------------------

min_x <- -15
max_x <- 15
density <- inf12m_plot %>%
    filter(t_year %in% c(2019, 2023)) %>%
    mutate(inf12m = if_else(inf12m < min_x, min_x, inf12m)) %>%
    mutate(inf12m = if_else(inf12m > max_x, max_x, inf12m)) %>%
    ggplot() +
    geom_density(aes(x = inf12m, color = factor(t_year), fill = factor(t_year)),
        linewidth = 1.2, alpha = 0.4
    ) +
        scale_fill_manual(
            name = "Año",
            values = c("2019" = "cadetblue", "2023" = "darkorange")
        ) +
        scale_color_manual(
            name = "Año",
            values = c("2019" = "cadetblue", "2023" = "darkorange")
        ) +
    labs(
        title = "Distribución de la inflación anual de los items del IPC en 2019 y 2023",
        x = "Inflación Anual (%)",
        y = "Densidad"
    ) +
    xlim(min_x - 1, max_x + 1)+ 
    theme_linedraw()

density

ggsave(file = "./output/denisity_simple.pdf", plot = density)


# e) Combinando ------------------------------------------------------------------

min_x <- -15
max_x <- 15
hist_denso <- inf12m_plot %>%
    filter(t_year %in% c(2019, 2023)) %>%
    mutate(inf12m = if_else(inf12m < min_x, min_x, inf12m)) %>%
    mutate(inf12m = if_else(inf12m > max_x, max_x, inf12m)) %>%
    ggplot() +
    geom_histogram(aes(x = inf12m, y = after_stat(density), fill = factor(t_year)),
                   bins = 40, color = "darkblue", position = "identity", alpha = 0.4,
                   linewidth = 0.1
    ) +
    geom_density(aes(x = inf12m, color = factor(t_year), fill = factor(t_year)),
                 linewidth = 1.2, alpha = 0
    ) +
    scale_fill_manual(
        name = "Año",
        values = c("2019" = "cadetblue", "2023" = "darkorange")
    ) +
    scale_color_manual(
        name = "Año",
        values = c("2019" = "cadetblue", "2023" = "darkorange")
    ) +
    labs(
        title = "Distribución de la inflación anual de los items del IPC en 2019 y 2023",
        x = "Inflación Anual (%)",
        y = "Densidad"
    ) +
    xlim(min_x - 1, max_x + 1) + 
    theme_linedraw()

hist_denso

ggsave(file = "./output/denisity_fancy.pdf", plot = hist_denso)
