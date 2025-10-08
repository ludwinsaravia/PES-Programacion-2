# Simulación y Calibración de Modelos Macroeconómicos y Econométricos
# Modelo 1: Modelo de Solow
# Calibración
#
# Ángelo Gutiérrez Daza
# 2025
#
# Programación II
# Programa de Estudios Superiores
# Banco de Guatemala
#
# Código probado utilizando la versión 4.5.1 de R

# Librerías usadas
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(magrittr)

# Limpiar entorno de trabajo
graphics.off()
rm(list = ls())
cat("\014")


# ------------------------------------------------------------------------------
# 1 - Conseguir datos del PIB per capita de Guatemala
# ------------------------------------------------------------------------------

# Alternativa 1: Utilizar los datos de Penn World Tables ***********************

library(pwt10)

# data() carga el dataset en el environment, no lo asigna directamente
data("pwt10.01")

# Ahora accedemos al objeto que se cargó en memoria y lo convertimos a tibble
pwt_recovered_data <- pwt10.01 %>% as_tibble()

pwt_dataTab <- pwt_recovered_data %>%
  filter(country == "Guatemala") %>%
  mutate(year = ym(paste0(year, "-01")), y_t_pwt = rgdpna / pop) %>%
  select(year, y_t_pwt) %>%
  arrange(year)

# Note que estos datos están disponibles hasta 2019.
# La versión 11 de las PWT acaba de salir (octubre 7 de 2025) y cubre hasta 2023
# pero su paquete de R aún no ha sido actualizado.
# Ver: https://www.rug.nl/ggdc/productivity/pwt/

# Alternativa 2: Utilizar la API del Banco Mundial *****************************

library(wbstats)

# Ahora vamos a obtener los datos del PIB per cápita de Guatemala
wb_recovered_data <- wb_data(
  country = "GTM",
  indicator = "NY.GDP.PCAP.KN"
)

wb_dataTab <- wb_recovered_data %>%
  mutate(year = ym(paste0(date, "-01")), y_t_wb = NY.GDP.PCAP.KN) %>%
  select(year, y_t_wb) %>%
  arrange(year)

# Remove attr(,"label") from the column name
attr(wb_dataTab$y_t_wb, "label") <- NULL

# Ver https://data.worldbank.org/indicator/NY.GDP.PCAP.KN?locations=GT

# Alternativa 3: Usar la API de FRED *******************************************

library(fredr)

# fredr_set_key("your_fred_api_key_here") # Reemplace con su clave de API de FRED
fredr_set_key("APIKEY") # Reemplace con su clave de API de FRED

fred_recovered_data_gdp <- fredr(
  series_id = "RGDPNAGTA666NRUG",
  observation_start = as.Date("1950-01-01"),
  observation_end = as.Date("2024-12-31")
)

fred_recovered_data_pop <- fredr(
  series_id = "POPTTLGTA148NRUG",
  observation_start = as.Date("1950-01-01"),
  observation_end = as.Date("2024-12-31")
)

fred_dataTab <- fred_recovered_data_gdp %>%
  inner_join(fred_recovered_data_pop, by = "date", suffix = c("_gdp", "_pop")) %>%
  mutate(year = as_date(date), y_t_fred = value_gdp / value_pop) %>%
  select(year, y_t_fred) %>%
  arrange(year)

# Ver: 
# https://fred.stlouisfed.org/series/RGDPNAGTA666NRUG
# https://fred.stlouisfed.org/series/POPTTLGTA148NRUG

# Comparemos

# Join, and set to missing the values not matched
dataTab <- pwt_dataTab %>% full_join(wb_dataTab, by = "year") %>%
  full_join(fred_dataTab, by = "year")

# Create variables normalized to 1 in 1960
dataTab %<>%
  mutate(
    y_t_pwt_norm = 100 * y_t_pwt / y_t_pwt[year == ym("1960-01")],
    y_t_wb_norm  = 100 * y_t_wb  / y_t_wb[year == ym("1960-01")],
    y_t_fred_norm= 100 * y_t_fred/ y_t_fred[year == ym("1960-01")]
  )

# Create growth rates
dataTab %<>%
  mutate(
    g_t_pwt = (y_t_pwt - lag(y_t_pwt)) / lag(y_t_pwt) * 100,
    g_t_wb  = (y_t_wb  - lag(y_t_wb))  / lag(y_t_wb)  * 100,
    g_t_fred= (y_t_fred- lag(y_t_fred))/ lag(y_t_fred)* 100
  ) 

# Gráfico comparativo
dataTab %>% 
  ggplot(aes(x = year)) +
  geom_point(aes(y = y_t_pwt_norm, color = "PWT"), size = 2) +
  geom_line(aes(y = y_t_wb_norm, color = "Banco Mundial"), size = 1) +
  geom_line(aes(y = y_t_fred_norm, color = "FRED"), size = 1, alpha = 0.5) +
  labs(
    title = "PIB per cápita de Guatemala",
    x = "Año",
    y = "Índice (1960 = 100)",
    color = "Fuente"
  ) +
  theme_minimal() +
  scale_color_manual(values = 
                       c("PWT" = "darkred", 
                         "Banco Mundial" = "darkblue",
                         "FRED" = "darkgreen")) +
  theme(legend.position = "bottom")

# Veamos las tasas de crecimiento
dataTab %>% 
  ggplot(aes(x = year)) +
  geom_point(aes(y = g_t_pwt, color = "PWT"), size = 2) +
  geom_line(aes(y = g_t_wb, color = "Banco Mundial"), size = 1) +
  geom_line(aes(y = g_t_fred, color = "FRED"), size = 1, alpha = 0.5) +
  labs(
    title = "Tasa de Crecimiento del PIB per cápita de Guatemala",
    x = "Año",
    y = "%",
    color = "Fuente"
  ) +
  theme_minimal() +
  scale_color_manual(values = 
                       c("PWT" = "darkred", 
                         "Banco Mundial" = "darkblue",
                         "FRED" = "darkgreen")) +
  theme(legend.position = "bottom")


# ------------------------------------------------------------------------------
# 1 - Conseguir datos del PIB per capita de Guatemala
# ------------------------------------------------------------------------------

# Ahora vamos a calibrar uno de los parámetros del modelo de Solow para tratar
# de acercar las simulaciones lo más posible a los datos del PIB de Guatemala

# Elegimos una serie
selected_data <- dataTab %>% select(year, y_t_wb) %>% filter(!is.na(y_t_wb)) 
Y_data <- selected_data %>% pull(y_t_wb)

# Función que simule una senda de capital y producto para un valor de g 
solow_sim <- function(s, delta, alpha, T, K0, A, L) {
  
  # Pre-asignemos memoria
  K <- vector(mode = "numeric", length = T)
  Y <- vector(mode = "numeric", length = T)
  C <- vector(mode = "numeric", length = T)
  I <- vector(mode = "numeric", length = T)
  
  # Definimos el nivel inicial del capital
  K[1] <- K0
  
  # Simulamos las sendas de capital, consumo, inversión y producto
  for (t in 1:T) {
    Y[t] <- (K[t]^alpha) * ((A[t] * L[t])^(1 - alpha))
    I[t] <- s * Y[t]
    C[t] <- (1 - s) * Y[t]
    K[t + 1] <- I[t] + (1 - delta) * K[t]
  }
  
  # Elimino el último dato de capital que sobra
  K <- K[1:T]
  
  # Guardamos los resultados en una lista
  Simulacion <- list(K_sim = K, Y_sim = Y, C_sim = C, I_sim = I)
  
  # Definimos el output de la función
  return(Simulacion)
}

## Ahora, definimos la función que calcula distancia entre datos simulados y
# verdaderos, dada una calibración

error_sim <- function(g, Y_data, s, delta, n, alpha) {
  
  # Número de observaciones a simular
  T <- length(Y_data)
  
  # Calculemos el valor inicial correspondiente
  K0 <- Y_data[1]^(1 / alpha)
  
  ## Simulamos la productividad
  A <- vector(mode = "numeric", length = T)
  A[1] <- 1 # Valor inicial de la productividad
  for (t in 2:T) {
    A[t] <- (1 + g) * A[t - 1]
  }
  
  # Simulamos la Oferta laboral
  L <- vector(mode = "numeric", length = T)
  L[1] <- 1 # Valor inicial de la oferta laboral
  for (t in 2:T) {
    L[t] <- (1 + n) * L[t - 1]
  }
  
  # Ahora usamos la función sim_solow para simular el modelo
  Simulacion <- solow_sim(s, delta, alpha, T, K0, A, L)
  
  
  # Recuperamos la senda de producto simulada y comparamos con los datos
  Y_sim <- Simulacion$Y_sim
  
  # Calculemos la distancia entre senda de producto simulada y la obtenida
  error <- sum((Y_sim - Y_data)^2) / 1000
  
  # Devolvamos el error
  return(error)
}

# Ahora vamos a calibrar nuestro modelo
s     <- 0.2            # Tasa de ahorro
delta <- 0.1            # Tasa de depreciación del capital
n     <- 0.02           # Tasa de crecimiento de la población
alpha <- 0.3            # Participación del capital en el producto

# Evaluemos la función un valor cualquiera de g
error_sim(0.1, Y_data, s, delta, n, alpha)

# Ahora usamos nuestra función para encontrar el óptimo

# Vamos a  definir la función objetivo del algoritmo de optimización
funcion_obj <- function(g) {
  error_sim(g, Y_data, s, delta, n, alpha)
}

# Evaluemos la función objetivo en un valor cualquiera de g
funcion_obj(0.1)

# Ahora usemos la función optim para encontrar el valor de g que minimiza el error

# Usamos optimize() para minimizar una_funcion
resultados <- optimize(f = funcion_obj, c(0, 1))
resultados # El output de la función es una lista

g_opt <- resultados$minimum # El g que minimiza la función
g_opt

## Finalmente, usemos el resultado para simular nuevamente el modelo y graficar

# Número de observaciones a simular
n_obs <- length(Y_data)

# Calculemos el valor inicial correspondiente
K0 <- Y_data[1]^(1 / alpha)

## Simulamos la productividad
A <- vector(mode = "numeric", length = n_obs)
A[1] <- 1 # Valor inicial de la productividad
for (t in 2:n_obs) {
  A[t] <- (1 + g_opt) * A[t - 1]
}

# Simulamos la oferta laboral
L <- vector(mode = "numeric", length = n_obs)
L[1] <- 1 # Valor inicial de la oferta laboral
for (t in 2:n_obs) {
  L[t] <- (1 + n) * L[t - 1]
}

# Ahora usamos la función sim_solow para simular el modelo
Simulacion <- solow_sim(s, delta, alpha, n_obs, K0, A, L)
Y_sim <- Simulacion$Y_sim

# Graficamos ambas series y comparamos usando ggplot2
# Crear un tibble con los datos para facilitar el plotting
datos_comparacion <- tibble(
  year = selected_data$year,
  Datos_Reales = Y_data,
  Modelo_Simulado = Y_sim
) %>%
  pivot_longer(cols = c(Datos_Reales, Modelo_Simulado), 
               names_to = "Serie", 
               values_to = "PIB_per_capita")

# Crear el gráfico con ggplot2
grafico_comparacion <- datos_comparacion %>%
  ggplot(aes(x = year, y = PIB_per_capita, color = Serie)) +
  geom_point(data = filter(datos_comparacion, Serie == "Datos_Reales"), 
             size = 2, alpha = 0.7) +
  geom_line(data = filter(datos_comparacion, Serie == "Modelo_Simulado"), 
            size = 1.2) +
  labs(
    title = "Calibración del Modelo de Solow para Guatemala",
    subtitle = paste("Tasa de crecimiento de la productividad calibrada: g =", round(g_opt, 4)),
    x = "Año",
    y = "PIB per cápita (USD constantes)",
    color = "Serie",
    caption = paste("Parámetros: s =", s, ", δ =", delta, ", n =", n, ", α =", alpha)
  ) +
  scale_color_manual(values = c("Datos_Reales" = "black", "Modelo_Simulado" = "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom"
  )

print(grafico_comparacion)

# Finalmente, detengamonos a reflexionar y pensar por qué este código no es muy eficiente
