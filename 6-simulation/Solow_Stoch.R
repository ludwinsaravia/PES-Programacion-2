# Simulación del Modelo de Solow Estocástico
# Modelo de Crecimiento con Choques de Productividad
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
library(patchwork)    # Para combinar gráficos
library(kableExtra)   # Para tablas profesionales

# Limpiar entorno de trabajo
graphics.off()
rm(list = ls())
cat("\014")

# Fijar semilla para reproducibilidad
set.seed(5)

# ------------------------------------------------------------------------------
# 1) Modelo de Solow Estocástico: Marco Teórico
# ------------------------------------------------------------------------------

cat("=== MODELO DE SOLOW ESTOCÁSTICO ===\n")
cat("El modelo de Solow con choques de productividad:\n\n")
cat("Ecuación de acumulación de capital:\n")
cat("K(t+1) = s·A(t)·K(t)^α·L(t)^(1-α) + (1-δ)·K(t)\n\n")
cat("Donde:\n")
cat("- A(t) = exp(ε(t)) es la productividad estocástica\n")
cat("- ε(t) ~ N(0, σ²) son choques de productividad\n")
cat("- Variables per cápita: k(t) = K(t)/L(t), y(t) = Y(t)/L(t)\n\n")

# Parámetros del modelo
T       <- 200        # Número de observaciones a simular
s       <- 0.25       # Tasa de ahorro
delta   <- 0.05       # Tasa de depreciación del capital
alpha   <- 0.35       # Participación del capital en el producto
sigma_e <- 0.02       # Desviación estándar de los choques de productividad
n       <- 0.02       # Tasa de crecimiento de la población

# Mostrar parámetros
cat("Parámetros del modelo:\n")
cat("T =", T, "(períodos de simulación)\n")
cat("s =", s, "(tasa de ahorro)\n")
cat("δ =", delta, "(tasa de depreciación)\n")
cat("α =", alpha, "(participación del capital)\n")
cat("σ =", sigma_e, "(desv. est. choques de productividad)\n")
cat("n =", n, "(tasa de crecimiento poblacional)\n\n")

# Calcular estado estacionario determinístico
k_ss <- (s / (n + delta))^(1 / (1 - alpha))
y_ss <- k_ss^alpha

cat("Estado estacionario (sin choques):\n")
cat("k* =", round(k_ss, 4), "(capital per cápita)\n")
cat("y* =", round(y_ss, 4), "(producto per cápita)\n\n")

# ------------------------------------------------------------------------------
# 2) Funciones de Simulación
# ------------------------------------------------------------------------------

# Función para simular el modelo usando la solución exacta
solow_sim_exact <- function(s, delta, alpha, n, k0, epsilon) {
  
  T <- length(epsilon)
  
  # Vectores para almacenar resultados
  k <- vector("numeric", T)
  y <- vector("numeric", T)
  
  # Condición inicial
  k[1] <- k0
  
  # Simulación exacta
  for (t in 1:T) {
    y[t] <- exp(epsilon[t]) * (k[t]^alpha)
    if (t < T) {
      k[t + 1] <- (s * exp(epsilon[t]) * (k[t]^alpha) + (1 - delta) * k[t]) / (1 + n)
    }
  }
  
  # Población (para obtener variables en niveles)
  L <- (1 + n)^(0:(T-1))
  
  # Variables en niveles
  K <- L * k
  Y <- L * y
  
  # Estado estacionario
  k_ss <- (s / (n + delta))^(1 / (1 - alpha))
  y_ss <- k_ss^alpha
  
  # Log-desviaciones del estado estacionario
  k_hat <- log(k) - log(k_ss)
  y_hat <- log(y) - log(y_ss)
  
  return(list(
    K = K, Y = Y, k = k, y = y,
    k_hat = k_hat, y_hat = y_hat,
    k_ss = k_ss, y_ss = y_ss,
    parametros = list(s = s, delta = delta, alpha = alpha, n = n)
  ))
}

# Función para simular el modelo usando la solución aproximada (log-linealizada)
solow_sim_approx <- function(s, delta, alpha, n, k_hat_0, epsilon) {
  
  T <- length(epsilon)
  
  # Vectores para almacenar resultados
  k_hat <- vector("numeric", T)
  y_hat <- vector("numeric", T)
  
  # Condición inicial
  k_hat[1] <- k_hat_0
  
  # Coeficientes de la aproximación log-lineal
  B <- (1 + alpha * n - delta * (1 - alpha)) / (1 + n)
  C <- (delta + n) / (1 + n)
  
  # Simulación aproximada
  for (t in 1:T) {
    y_hat[t] <- epsilon[t] + alpha * k_hat[t]
    if (t < T) {
      k_hat[t + 1] <- B * k_hat[t] + C * epsilon[t]
    }
  }
  
  # Estado estacionario
  k_ss <- (s / (n + delta))^(1 / (1 - alpha))
  y_ss <- k_ss^alpha
  
  # Recuperar variables en niveles per cápita
  k <- k_ss * exp(k_hat)
  y <- y_ss * exp(y_hat)
  
  # Población y variables en niveles
  L <- (1 + n)^(0:(T-1))
  K <- L * k
  Y <- L * y
  
  return(list(
    K = K, Y = Y, k = k, y = y,
    k_hat = k_hat, y_hat = y_hat,
    k_ss = k_ss, y_ss = y_ss,
    coeficientes = list(B = B, C = C),
    parametros = list(s = s, delta = delta, alpha = alpha, n = n)
  ))
}

cat("Funciones de simulación definidas:\n")
cat("1. solow_sim_exact(): Solución exacta del modelo no lineal\n")
cat("2. solow_sim_approx(): Solución aproximada (log-linealizada)\n\n")

# ------------------------------------------------------------------------------
# 3) Simulación Comparativa: Solución Exacta vs Aproximada
# ------------------------------------------------------------------------------

cat("=== SIMULACIÓN COMPARATIVA ===\n")

# Generar choques de productividad
los_choques <- rnorm(T, mean = 0, sd = sigma_e)

# Simulación con solución exacta
cat("Ejecutando simulación exacta...\n")
sol_exact <- solow_sim_exact(s, delta, alpha, n, k0 = k_ss, epsilon = los_choques)

# Simulación con solución aproximada
cat("Ejecutando simulación aproximada...\n")
sol_approx <- solow_sim_approx(s, delta, alpha, n, k_hat_0 = 0, epsilon = los_choques)

# Crear datos para comparación
datos_comparacion <- tibble(
  t = 1:T,
  # Choques
  epsilon = los_choques,
  # Capital per cápita
  k_exact = sol_exact$k,
  k_approx = sol_approx$k,
  # Producto per cápita
  y_exact = sol_exact$y,
  y_approx = sol_approx$y,
  # Variables en niveles
  K_exact = sol_exact$K,
  K_approx = sol_approx$K,
  Y_exact = sol_exact$Y,
  Y_approx = sol_approx$Y,
  # Log-desviaciones
  k_hat_exact = sol_exact$k_hat,
  k_hat_approx = sol_approx$k_hat,
  y_hat_exact = sol_exact$y_hat,
  y_hat_approx = sol_approx$y_hat
)

# Gráfico de choques de productividad
grafico_choques <- datos_comparacion %>%
  ggplot(aes(x = t, y = epsilon)) +
  geom_col(fill = "orange", alpha = 0.7) +
  labs(
    title = "Choques de Productividad ε(t)",
    x = "Tiempo (t)",
    y = "ε(t)"
  ) +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "black")

# Gráfico comparativo: Capital per cápita
grafico_capital <- datos_comparacion %>%
  select(t, k_exact, k_approx) %>%
  pivot_longer(cols = c(k_exact, k_approx), names_to = "metodo", values_to = "capital") %>%
  mutate(metodo = recode(metodo, "k_exact" = "Solución Exacta", "k_approx" = "Solución Aproximada")) %>%
  ggplot(aes(x = t, y = capital, color = metodo)) +
  geom_line(size = 1) +
  geom_point(alpha = 0.6, size = 0.8) +
  geom_hline(yintercept = k_ss, linetype = "dashed", color = "gray50") +
  labs(
    title = "Capital per Cápita k(t)",
    subtitle = "Comparación: Solución Exacta vs Aproximada",
    x = "Tiempo (t)",
    y = "k(t)",
    color = "Método"
  ) +
  scale_color_manual(values = c("Solución Exacta" = "black", "Solución Aproximada" = "blue")) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Gráfico comparativo: Producto per cápita
grafico_producto <- datos_comparacion %>%
  select(t, y_exact, y_approx) %>%
  pivot_longer(cols = c(y_exact, y_approx), names_to = "metodo", values_to = "producto") %>%
  mutate(metodo = recode(metodo, "y_exact" = "Solución Exacta", "y_approx" = "Solución Aproximada")) %>%
  ggplot(aes(x = t, y = producto, color = metodo)) +
  geom_line(size = 1) +
  geom_point(alpha = 0.6, size = 0.8) +
  geom_hline(yintercept = y_ss, linetype = "dashed", color = "gray50") +
  labs(
    title = "Producto per Cápita y(t)",
    subtitle = "Comparación: Solución Exacta vs Aproximada",
    x = "Tiempo (t)",
    y = "y(t)",
    color = "Método"
  ) +
  scale_color_manual(values = c("Solución Exacta" = "black", "Solución Aproximada" = "blue")) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Mostrar gráficos
print(grafico_choques)
print(grafico_capital)
print(grafico_producto)

# Calcular estadísticas de diferencias
cat("\nEvaluación de la aproximación:\n")
diferencia_k <- mean(abs(datos_comparacion$k_exact - datos_comparacion$k_approx))
diferencia_y <- mean(abs(datos_comparacion$y_exact - datos_comparacion$y_approx))

cat("Error absoluto medio en k(t):", round(diferencia_k, 6), "\n")
cat("Error absoluto medio en y(t):", round(diferencia_y, 6), "\n")
cat("Error relativo medio en k(t):", round(diferencia_k / mean(datos_comparacion$k_exact) * 100, 3), "%\n")
cat("Error relativo medio en y(t):", round(diferencia_y / mean(datos_comparacion$y_exact) * 100, 3), "%\n\n")

# ------------------------------------------------------------------------------
# 4) Análisis de Momentos Estadísticos
# ------------------------------------------------------------------------------

cat("=== ANÁLISIS DE MOMENTOS ===\n")

# Simulación larga para calcular momentos poblacionales
N_sim <- 5000
cat("Realizando simulación larga (N =", N_sim, ") para calcular momentos...\n")

# Generar choques para simulación larga
choques_largos <- rnorm(N_sim, mean = 0, sd = sigma_e)

# Simulaciones largas
sol_exact_long <- solow_sim_exact(s, delta, alpha, n, k0 = k_ss, epsilon = choques_largos)
sol_approx_long <- solow_sim_approx(s, delta, alpha, n, k_hat_0 = 0, epsilon = choques_largos)

# Extraer log-desviaciones (eliminar períodos iniciales para convergencia)
burn_in <- 500  # Períodos de "calentamiento"
k_hat_exact <- sol_exact_long$k_hat[-(1:burn_in)]
y_hat_exact <- sol_exact_long$y_hat[-(1:burn_in)]
k_hat_approx <- sol_approx_long$k_hat[-(1:burn_in)]
y_hat_approx <- sol_approx_long$y_hat[-(1:burn_in)]

# Calcular momentos empíricos
momentos_exactos <- tibble(
  Variable = c("k_hat", "y_hat"),
  Media = c(mean(k_hat_exact), mean(y_hat_exact)),
  Varianza = c(var(k_hat_exact), var(y_hat_exact)),
  `Desv. Est.` = sqrt(Varianza),
  Método = "Solución Exacta"
)

momentos_aproximados <- tibble(
  Variable = c("k_hat", "y_hat"),
  Media = c(mean(k_hat_approx), mean(y_hat_approx)),
  Varianza = c(var(k_hat_approx), var(y_hat_approx)),
  `Desv. Est.` = sqrt(Varianza),
  Método = "Solución Aproximada"
)

# Momentos teóricos (para la solución aproximada)
# Varianza de k_hat
var_k_theory <- (sigma_e^2) * (((delta + n)^2) / ((1 + n)^2 - (1 + alpha * n - delta * (1 - alpha))^2))
# Varianza de y_hat  
var_y_theory <- sigma_e^2 + (alpha^2) * var_k_theory

momentos_teoricos <- tibble(
  Variable = c("k_hat", "y_hat"),
  Media = c(0, 0),  # En log-desviaciones, la media teórica es 0
  Varianza = c(var_k_theory, var_y_theory),
  `Desv. Est.` = sqrt(Varianza),
  Método = "Teórico (Aproximado)"
)

# Combinar todos los momentos
tabla_momentos <- bind_rows(momentos_exactos, momentos_aproximados, momentos_teoricos) %>%
  mutate(across(c(Media, Varianza, `Desv. Est.`), ~ round(.x, 6)))

cat("\nComparación de momentos estadísticos:\n")
print(tabla_momentos)

# Autocorrelaciones
cat("\nAutocorrelaciones de primer orden:\n")
autocorr_k_exact <- cor(k_hat_exact[-1], k_hat_exact[-length(k_hat_exact)])
autocorr_y_exact <- cor(y_hat_exact[-1], y_hat_exact[-length(y_hat_exact)])
autocorr_k_approx <- cor(k_hat_approx[-1], k_hat_approx[-length(k_hat_approx)])
autocorr_y_approx <- cor(y_hat_approx[-1], y_hat_approx[-length(y_hat_approx)])

# Autocorrelación teórica para k_hat (AR(1) con coeficiente B)
B_teorico <- (1 + alpha * n - delta * (1 - alpha)) / (1 + n)
autocorr_k_theory <- B_teorico

tabla_autocorr <- tibble(
  Variable = c("k_hat", "y_hat"),
  `Exacta` = c(autocorr_k_exact, autocorr_y_exact),
  `Aproximada` = c(autocorr_k_approx, autocorr_y_approx),
  `Teórica` = c(autocorr_k_theory, NA)
) %>%
  mutate(across(c(Exacta, Aproximada, Teórica), ~ round(.x, 4)))

print(tabla_autocorr)

cat("\nCoeficiente B teórico (persistencia de k_hat):", round(B_teorico, 4), "\n\n")


# ------------------------------------------------------------------------------
# 5) Función de Impulso-Respuesta (IRF)
# ------------------------------------------------------------------------------

cat("=== FUNCIÓN DE IMPULSO-RESPUESTA ===\n")

# Analizar respuesta a choques de productividad
N_irf <- 50
cat("Calculando IRF para", N_irf, "períodos...\n")

# Caso 1: Choque transitorio (solo en período 10)
choque_transitorio <- rep(0, N_irf)
choque_transitorio[10] <- 0.05  # Choque positivo de 5%

cat("1. Choque transitorio de 5% en período 10\n")

# Caso 2: Choque permanente (a partir del período 10)
choque_permanente <- rep(0, N_irf)
choque_permanente[10:N_irf] <- 0.05  # Choque permanente de 5%

cat("2. Choque permanente de 5% a partir del período 10\n\n")

# Simular IRF para choque transitorio
irf_transitorio <- solow_sim_approx(s, delta, alpha, n, k_hat_0 = 0, epsilon = choque_transitorio)

# Simular IRF para choque permanente  
irf_permanente <- solow_sim_approx(s, delta, alpha, n, k_hat_0 = 0, epsilon = choque_permanente)

# Crear datos para gráficos de IRF
datos_irf <- tibble(
  t = 1:N_irf,
  # Choques
  choque_trans = choque_transitorio,
  choque_perm = choque_permanente,
  # Log-desviaciones - choque transitorio
  k_hat_trans = irf_transitorio$k_hat,
  y_hat_trans = irf_transitorio$y_hat,
  # Log-desviaciones - choque permanente
  k_hat_perm = irf_permanente$k_hat,
  y_hat_perm = irf_permanente$y_hat,
  # Variables en niveles - choque transitorio
  K_trans = irf_transitorio$K,
  Y_trans = irf_transitorio$Y,
  # Variables en niveles - choque permanente
  K_perm = irf_permanente$K,
  Y_perm = irf_permanente$Y
)

# Gráfico IRF: Capital per cápita (log-desviaciones)
grafico_irf_k <- datos_irf %>%
  select(t, k_hat_trans, k_hat_perm) %>%
  pivot_longer(cols = c(k_hat_trans, k_hat_perm), names_to = "tipo_choque", values_to = "k_hat") %>%
  mutate(tipo_choque = recode(tipo_choque, 
                             "k_hat_trans" = "Choque Transitorio", 
                             "k_hat_perm" = "Choque Permanente")) %>%
  ggplot(aes(x = t, y = k_hat, color = tipo_choque)) +
  geom_line(size = 1.2) +
  geom_point(alpha = 0.7, size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 10, linetype = "dotted", alpha = 0.7) +
  labs(
    title = "IRF: Capital per Cápita (Log-desviaciones)",
    subtitle = "Respuesta a choques de productividad del 5%",
    x = "Períodos después del choque",
    y = "log(k(t)) - log(k*)",
    color = "Tipo de Choque"
  ) +
  scale_color_manual(values = c("Choque Transitorio" = "blue", "Choque Permanente" = "red")) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Gráfico IRF: Producto per cápita (log-desviaciones)
grafico_irf_y <- datos_irf %>%
  select(t, y_hat_trans, y_hat_perm) %>%
  pivot_longer(cols = c(y_hat_trans, y_hat_perm), names_to = "tipo_choque", values_to = "y_hat") %>%
  mutate(tipo_choque = recode(tipo_choque, 
                             "y_hat_trans" = "Choque Transitorio", 
                             "y_hat_perm" = "Choque Permanente")) %>%
  ggplot(aes(x = t, y = y_hat, color = tipo_choque)) +
  geom_line(size = 1.2) +
  geom_point(alpha = 0.7, size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 10, linetype = "dotted", alpha = 0.7) +
  labs(
    title = "IRF: Producto per Cápita (Log-desviaciones)",
    subtitle = "Respuesta a choques de productividad del 5%",
    x = "Períodos después del choque",
    y = "log(y(t)) - log(y*)",
    color = "Tipo de Choque"
  ) +
  scale_color_manual(values = c("Choque Transitorio" = "blue", "Choque Permanente" = "red")) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Mostrar gráficos IRF
print(grafico_irf_k)
print(grafico_irf_y)
