# Simulación y Pronóstico de Series de Tiempo
# Modelo AR(1): Simulación, IRF y Pronóstico
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
library(forecast)     # Para funciones de series de tiempo
library(tseries)      # Para tests de series de tiempo
library(gridExtra)    # Para múltiples gráficos

# Limpiar entorno de trabajo
graphics.off()
rm(list = ls())
cat("\014")

# Fijar semilla para reproducibilidad
set.seed(5)

# ------------------------------------------------------------------------------
# 1) Modelo AR(1): Definición y Parámetros
# ------------------------------------------------------------------------------

# Nuestro proceso estocástico es:
# y(t) = μ + ρ*y(t-1) + ε(t)
# donde: ε(t) ~ N(0, σ²) y y(0) dado

cat("=== MODELO AR(1) ===\n")
cat("Proceso: y(t) = μ + ρ*y(t-1) + ε(t)\n")
cat("donde ε(t) ~ N(0, σ²)\n\n")

# Parámetros del modelo
T     <- 100        # Número de observaciones a simular
rho   <- 0.7        # Persistencia del proceso (-1 < ρ < 1 para estacionariedad)
mu    <- 0.02       # Constante del proceso
sigma <- 0.01       # Desviación estándar de los choques
y0    <- 0          # Valor inicial del proceso

# Mostrar parámetros
cat("Parámetros del modelo:\n")
cat("T =", T, "(períodos)\n")
cat("ρ =", rho, "(persistencia)\n") 
cat("μ =", mu, "(constante)\n")
cat("σ =", sigma, "(desv. est. choques)\n")
cat("y₀ =", y0, "(valor inicial)\n\n")

# Verificar condición de estacionariedad
if (abs(rho) < 1) {
  media_incondicional <- mu / (1 - rho)
  varianza_incondicional <- sigma^2 / (1 - rho^2)
  cat("El proceso es estacionario:\n")
  cat("Media incondicional =", round(media_incondicional, 4), "\n")
  cat("Varianza incondicional =", round(varianza_incondicional, 6), "\n\n")
} else {
  cat("¡ADVERTENCIA! El proceso no es estacionario (|ρ| ≥ 1)\n\n")
}

# ------------------------------------------------------------------------------
# 2) Función de Simulación AR(1)
# ------------------------------------------------------------------------------

# Función para simular un proceso AR(1)
ar1_sim <- function(rho_sim, mu_sim, sigma_sim, y0_sim, T_sim, epsilon_sim = NULL) {
  
  # Si no se proporcionan choques, generarlos
  if (is.null(epsilon_sim)) {
    epsilon_sim <- rnorm(T_sim, mean = 0, sd = sigma_sim)
  }
  
  # Vector para almacenar la serie
  y_sim <- vector(mode = "numeric", length = T_sim)
  y_sim[1] <- y0_sim
  
  # Simular el proceso
  for (t in 2:T_sim) {
    y_sim[t] <- mu_sim + rho_sim * y_sim[t - 1] + epsilon_sim[t]
  }
  
  return(list(
    y = y_sim,
    epsilon = epsilon_sim,
    parametros = list(rho = rho_sim, mu = mu_sim, sigma = sigma_sim, y0 = y0_sim)
  ))
}

# ------------------------------------------------------------------------------
# 3) Simulación del Proceso
# ------------------------------------------------------------------------------

cat("=== SIMULACIÓN DEL PROCESO AR(1) ===\n")

# Generar choques aleatorios
epsilon <- rnorm(T, mean = 0, sd = sigma)

# Simular el proceso
simulacion <- ar1_sim(rho, mu, sigma, y0, T, epsilon)
y <- simulacion$y

# Crear tibble para visualización
datos_sim <- tibble(
  t = 1:T,
  y = y,
  epsilon = epsilon
)

# Gráfico de la serie simulada
grafico_serie <- datos_sim %>%
  ggplot(aes(x = t, y = y)) +
  geom_line(color = "darkblue", size = 1) +
  geom_point(color = "red", alpha = 0.6, size = 0.8) +
  labs(
    title = "Serie de Tiempo AR(1) Simulada",
    subtitle = paste("ρ =", rho, ", μ =", mu, ", σ =", sigma),
    x = "Tiempo (t)",
    y = "y(t)"
  ) +
  theme_minimal() +
  geom_hline(yintercept = media_incondicional, 
             linetype = "dashed", color = "gray50", alpha = 0.7) +
  annotate("text", x = T * 0.8, y = media_incondicional + 0.005, 
           label = paste("Media incondicional =", round(media_incondicional, 3)),
           size = 3, color = "gray50")

print(grafico_serie) 
# Gráfico de los choques
grafico_choques <- datos_sim %>%
  ggplot(aes(x = t, y = epsilon)) +
  geom_col(fill = "orange", alpha = 0.7) +
  labs(
    title = "Choques Aleatorios ε(t)",
    x = "Tiempo (t)",
    y = "ε(t)"
  ) +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "black", size = 0.5)

print(grafico_choques)

# ------------------------------------------------------------------------------
# 4) Función de Impulso-Respuesta (IRF)
# ------------------------------------------------------------------------------

cat("\n=== FUNCIÓN DE IMPULSO-RESPUESTA ===\n")

# Una forma de estudiar las propiedades de un modelo de series de tiempo es 
# a través de su función de impulso-respuesta (IRF)
# ¿Qué pasa si solo ocurre un choque en un período específico?

# Simular IRF con un choque unitario en el período 25
T_irf <- 50
periodo_choque <- 25
magnitud_choque <- sigma  # Un choque de una desviación estándar

# Crear vector de choques (ceros excepto en período específico)
epsilon_irf <- rep(0, T_irf)
epsilon_irf[periodo_choque] <- magnitud_choque

# Simular el proceso con este choque específico
irf_sim <- ar1_sim(rho, mu, sigma, y0 = 0, T_irf, epsilon_irf)
y_irf <- irf_sim$y

# Crear datos para visualización
datos_irf <- tibble(
  t = 1:T_irf,
  y = y_irf,
  epsilon = epsilon_irf,
  periodo_post_choque = t - periodo_choque
) %>%
  mutate(
    tipo_periodo = case_when(
      t < periodo_choque ~ "Pre-choque",
      t == periodo_choque ~ "Choque",
      TRUE ~ "Post-choque"
    )
  )

# Calcular IRF teórica (respuesta analítica)
periodos_post <- 0:20
irf_teorica <- rho^periodos_post * magnitud_choque

datos_irf_teorica <- tibble(
  periodo_post_choque = periodos_post,
  irf_teorica = irf_teorica
)

# Gráfico de la función de impulso-respuesta
grafico_irf <- datos_irf %>%
  filter(t >= periodo_choque) %>%
  ggplot(aes(x = periodo_post_choque, y = y)) +
  geom_line(color = "darkblue", size = 1.2) +
  geom_point(aes(color = tipo_periodo), size = 2) +
  geom_line(data = datos_irf_teorica, 
            aes(x = periodo_post_choque, y = irf_teorica), 
            color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Función de Impulso-Respuesta AR(1)",
    subtitle = paste("Choque unitario de σ =", sigma, "en período", periodo_choque),
    x = "Períodos después del choque",
    y = "Respuesta",
    color = "Tipo de período"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Pre-choque" = "gray", 
                               "Choque" = "red", 
                               "Post-choque" = "darkblue")) +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.5) +
  annotate("text", x = 15, y = max(y_irf) * 0.8, 
           label = paste("IRF teórica: ρᵗ × σ"), 
           color = "red", size = 3.5)

print(grafico_irf)

# Mostrar velocidad de convergencia
cat("Análisis de convergencia:\n")
cat("Después de 5 períodos: respuesta =", round(rho^5 * magnitud_choque, 6), "\n")
cat("Después de 10 períodos: respuesta =", round(rho^10 * magnitud_choque, 6), "\n")
cat("Tiempo de vida media:", round(log(0.5) / log(rho), 1), "períodos\n\n")

# ------------------------------------------------------------------------------
# 5) Pronóstico con AR(1)
# ------------------------------------------------------------------------------

cat("=== PRONÓSTICO CON MODELO AR(1) ===\n")

# Primero, simulamos una realización del proceso (esto simula "datos reales")
epsilon_real <- rnorm(T, mean = 0, sd = sigma)
serie_real <- ar1_sim(rho, mu, sigma, y0, T, epsilon_real)
y_observado <- serie_real$y

# Parámetros de pronóstico
H <- 15  # Número de períodos a pronosticar

cat("Datos observados:", T, "períodos\n")
cat("Horizontes de pronóstico:", H, "períodos\n\n")

# Pronóstico puntual (óptimo): E[y(T+h)|información hasta T]
# Para AR(1): y(T+h) = μ + ρ*y(T+h-1)
# Que da: y(T+h) = μ/(1-ρ) + ρ^h * (y(T) - μ/(1-ρ))

y_ultimo <- y_observado[T]
pronosticos_puntuales <- numeric(H)

for (h in 1:H) {
  pronosticos_puntuales[h] <- media_incondicional + 
    rho^h * (y_ultimo - media_incondicional)
}

# Crear datos para pronóstico
datos_pronostico <- tibble(
  t = (T + 1):(T + H),
  y_pronostico = pronosticos_puntuales,
  tipo = "Pronóstico puntual"
)

# Datos observados
datos_observados <- tibble(
  t = 1:T,
  y_observado = y_observado,
  tipo = "Datos observados"
)

# Gráfico básico de pronóstico
grafico_pronostico_basico <- bind_rows(
  datos_observados %>% rename(valor = y_observado),
  datos_pronostico %>% rename(valor = y_pronostico)
) %>%
  ggplot(aes(x = t, y = valor, color = tipo)) +
  geom_line(size = 1) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_vline(xintercept = T + 0.5, linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = media_incondicional, 
             linetype = "dotted", color = "gray50", alpha = 0.7) +
  labs(
    title = "Pronóstico AR(1): Predicción Puntual",
    subtitle = paste("Últimos", min(20, T), "períodos observados y", H, "períodos pronosticados"),
    x = "Tiempo (t)",
    y = "y(t)",
    color = "Serie"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Datos observados" = "darkblue", 
                               "Pronóstico puntual" = "red")) +
  xlim(max(1, T - 20), T + H) +
  annotate("text", x = T + H/2, y = media_incondicional + 0.005, 
           label = "Media incondicional", 
           color = "gray50", size = 3, angle = 0)

print(grafico_pronostico_basico)

# ------------------------------------------------------------------------------
# 6) Intervalos de Confianza para Pronósticos
# ------------------------------------------------------------------------------

cat("\n=== INTERVALOS DE CONFIANZA PARA PRONÓSTICOS ===\n")

# Los pronósticos puntuales son útiles, pero queremos conocer la incertidumbre
# Construimos intervalos de confianza simulando múltiples trayectorias futuras

N_sim <- 1000  # Número de simulaciones para intervalos de confianza
alpha_ic <- 0.1  # Nivel de significancia (90% de confianza)

cat("Simulaciones para IC:", N_sim, "\n")
cat("Nivel de confianza:", (1 - alpha_ic) * 100, "%\n\n")

# Matriz para almacenar todas las simulaciones
simulaciones_futuras <- matrix(NA, nrow = H, ncol = N_sim)

# Realizar N_sim simulaciones del futuro
set.seed(123)  # Para reproducibilidad
for (i in 1:N_sim) {
  # Generar choques futuros aleatorios
  epsilon_futuro <- rnorm(H, mean = 0, sd = sigma)
  
  # Simular trayectoria futura comenzando desde y_ultimo
  y_futuro <- numeric(H)
  y_temp <- y_ultimo
  
  for (h in 1:H) {
    y_temp <- mu + rho * y_temp + epsilon_futuro[h]
    y_futuro[h] <- y_temp
  }
  
  simulaciones_futuras[, i] <- y_futuro
}

# Calcular cuantiles para intervalos de confianza
ic_inferior <- apply(simulaciones_futuras, 1, quantile, probs = alpha_ic / 2)
ic_superior <- apply(simulaciones_futuras, 1, quantile, probs = 1 - alpha_ic / 2)

# Intervalos de confianza teóricos (fórmula analítica)
# Para AR(1): Var[y(T+h)] = σ² * (1 - ρ^(2h)) / (1 - ρ²)
varianzas_teoricas <- numeric(H)
for (h in 1:H) {
  if (h == 1) {
    varianzas_teoricas[h] <- sigma^2
  } else {
    varianzas_teoricas[h] <- sigma^2 * (1 - rho^(2*h)) / (1 - rho^2)
  }
}

ic_teorico_inferior <- pronosticos_puntuales - qnorm(1 - alpha_ic/2) * sqrt(varianzas_teoricas)
ic_teorico_superior <- pronosticos_puntuales + qnorm(1 - alpha_ic/2) * sqrt(varianzas_teoricas)

# Crear datos completos para visualización
datos_completos <- bind_rows(
  # Datos observados
  tibble(
    t = 1:T,
    valor = y_observado,
    tipo = "Observado",
    ic_inf = NA,
    ic_sup = NA
  ),
  # Pronósticos con intervalos
  tibble(
    t = (T + 1):(T + H),
    valor = pronosticos_puntuales,
    tipo = "Pronóstico",
    ic_inf = ic_inferior,
    ic_sup = ic_superior
  )
)

# Gráfico completo con intervalos de confianza
grafico_completo <- datos_completos %>%
  filter(t >= max(1, T - 25)) %>%  # Mostrar últimos 25 períodos
  ggplot(aes(x = t)) +
  # Banda de confianza
  geom_ribbon(aes(ymin = ic_inf, ymax = ic_sup), 
              alpha = 0.3, fill = "lightblue") +
  # Series
  geom_line(aes(y = valor, color = tipo), size = 1.2) +
  geom_point(aes(y = valor, color = tipo), size = 1.5, alpha = 0.8) +
  # Líneas de referencia
  geom_vline(xintercept = T + 0.5, linetype = "dashed", alpha = 0.6) +
  geom_hline(yintercept = media_incondicional, 
             linetype = "dotted", color = "gray50", alpha = 0.7) +
  labs(
    title = "Pronóstico AR(1) con Intervalos de Confianza",
    subtitle = paste("IC del", (1-alpha_ic)*100, "% basado en", N_sim, "simulaciones"),
    x = "Tiempo (t)",
    y = "y(t)",
    color = "Serie"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Observado" = "darkblue", "Pronóstico" = "red")) +
  theme(legend.position = "bottom")

print(grafico_completo)

# ------------------------------------------------------------------------------
# 7) Comparación y Resumen de Resultados
# ------------------------------------------------------------------------------

cat("=== RESUMEN DE RESULTADOS ===\n")

# Tabla resumen de pronósticos
tabla_pronosticos <- tibble(
  Horizonte = 1:min(10, H),
  `Pronóstico Puntual` = round(pronosticos_puntuales[1:min(10, H)], 4),
  `IC Inferior` = round(ic_inferior[1:min(10, H)], 4),
  `IC Superior` = round(ic_superior[1:min(10, H)], 4),
  `Ancho IC` = round(ic_superior[1:min(10, H)] - ic_inferior[1:min(10, H)], 4)
)

print(tabla_pronosticos)

# Estadísticas del modelo
cat("\nEstadísticas del modelo simulado:\n")
cat("Media de la serie observada:", round(mean(y_observado), 4), "\n")
cat("Desviación estándar observada:", round(sd(y_observado), 4), "\n")
cat("Autocorrelación lag-1 observada:", round(cor(y_observado[-1], y_observado[-T]), 4), "\n")
cat("Parámetro ρ teórico:", rho, "\n")

# Convergencia del pronóstico
cat("\nConvergencia del pronóstico:\n")
cat("Último valor observado:", round(y_ultimo, 4), "\n")
cat("Media incondicional:", round(media_incondicional, 4), "\n")
cat("Pronóstico a 1 período:", round(pronosticos_puntuales[1], 4), "\n")
cat("Pronóstico a", H, "períodos:", round(pronosticos_puntuales[H], 4), "\n")

cat("\n=== ANÁLISIS COMPLETADO ===\n")
cat("El modelo AR(1) muestra las siguientes características:\n")
cat("1. Persistencia:", ifelse(rho > 0.5, "Alta", "Moderada"), "(ρ =", rho, ")\n")
cat("2. Los pronósticos convergen a la media incondicional\n")
cat("3. La incertidumbre aumenta con el horizonte de pronóstico\n")
cat("4. Los intervalos de confianza reflejan la variabilidad del proceso\n")
