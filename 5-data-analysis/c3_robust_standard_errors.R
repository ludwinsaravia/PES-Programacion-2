# Limpieza y visualización de datos
# Caso practico: Analizando los componentes del IPC
# Análisis de datos 3: Errores estándar robustos
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
library(modelsummary)
library(broom)

# Librerías específicas para errores estándar robustos
library(sandwich)      # Para matrices de covarianza robustas
library(lmtest)        # Para tests con errores estándar robustos
library(clubSandwich) # Para errores estándar clustered

# Limpiar entorno de trabajo
graphics.off()
rm(list = ls())
cat("\014")

# ------------------------------------------------------------------------------
# 1) Cargar y preparar datos (mismo proceso que c2_linear_regression.R)
# ------------------------------------------------------------------------------

ipc_data <- read_csv("./input/clean_data.csv")

# Preparación de datos (simplificada del archivo anterior)
selected_sample <- ipc_data %>%
    select(t_date, id_item, id_grupo, id_nivel, descr_grupo, descr, ipc) %>%
    filter(year(t_date) <= 2023) %>%
    filter(id_nivel %in% c("General", "Grupo"))

# Calcular inflaciones anuales
ipc_data <- selected_sample %>%
    group_by(id_item) %>%
    arrange(t_date) %>%
    mutate(
        ipc_lag12 = lag(ipc, n = 12, order_by = t_date),
        inf12m = (ipc / ipc_lag12 - 1) * 100
    ) %>%
    ungroup() %>%
    filter(!is.na(inf12m))

# Transformar a formato ancho
inf12m_tab <- ipc_data %>%
    select(t_date, id_item, inf12m) %>%
    pivot_wider(names_from = id_item, values_from = inf12m)

# Añadir variables temporales
dataTab <- inf12m_tab %>%
    mutate(
        mes = forcats::as_factor(month(t_date)),
        año = year(t_date),
        t = row_number()
    )

# Usar nombres descriptivos
colnames(dataTab) <- c(
    "Fecha", "General", "Alimentos", "Alcohol", "Ropa", "Vivienda", 
    "Muebles", "Salud", "Transporte", "Comunicaciones", "Recreacion", 
    "Educacion", "Restaurantes", "Otros", "Mes", "Año", "t"
)

# ------------------------------------------------------------------------------
# 2) ¿Por qué necesitamos errores estándar robustos?
# ------------------------------------------------------------------------------

# Los errores estándar clásicos de OLS asumen:
# 1. Homocedasticidad (varianza constante de los errores)
# 2. No autocorrelación (independencia de los errores)
# 3. Normalidad de los errores

# En datos económicos de series de tiempo, estas asunciones frecuentemente se violan

# Estimemos un modelo base para diagnosticar problemas
modelo_base <- lm(General ~ Alimentos + Vivienda + Transporte + t, data = dataTab)

# Diagnósticos básicos
print(summary(modelo_base))

# Guardar residuos para análisis
dataTab$residuos <- residuals(modelo_base)
dataTab$fitted <- fitted(modelo_base)

# ------------------------------------------------------------------------------
# 3) Diagnósticos de heterocedasticidad
# ------------------------------------------------------------------------------

#### Pruebas de heterocedasticidad ---------------------------------------------    

# Test de Breusch-Pagan para heterocedasticidad
bp_test <- lmtest::bptest(modelo_base)
cat("Test de Breusch-Pagan:\n")
print(bp_test)

# Test de White para heterocedasticidad
# (versión general que no asume forma específica)
white_test <- lmtest::bptest(modelo_base, ~ fitted(modelo_base) + I(fitted(modelo_base)^2))
cat("\nTest de White:\n")
print(white_test)

# Visualización de heterocedasticidad
hetero_plot <- dataTab %>%
    ggplot(aes(x = fitted, y = residuos)) +
    geom_point(alpha = 0.6, color = "darkblue") +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    geom_smooth(method = "loess", color = "orange", se = FALSE) +
    labs(
        title = "Diagnóstico de Heterocedasticidad",
        subtitle = "Si hay heterocedasticidad, la dispersión cambia con los valores ajustados",
        x = "Valores ajustados",
        y = "Residuos"
    ) +
    theme_minimal()

hetero_plot

# ------------------------------------------------------------------------------
# 4) Diagnósticos de autocorrelación
# ------------------------------------------------------------------------------

# Test de Durbin-Watson para autocorrelación de primer orden
dw_test <- lmtest::dwtest(modelo_base)
cat("Test de Durbin-Watson:\n")
print(dw_test)

# Test de Breusch-Godfrey para autocorrelación de orden superior
bg_test <- lmtest::bgtest(modelo_base, order = 4)  # hasta 4 rezagos
cat("\nTest de Breusch-Godfrey (4 rezagos):\n")
print(bg_test)

# Visualización de autocorrelación
autocorr_plot <- dataTab %>%
    arrange(Fecha) %>%
    mutate(residuos_lag = lag(residuos)) %>%
    filter(!is.na(residuos_lag)) %>%
    ggplot(aes(x = residuos_lag, y = residuos)) +
    geom_point(alpha = 0.6, color = "darkblue") +
    geom_smooth(method = "lm", color = "orange", se = TRUE) +
    labs(
        title = "Diagnóstico de Autocorrelación",
        subtitle = "Correlación entre residuos consecutivos",
        x = "Residuos (t-1)",
        y = "Residuos (t)"
    ) +
    theme_minimal()

autocorr_plot

# ------------------------------------------------------------------------------
# 5) Tipos de errores estándar robustos
# ------------------------------------------------------------------------------

# Podemos utilizar la librería sandwich para calcular diferentes tipos de
# errores estándar robustos de forma fácil

# Matriz de covarianza clásica (OLS)
vcov_classical <- vcov(modelo_base)

# HC0: White (1980) - heterocedasticidad robusta básica
vcov_hc0 <- sandwich::vcovHC(modelo_base, type = "HC0")

# HC1: Corrección por grados de libertad
vcov_hc1 <- sandwich::vcovHC(modelo_base, type = "HC1")

# HC2: Corrección por leverage
vcov_hc2 <- sandwich::vcovHC(modelo_base, type = "HC2")

# HC3: Corrección por leverage (más conservadora)
vcov_hc3 <- sandwich::vcovHC(modelo_base, type = "HC3")

# HAC: Heterocedasticity and Autocorrelation Consistent (Newey-West)
vcov_hac <- sandwich::NeweyWest(modelo_base, lag = 4, prewhite = FALSE)

# ------------------------------------------------------------------------------
# 6) Comparación de errores estándar
# ------------------------------------------------------------------------------

# Usando broom para extraer información de los modelos de forma ordenada
# Función para crear tabla tidy con diferentes errores estándar
crear_tabla_tidy <- function(modelo, vcov_matrix, tipo_error) {
    # Calcular estadísticos t y p-values manualmente
    coefs <- coef(modelo)
    se <- sqrt(diag(vcov_matrix))
    t_stat <- coefs / se
    p_values <- 2 * pt(abs(t_stat), df = modelo$df.residual, lower.tail = FALSE)
    
    # Crear tibble tidy
    tibble(
        term = names(coefs),
        estimate = coefs,
        std.error = se,
        statistic = t_stat,
        p.value = p_values,
        tipo_error = tipo_error
    )
}

# Crear comparación completa usando broom style
comparacion_tidy <- bind_rows(
    crear_tabla_tidy(modelo_base, vcov_classical, "Clásico"),
    crear_tabla_tidy(modelo_base, vcov_hc1, "HC1"),
    crear_tabla_tidy(modelo_base, vcov_hc3, "HC3"),
    crear_tabla_tidy(modelo_base, vcov_hac, "HAC")
) %>%
    mutate(
        across(c(estimate, std.error, statistic), ~ round(.x, 4)),
        p.value = round(p.value, 4),
        significativo = case_when(
            p.value < 0.01 ~ "***",
            p.value < 0.05 ~ "**", 
            p.value < 0.1 ~ "*",
            TRUE ~ ""
        )
    )

print(comparacion_tidy)

# ------------------------------------------------------------------------------
# 7) Tests de significancia con errores estándar robustos
# ------------------------------------------------------------------------------

# Podemos usar el output de la librería sandwich para hacer tests
# correctos usando lmtest

# Test t con errores estándar HC3
coeftest_hc3 <- lmtest::coeftest(modelo_base, vcov = vcov_hc3)
cat("Test t con errores estándar HC3:\n")
print(coeftest_hc3)

# Test t con errores estándar HAC (Newey-West)
coeftest_hac <- lmtest::coeftest(modelo_base, vcov = vcov_hac)
cat("\nTest t con errores estándar HAC (Newey-West):\n")
print(coeftest_hac)

# Test F conjunto con errores estándar robustos
# H0: coeficientes de Alimentos = Vivienda = Transporte = 0
waldtest_robust <- lmtest::waldtest(modelo_base, 
                                   c("Alimentos", "Vivienda", "Transporte"),
                                   vcov = vcov_hc3)
cat("\nTest F conjunto (HC3) - Alimentos, Vivienda, Transporte:\n")
print(waldtest_robust)

# ------------------------------------------------------------------------------
# 8) Usando modelsummary para presentar resultados
# ------------------------------------------------------------------------------

# Crear lista de modelos con diferentes errores estándar
modelos_lista <- list(
    "OLS" = modelo_base,
    "HC1" = modelo_base,
    "HC3" = modelo_base,
    "HAC" = modelo_base
)

# Especificar matrices de covarianza
vcov_lista <- list(
    "OLS" = vcov_classical,
    "HC1" = vcov_hc1, 
    "HC3" = vcov_hc3,
    "HAC" = vcov_hac
)

# Crear tabla usando modelsummary
tabla_resumen <- modelsummary::modelsummary(
    modelos_lista,
    vcov = vcov_lista,
    stars = c('*' = .1, '**' = .05, '***' = .01),
    title = "Comparación de Errores Estándar",
    notes = c("* p < 0.1, ** p < 0.05, *** p < 0.01",
              "OLS: errores estándar clásicos",
              "HC1: White con corrección por grados de libertad", 
              "HC3: White con corrección por leverage",
              "HAC: Newey-West con 4 rezagos"),
    gof_map = c("nobs", "r.squared", "adj.r.squared")
)

print(tabla_resumen)

# ------------------------------------------------------------------------------
# 9) Errores estándar clustered (ejemplo con clustering por año)
# ------------------------------------------------------------------------------

# Para datos panel o cuando hay grupos naturales, podemos usar clustering
# Ejemplo: clustering por año (aunque con pocos clusters)

# Usando clubSandwich para errores estándar clustered
vcov_cluster_año <- clubSandwich::vcovCR(modelo_base, 
                                        cluster = dataTab$Año, 
                                        type = "CR1")

# Test con errores estándar clustered
coeftest_cluster <- clubSandwich::coef_test(modelo_base, 
                                           vcov = vcov_cluster_año,
                                           test = "Satterthwaite")

cat("Test con errores estándar clustered por año:\n")
print(coeftest_cluster)

# ------------------------------------------------------------------------------
# 10) Visualización de intervalos de confianza
# ------------------------------------------------------------------------------

# Crear gráfico comparativo de intervalos de confianza usando comparacion_tidy
# Seleccionar solo algunos tipos de errores estándar para claridad
comparacion_ic <- comparacion_tidy %>%
    filter(tipo_error %in% c("Clásico", "HC3", "HAC")) %>%
    mutate(
        IC_inf = estimate - 1.96 * std.error,
        IC_sup = estimate + 1.96 * std.error
    ) %>%
    filter(term != "(Intercept)")  # Excluir intercepto para claridad

ic_plot <- comparacion_ic %>%
    ggplot(aes(x = term, y = estimate, color = tipo_error)) +
    geom_point(position = position_dodge(width = 0.3), size = 2) +
    geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), 
                  position = position_dodge(width = 0.3), 
                  width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    labs(
        title = "Comparación de Intervalos de Confianza (95%)",
        subtitle = "Diferentes tipos de errores estándar",
        x = "Variables",
        y = "Coeficiente ± 1.96 * Error Estándar",
        color = "Tipo de Error Estándar"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_manual(values = c("Clásico" = "blue", "HC3" = "red", "HAC" = "green"))

print(ic_plot)

# Tabla resumida de intervalos de confianza
cat("\n=== INTERVALOS DE CONFIANZA (95%) ===\n")
tabla_ic <- comparacion_ic %>%
    select(term, tipo_error, estimate, std.error, IC_inf, IC_sup, significativo) %>%
    mutate(
        IC_texto = paste0("[", round(IC_inf, 3), ", ", round(IC_sup, 3), "]"),
        estimate = round(estimate, 4),
        std.error = round(std.error, 4)
    ) %>%
    select(term, tipo_error, estimate, std.error, IC_texto, significativo)

print(tabla_ic)

# ------------------------------------------------------------------------------
# 11) Guardando resultados y recomendaciones
# ------------------------------------------------------------------------------

# Exportar tabla de comparación completa
write.csv(comparacion_tidy, "./output/comparacion_errores_estandar_completa.csv", row.names = FALSE)

# Exportar tabla de intervalos de confianza  
write.csv(tabla_ic, "./output/intervalos_confianza_comparacion.csv", row.names = FALSE)

# Exportar tabla de modelsummary en HTML
modelsummary::modelsummary(
    modelos_lista,
    vcov = vcov_lista,
    output = "./output/tabla_errores_robustos.html",
    title = "Comparación de Errores Estándar Robustos",
    stars = c('*' = .1, '**' = .05, '***' = .01),
    notes = c("* p < 0.1, ** p < 0.05, *** p < 0.01",
              "OLS: errores estándar clásicos",
              "HC1: White con corrección por grados de libertad", 
              "HC3: White con corrección por leverage",
              "HAC: Newey-West con 4 rezagos")
)
