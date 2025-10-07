# Limpieza y visualización de datos
# Caso practico: Analizando los componentes del IPC
# Análisis de datos 2: Modelo de regresión lineal
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
library(modelsummary)

# Limpiar entorno de trabajo
graphics.off()
rm(list = ls())
cat("\014")

# ------------------------------------------------------------------------------
# 1) Cargar y limpiar datos
# ------------------------------------------------------------------------------

ipc_data <- read_csv("./input/clean_data.csv")

# Vamos a restringir el análisis a los datos a nivel nacional

selected_sample <- ipc_data %>%
    select(
        t_date,
        id_item,
        id_grupo,
        id_nivel,
        descr_grupo,
        descr,
        ipc,
    )


# Para evitar problemas de comparabilidad con el cambio de la canasta
# del IPC en 2024, vamos a seleccionar solo los años antes de 2024
selected_sample %<>% filter(year(t_date) <= 2023)

# Finalmente, vamos a enfocarnos en el análisis a nivel de "Grupo" y el
# general, así que vamos a filtrar los datos para quedarnos solo con esos
# Nota: usamos %in% para filtrar múltiples valores
selected_sample %<>% filter(id_nivel %in% c("General", "Grupo"))

# ------------------------------------------------------------------------------
# 2) Construir índices de inflación anual
# ------------------------------------------------------------------------------

# Calculemos las inflaciones anuales (variación interanual)
# Para cada grupo de productos, calculamos la inflación de 12 meses
ipc_data <- selected_sample %>%
    group_by(id_item) %>%
    arrange(t_date) %>%
    mutate(
        # lag(n=12) nos da el valor de hace 12 meses
        ipc_lag12 = lag(ipc, n = 12, order_by = t_date),
        # Fórmula de inflación: (IPC_actual / IPC_hace_12_meses - 1) * 100
        inf12m = (ipc / ipc_lag12 - 1) * 100
    ) %>%
    ungroup()

# Eliminamos observaciones sin inflación (primeros 12 meses)
ipc_data %<>% filter(!is.na(inf12m))

# Transformamos los datos de formato largo a ancho para el análisis
# Cada columna será un grupo de productos del IPC
inf12m_tab <- ipc_data %>%
    select(t_date, id_item, inf12m) %>%
    pivot_wider(names_from = id_item, values_from = inf12m)

# Creemos una tabla con los nombres de las columnas
grupo_list <- ipc_data %>%
    filter(year(t_date) == 2023) %>%
    select(id_grupo = id_item, descr_grupo = descr, id_nivel) %>%
    distinct()

# Añadamos una variable categórica para indicar el mes
dataTab <- inf12m_tab %<>% mutate(mes = forcats::as_factor(month(t_date)))

# Finalmente, usemos algunos nombres más descriptivos para facilitar el análisis
colnames(dataTab) <- c(
    "Fecha",
    "General",
    "Alimentos",
    "Alcohol",
    "Ropa",
    "Vivienda",
    "Muebles",
    "Salud",
    "Transporte",
    "Comunicaciones",
    "Recreacion",
    "Educacion",
    "Restaurantes",
    "Otros",
    "Mes"
)


# ------------------------------------------------------------------------------
# 3) Modelo de regresión lineal
# ------------------------------------------------------------------------------

# El comando básico de R para modelos de regresión lineal es lm() (linear model)
# El formato básico es lm(y ~ x1 + x2 + ... + xn, data = nombre_data_frame)
# donde:
#   - y es la variable dependiente (lo que queremos explicar)
#   - x1, x2, ..., xn son las variables independientes (explicativas)
#   - data especifica el data frame que contiene las variables

# Modelo básico: inflación general explicada por inflación de alimentos, vivienda y transporte
regOutput <- lm(General ~ Alimentos + Vivienda + Transporte, data = dataTab)
summary(regOutput)

# Aquí, el argumento es un objeto tipo fórmula, que se escribe como:
# y ~ x1 + x2 + ... + xn
# El símbolo ~ se lee "explicado por" o "en función de"

# FORMAS ALTERNATIVAS DE ESPECIFICAR FÓRMULAS:
# Se pueden crear objetos tipo fórmula usando as.formula()
# o escribiendo la fqórmula directamente en el argumento
# Opción 1: Crear fórmula como string y convertirla
mi_formula <- as.formula("General ~ Alimentos + Vivienda + Transporte")
regOutput <- lm(mi_formula, data = dataTab)
summary(regOutput)

# Opción 2: Crear fórmula directamente (más común)
mi_formula <- General ~ Alimentos + Vivienda + Transporte
regOutput <- lm(mi_formula, data = dataTab)
summary(regOutput)

# MODELOS MÁS COMPLEJOS:

# 1) Variables categóricas (dummies): factor() crea variables dummy automáticamente
# Esto incluye una dummy para cada mes (menos uno como referencia)
mi_formula <- General ~ Alimentos + factor(Mes)
regOutput <- lm(mi_formula, data = dataTab)
summary(regOutput)

# 2) Términos polinómicos: I() protege operaciones matemáticas en fórmulas
# Aquí añadimos el término cuadrático del transporte
mi_formula <- General ~ Alimentos + I(Transporte^2) + factor(Mes)
regOutput <- lm(mi_formula, data = dataTab)
summary(regOutput)

# 3) Tendencias temporales: incluir tiempo para capturar tendencias
# Creamos una variable de tiempo (t) que va de 1 a n
dataTab <- dataTab %>%
    arrange(Fecha) %>%
    mutate(t = row_number())  # row_number() asigna 1, 2, 3, ..., n

# Modelo con término cuadrático de alimentos y tendencia cuadrática del tiempo
mi_formula <- General ~ Alimentos + I(Alimentos^2) + factor(Mes) + t + I(t^2)
regOutput <- lm(mi_formula, data = dataTab)

# 4) Transformaciones de variables: sqrt(), log(), etc.
# La raíz cuadrada puede capturar relaciones no lineales
mi_formula <- General ~ sqrt(Alimentos) + factor(Mes) + t + I(t^2)
regOutput <- lm(mi_formula, data = dataTab)
summary(regOutput)

# 5) Polinomios ortogonales con poly(): más estable numéricamente
# poly(x, 3) crea polinomios ortogonales de grado 3 (x, x^2, x^3)
mi_formula <- General ~ poly(Alimentos, 3) + poly(Vivienda, 3) + factor(Mes) + t + I(t^2)
regOutput <- lm(mi_formula, data = dataTab)
summary(regOutput)

# Para remover la constante, podemos usar fijar en 0 el intercepto, o usar -1
mi_formula <- General ~ Alimentos + Vivienda + Transporte - 1
regOutput <- lm(mi_formula, data = dataTab)

summary(regOutput)

# ------------------------------------------------------------------------------
# 4) Organizar los resultados usando la librería broom
# ------------------------------------------------------------------------------

# La librería broom (parte del tidyverse) convierte resultados de modelos
# en data frames "tidy", facilitando su manipulación, visualización y exportación
# Tres funciones principales: tidy(), glance(), augment()


# Estimemos un modelo base para demostrar broom
regOutput <- lm(General ~ Alimentos + I(Alimentos^2), data = dataTab)

# Carguemos la librería broom
library(broom)

# tidy() extrae coeficientes, errores estándar, estadísticos t, p-valores
tidyRegOutput <- tidy(regOutput)
print("Coeficientes del modelo:")
tidyRegOutput

# glance() extrae estadísticas del modelo: R², AIC, BIC, etc.
tidyStats <- glance(regOutput)
print("Estadísticas del modelo:")
tidyStats

# augment() añade predicciones y residuos a los datos
# También permite hacer predicciones con datos nuevos

# Ejemplo: predicciones para valores específicos de inflación de alimentos
new_data <- data.frame(
    Alimentos = c(5, 10, 15)  # Diferentes niveles de inflación de alimentos
)
predictions <- augment(regOutput, newdata = new_data)
print("Predicciones para nuevos datos:")
predictions

# VISUALIZACIÓN DE PREDICCIONES
# Generemos una secuencia de valores para graficar la curva de predicción
new_data <- data.frame(
    Alimentos = seq(0, 20, by = 0.5)  # Rango amplio para ver la curva completa
)
predictions <- augment(regOutput, newdata = new_data)

# Gráfico que muestra datos observados vs predicciones del modelo
predictions_plot <- predictions %>% 
    ggplot(aes(x = Alimentos, y = .fitted)) +
    geom_line(color = "darkorange", linewidth = 1.2, alpha = 0.8) +
    labs(
        title = "Modelo de Regresión: Inflación General vs Alimentos",
        subtitle = "Línea naranja: predicciones del modelo | Puntos azules: datos observados",
        x = "Inflación de Alimentos (%, 12 meses)",
        y = "Inflación General (%, 12 meses)"
    ) +
    geom_point(data = dataTab, aes(x = Alimentos, y = General),
               color = "darkblue", size = 2, alpha = 0.6) +
    theme_minimal() +
    theme(plot.subtitle = element_text(size = 10, color = "gray50"))

print(predictions_plot)


# También podemos usar broom para comparar fácilmente diferentes especificaciones

# Modelo simple
modelo1 <- lm(General ~ Alimentos, data = dataTab)

# Modelo con término cuadrático  
modelo2 <- lm(General ~ Alimentos + I(Alimentos^2), data = dataTab)

# Modelo con múltiples variables
modelo3 <- lm(General ~ Alimentos + Vivienda + Transporte, data = dataTab)

# Comparar modelos usando glance()
comparacion_modelos <- bind_rows(
    glance(modelo1) %>% mutate(modelo = "Simple"),
    glance(modelo2) %>% mutate(modelo = "Cuadrático"), 
    glance(modelo3) %>% mutate(modelo = "Múltiple")
) %>%
    select(modelo, r.squared, adj.r.squared, AIC, BIC)

print("Comparación de modelos:")
print(comparacion_modelos)


# Finalmente, podemos usar la librería modelsummary para reportar resultados ....

# .. Como tablas
model_list <- list(
    "Modelo Simple"     = modelo1,
    "Modelo Cuadrático" = modelo2,
    "Modelo Múltiple"   = modelo3
)
comparacion_modelos <- modelsummary::modelsummary(model_list, output = "data.frame")
print("Comparación de modelos usando modelsummary:")
print(comparacion_modelos)  

# .. O como gráficos
modelsummary::modelplot(model_list,
                        coef_omit = "Intercept",
                        title = "Comparación de Coeficientes entre Modelos",
                        subtitle = "Modelos de Regresión Lineal",
                        xlab = "Valor del Coeficiente",
                        ylab = "Variables",
                        point_size = 3,
                        errorbar_size = 1,
                        colors = c("darkblue", "darkorange", "darkgreen")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text = element_text(size = 12))
