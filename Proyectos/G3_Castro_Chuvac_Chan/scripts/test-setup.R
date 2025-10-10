# scripts/test-setup.R
library(tidyverse)
library(tidyquant)
library(plotly)

# Prueba rápida: ¿Podemos traer datos?
spy <- tq_get("SPY", from = "2020-01-01", to = "2023-12-31")
print(head(spy))

# Prueba de gráfico interactivo
p <- ggplot(spy, aes(x = date, y = adjusted)) +
    geom_line() +
    labs(title = "Precio de SPY (prueba de conexión)")

ggplotly(p)

# Cargar datos
portfolio_data <- readRDS("data/portfolio_data.rds")
returns_matrix <- portfolio_data$returns_daily

# Calcular correlación
cor_matrix <- cor(returns_matrix) #no usamos correlacion porque libreria quadprog usa matris de covarianza

# Mostrar como tabla
library(knitr)
kable(cor_matrix, digits = 2, caption = "Matriz de correlación (diaria)")
#################################

library(ggplot2)
library(reshape2)

cor_df <- melt(cor_matrix)
ggplot(cor_df, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    theme_minimal() +
    labs(title = "Correlación entre activos")

