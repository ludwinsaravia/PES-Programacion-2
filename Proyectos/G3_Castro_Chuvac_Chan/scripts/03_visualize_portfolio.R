# scripts/03_visualize_portfolio.R
# ------------------------------------------------------------
# Visualización de la frontera eficiente y portafolios óptimos
# ------------------------------------------------------------

library(tidyverse)
library(plotly)

# 1. Cargar datos de optimización
opt_results <- readRDS("data/optimization_results.rds")
portfolio_data <- readRDS("data/portfolio_data.rds")

tickers <- opt_results$tickers
mu <- portfolio_data$mean_returns
Sigma <- portfolio_data$cov_matrix
rf <- 0.02  # debe coincidir con el usado en optimización

# 2. Preparar datos de activos individuales
assets_df <- tibble(
    asset = tickers,
    volatility = sqrt(diag(Sigma)),  # volatilidad = sqrt(varianza)
    return = mu
)

# 3. Obtener métricas de portafolios óptimos
minvar_vol <- opt_results$minvar_metrics["volatility"]
minvar_ret <- opt_results$minvar_metrics["return"]

sharpe_vol <- opt_results$sharpe_metrics["volatility"]
sharpe_ret <- opt_results$sharpe_metrics["return"]

# 4. Crear Línea de Mercado de Capitales (CML)
#    Va desde (0, rf) hasta el portafolio de máximo Sharpe
cml_df <- tibble(
    volatility = c(0, sharpe_vol * 1.5),  # extendemos un poco más allá
    return = c(rf, rf + opt_results$sharpe_ratio * sharpe_vol * 1.5)
)

# 5. Construir el gráfico con ggplot2
p <- ggplot() +
    
    # Frontera eficiente
    geom_line(
        data = opt_results$frontier_df,
        aes(x = volatility, y = return),
        color = "steelblue",
        size = 1.2,
        linetype = "solid"
    ) +
    
    # Activos individuales
    geom_point(
        data = assets_df,
        aes(x = volatility, y = return, text = asset),
        color = "black",
        size = 3,
        shape = 19
    ) +
    geom_text(
        data = assets_df,
        aes(x = volatility, y = return, label = asset),
        vjust = -1,
        size = 3.5
    ) +
    
    # Portafolio de mínima varianza
    geom_point(
        aes(x = minvar_vol, y = minvar_ret),
        color = "red",
        size = 4,
        shape = 17  # triángulo
    ) +
    annotate("text", x = minvar_vol, y = minvar_ret, label = "Mínima Varianza", 
             vjust = -1, color = "red", size = 3.5) +
    
    # Portafolio de máximo Sharpe
    geom_point(
        aes(x = sharpe_vol, y = sharpe_ret),
        color = "green",
        size = 4,
        shape = 17
    ) +
    annotate("text", x = sharpe_vol, y = sharpe_ret, label = "Máximo Sharpe", 
             vjust = -1, color = "green", size = 3.5) +
    
    # Línea de Mercado de Capitales (CML)
    geom_line(
        data = cml_df,
        aes(x = volatility, y = return),
        color = "purple",
        linetype = "dashed",
        size = 1
    ) +
    annotate("text", x = tail(cml_df$volatility, 1), y = tail(cml_df$return, 1), 
             label = "CML", color = "purple", vjust = -0.5, size = 3.5) +
    
    # Etiquetas y tema
    labs(
        title = "Frontera Eficiente y Portafolios Óptimos",
        subtitle = paste("Tasa libre de riesgo =", rf * 100, "% | Sharpe Ratio =", round(opt_results$sharpe_ratio, 2)),
        x = "Volatilidad (Riesgo Anualizado)",
        y = "Rendimiento Esperado (Anualizado)",
        caption = "Datos históricos | Optimización con restricciones: w >= 0, sum(w) = 1"
    ) +
    theme_minimal(base_size = 12) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))

# 6. Convertir a gráfico interactivo
p_interactive <- ggplotly(p, tooltip = "text") %>%
    layout(
        title = list(
            text = "Frontera Eficiente y Portafolios Óptimos",
            x = 0.5
        ),
        hovermode = "closest"
    )

# 7. Mostrar el gráfico
print(p_interactive)

# 8. Opcional: guardar como HTML
#htmlwidgets::saveWidget(p_interactive, "plots/portfolio_frontier.html")

cat("✅ Gráfico interactivo generado.\n")
cat("Puedes guardarlo como HTML descomentando la línea 8.\n")