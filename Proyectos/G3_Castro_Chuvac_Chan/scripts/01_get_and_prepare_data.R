# scripts/01_get_and_prepare_data.R
# ------------------------------------------------------------
# Objetivo: Descargar datos financieros y prepararlos para optimización
# ------------------------------------------------------------

# Cargar librerías
library(tidyverse)
library(tidyquant)
library(knitr)
library(dplyr)

# 1. Definir los tickers y período
tickers <- c(
    # Acciones/ETFs de USA
    "QQQ",
    # Materias primas y alternativos
    "MSFT",
    # Incluso acciones individuales
    "GLD"
)
tickers2 <- c(
    # Acciones/ETFs de USA
    "SPY", "QQQ", "IWM", "VTI",
    
    # Internacional
    "EFA", "EEM", "VEU",
    
    # Renta fija
    "IEF", "TLT", "LQD", "AGG","GOVT",
    
    # Materias primas y alternativos
    "GLD", "SLV", "DBC", "USO",
    
    # Sectores
    "XLK", "XLF", "XLV", "XLE",
    
    # Incluso acciones individuales
    "AAPL", "MSFT", "GOOGL", "AMZN"
)
from_date <- "2018-01-01"
to_date   <- Sys.Date()  # hasta hoy

# 2. Descargar precios ajustados (ajustados por dividendos y splits)
# Opción 1: Yahoo Finance (sin clave)
source <- "yahoo"
# Opción 2: Tiingo (requiere clave en .Renviron)
#source <- "tiingo"

prices <- tq_get(
    tickers,
    from = from_date,
    to = to_date,
    get = "stock.prices",
    source = source  # ← configurable
) %>%
    select(symbol, date, adjusted)  # solo lo que necesitamos

# Verificar que no falten datos
print("Primeras filas de precios:")
print(head(prices))

# 3. Pivotear a formato ancho (una columna por activo)
prices_wide <- prices %>%
    pivot_wider(names_from = symbol, values_from = adjusted)

# 4. Calcular rendimientos logarítmicos diarios
#    r_t = ln(P_t / P_{t-1})
returns_daily <- prices_wide %>%
    arrange(date) %>%
    mutate(across(all_of(tickers), ~ log(.x / lag(.x)))) %>%
    select(date, all_of(tickers)) %>%
    drop_na()  # eliminar primer NA

# 5. Extraer solo la matriz numérica de rendimientos (sin fecha)
returns_matrix <- as.matrix(returns_daily[, tickers])

# 6. Calcular rendimientos esperados ANUALIZADOS (252 días hábiles)
mean_returns_annual <- colMeans(returns_matrix) * 252

# 7. Calcular matriz de covarianza ANUALIZADA
cov_matrix_annual <- cov(returns_matrix) * 252

# 8. Guardar resultados en una lista (fácil de usar después)
portfolio_data <- list(
    tickers = tickers,
    returns_daily = returns_matrix,
    mean_returns = mean_returns_annual,
    cov_matrix = cov_matrix_annual,
    dates = returns_daily$date
)

# Opcional: guardar en RDS para no descargar cada vez
saveRDS(portfolio_data, "data/portfolio_data.rds")

# Mensaje de éxito
#cat("✅Datos preparados y guardados en 'data/portfolio_data.rds'\n")
#cat("Activos:", paste(tickers, collapse = ", "), "\n")
#cat("Período:", min(returns_daily$date), "a", max(returns_daily$date), "\n")

cat("\n RESUMEN DE DATOS DESCARGADOS\n")
cat("---------------------------------\n")
cat("Activos incluidos:", paste(tickers, collapse = ", "), "\n")
cat("Período analizado:", min(returns_daily$date), "→", max(returns_daily$date), "\n")
cat("Número de observaciones:", nrow(returns_daily), "\n\n")

