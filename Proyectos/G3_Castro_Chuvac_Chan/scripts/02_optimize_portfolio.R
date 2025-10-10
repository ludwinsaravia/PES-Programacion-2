# scripts/02_optimize_portfolio.R
# ------------------------------------------------------------
# Optimización de portafolios: mínima varianza, máximo Sharpe, frontera eficiente
# ------------------------------------------------------------

library(tidyverse)
library(quadprog)

# 1. Cargar datos preparados en el Día 2
portfolio_data <- readRDS("data/portfolio_data.rds")
tickers <- portfolio_data$tickers
mu <- portfolio_data$mean_returns        # vector de rendimientos esperados (anualizados)
Sigma <- portfolio_data$cov_matrix       # matriz de covarianza (anualizada)

# 2. Tasa libre de riesgo (anualizada)
rf <- 0.02  # 2%

# 3. Función auxiliar: calcular rendimiento y volatilidad de un portafolio
portfolio_metrics <- function(weights, mu, Sigma) {
    ret <- sum(weights * mu)
    vol <- sqrt(t(weights) %*% Sigma %*% weights)
    return(c(return = ret, volatility = vol))
}

# 4. Portafolio de MÍNIMA VARIANZA (sin ventas en corto)
#    Minimizar: (1/2) w' Σ w
#    Sujeto a: sum(w) = 1, w >= 0

n_assets <- length(tickers)

Dmat <- Sigma
dvec <- rep(0, n_assets)

# Restricción de igualdad: sum(w) = 1
A_eq <- rep(1, n_assets)

# Restricciones de desigualdad: w >= 0
A_ineq <- diag(n_assets)

# Combinar: cada columna = una restricción
Amat <- cbind(A_eq, A_ineq)
bvec <- c(1, rep(0, n_assets))

# Resolver
sol_minvar <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1)
w_minvar <- sol_minvar$solution

# Calcular métricas inmediatamente
metrics_minvar <- portfolio_metrics(w_minvar, mu, Sigma)

# 5. Portafolio de MÁXIMO SHARPE (enfoque robusto)
#    Generamos portafolios en la frontera y elegimos el de mayor Sharpe

min_ret <- metrics_minvar["return"]
max_ret <- max(mu)
target_returns_sharpe <- seq(min_ret, max_ret, length.out = 50)

sharpe_ratios <- numeric(length(target_returns_sharpe))
weights_list <- vector("list", length(target_returns_sharpe))

for (i in seq_along(target_returns_sharpe)) {
    target_ret <- target_returns_sharpe[i]
    
    # Restricciones: sum(w) = 1, w'mu = target_ret, w >= 0
    Aeq_f <- cbind(rep(1, n_assets), mu)
    beq_f <- c(1, target_ret)
    
    Amat_f <- cbind(Aeq_f, diag(n_assets))  # w >= 0 → ya en forma A^T w >= b
    bvec_f <- c(beq_f, rep(0, n_assets))
    
    sol_f <- try(solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat_f, bvec = bvec_f, meq = 2), silent = TRUE)
    
    if (inherits(sol_f, "try-error") || any(is.na(sol_f$solution))) {
        sharpe_ratios[i] <- -Inf
        next
    }
    
    w_f <- sol_f$solution
    weights_list[[i]] <- w_f
    port_ret <- sum(w_f * mu)
    port_vol <- sqrt(t(w_f) %*% Sigma %*% w_f)
    sharpe_ratios[i] <- (port_ret - rf) / port_vol
}

# Encontrar el mejor Sharpe
best_idx <- which.max(sharpe_ratios)
if (sharpe_ratios[best_idx] <= -Inf) {
    stop("No se encontró un portafolio factible. Revisa los datos.")
}

w_sharpe <- weights_list[[best_idx]]
metrics_sharpe <- portfolio_metrics(w_sharpe, mu, Sigma)
sharpe_ratio <- sharpe_ratios[best_idx]

# 6. Generar FRONTERA EFICIENTE (para visualización)
target_returns <- seq(metrics_minvar["return"], max(mu), length.out = 30)
frontier_weights <- list()
frontier_metrics <- matrix(NA, nrow = length(target_returns), ncol = 2)

for (i in seq_along(target_returns)) {
    target_ret <- target_returns[i]
    
    Aeq_f <- cbind(rep(1, n_assets), mu)
    beq_f <- c(1, target_ret)
    
    Amat_f <- cbind(Aeq_f, diag(n_assets))
    bvec_f <- c(beq_f, rep(0, n_assets))
    
    sol_f <- try(solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat_f, bvec = bvec_f, meq = 2), silent = TRUE)
    
    if (inherits(sol_f, "try-error") || any(is.na(sol_f$solution))) {
        frontier_metrics[i, ] <- NA
        next
    }
    
    w_f <- sol_f$solution
    frontier_weights[[i]] <- w_f
    frontier_metrics[i, ] <- portfolio_metrics(w_f, mu, Sigma)
}

# Convertir a data frame limpio
frontier_df <- tibble(
    volatility = frontier_metrics[, 2],  # columna 2 = volatility
    return = frontier_metrics[, 1]       # columna 1 = return
) %>%
    drop_na()

# 7. Guardar todos los resultados
optimization_results <- list(
    tickers = tickers,
    minvar_weights = w_minvar,
    minvar_metrics = metrics_minvar,
    sharpe_weights = w_sharpe,
    sharpe_metrics = metrics_sharpe,
    sharpe_ratio = sharpe_ratio,
    frontier_df = frontier_df,
    frontier_weights = frontier_weights,
    target_returns = target_returns
)

saveRDS(optimization_results, "data/optimization_results.rds")

# Mensaje final
cat("✅ Optimización completada y guardada en 'data/optimization_results.rds'\n")
cat("Portafolio mínima varianza (pesos):\n")
print(setNames(round(w_minvar, 4), tickers))
cat("\nPortafolio máximo Sharpe (pesos):\n")
print(setNames(round(w_sharpe, 4), tickers))
cat("\nSharpe Ratio:", round(sharpe_ratio, 3), "\n")

# Crear data frame con los resultados clave
summary_table <- tibble(
    Portafolio = c("Minima Varianza", "Maximo Sharpe"),
    Retorno = c(metrics_minvar["return"], metrics_sharpe["return"]),
    Volatilidad = c(metrics_minvar["volatility"], metrics_sharpe["volatility"]),
    Sharpe = c((metrics_minvar["return"] - rf) / metrics_minvar["volatility"],
               (metrics_sharpe["return"] - rf) / metrics_sharpe["volatility"])
)

# Mostrar tabla básica
cat("\nResumen de optimizaciones:\n")
kable(summary_table, digits = 4, align = "c")

# -------------------------------------------
# 10. TABLA DE PESOS DE LOS ACTIVOS POR PORTAFOLIO
# -------------------------------------------

# Crear tabla de pesos comparativa
weights_df <- tibble(
    Activo = tickers,
    `Minima Varianza` = round(w_minvar, 4),
    `Maximo Sharpe` = round(w_sharpe, 4)
)

cat("\nPesos por activo en cada portafolio:\n")
kable(weights_df, digits = 4, align = "c")
