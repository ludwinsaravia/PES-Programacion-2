# Microbenchmark para Lampard vs Mourinho
# Análisis de rendimiento del código de simulación
#
# Ángelo Gutiérrez Daza
# Programación II
# Programa de Estudios Superiores
# Banco de Guatemala
# 2025

# Cargar librerías necesarias
library(microbenchmark)

# Limpiemos
rm(list = ls())
graphics.off()
set.seed(123)

# Parámetros del modelo (mismos que el archivo original)
pMou          <- 0.05
pGol_Frank_OO <- 0.05
pGol_Mou_OO   <- 0.05
pGol_Frank_OD <- 0.03
pGol_Mou_OD   <- 0.01
pGol_Frank_DO <- 0.01
pGol_Mou_DO   <- 0.03
nT            <- 90

# ======================================================================
# FUNCIONES PARA MICROBENCHMARK
# ======================================================================

# Esta función replica exactamente tu código original completo
frank_vs_mou <- function() {
    nP <- 21
    nSim <- 10000
    pFrank_vec <- seq(0, 1, length.out = nP)
    probFrankGane <- rep(NaN, nP)
    
    for (k in 1:nP) {
        pFrank <- pFrank_vec[k]
        ganoFrank <- rep(NaN, nSim)
        
        for (i in 1:nSim) {
            marcardor_Frank <- 0
            marcardor_Mou <- 0
            
            for (t in 1:nT) {
                Frank_O <- runif(1) < pFrank
                Mou_O <- runif(1) < pMou
                
                Goal_Frank <- 0
                Goal_Mou <- 0
                if (Frank_O == T && Mou_O == T) {
                    Goal_Frank <- runif(1) < pGol_Frank_OO
                    Goal_Mou <- runif(1) < pGol_Mou_OO
                }
                if (Frank_O == T && Mou_O == F) {
                    Goal_Frank <- runif(1) < pGol_Frank_OD
                    Goal_Mou <- runif(1) < pGol_Mou_OD
                }
                if (Frank_O == F && Mou_O == T) {
                    Goal_Frank <- runif(1) < pGol_Frank_DO
                    Goal_Mou <- runif(1) < pGol_Mou_DO
                }
                if (Frank_O == F && Mou_O == F) {
                    Goal_Frank <- 0
                    Goal_Mou <- 0
                }
                
                marcardor_Frank <- marcardor_Frank + Goal_Frank
                marcardor_Mou <- marcardor_Mou + Goal_Mou
            }
            
            ganoFrank[i] <- marcardor_Frank > marcardor_Mou
        }
        
        probFrankGane[k] <- mean(ganoFrank)
    }
    
    return(probFrankGane)
}

benchmark_completo <- microbenchmark(
    codigo_original = frank_vs_mou(),
    times = 56
)

print(benchmark_completo)
