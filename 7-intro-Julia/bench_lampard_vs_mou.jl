# Microbenchmark para Lampard vs Mourinho en Julia
# Análisis de rendimiento del código de simulación
#
# Ángelo Gutiérrez Daza
# Programación II
# Programa de Estudios Superiores
# Banco de Guatemala
# 2025

using BenchmarkTools
using Random
using Statistics

# Configurar semilla para reproducibilidad
Random.seed!(123)

# Parámetros del modelo (mismos que el archivo original)
const p_mou = 0.05
const p_gol_frank_oo = 0.05
const p_gol_mou_oo = 0.05
const p_gol_frank_od = 0.03
const p_gol_mou_od = 0.01
const p_gol_frank_do = 0.01
const p_gol_mou_do = 0.03
const n_t = 90


# ======================================================================
# FUNCIÓN PARA MICROBENCHMARK
# ======================================================================

# Esta función replica exactamente tu código original completo
function frank_vs_mou()
    n_p = 11
    n_sim = 10000
    p_frank_vec = range(0, 1, length=n_p)
    prob_frank_gane = fill(NaN, n_p)
    
    for k in 1:n_p
        p_frank = p_frank_vec[k]
        gano_frank = fill(NaN, n_sim)
        
        for i in 1:n_sim
            marcador_frank = 0
            marcador_mou = 0
            
            for t in 1:n_t
                frank_o = rand() < p_frank
                mou_o = rand() < p_mou
                
                goal_frank = 0
                goal_mou = 0
                
                if frank_o == true && mou_o == true
                    goal_frank = rand() < p_gol_frank_oo ? 1 : 0
                    goal_mou = rand() < p_gol_mou_oo ? 1 : 0
                end
                
                if frank_o == true && mou_o == false
                    goal_frank = rand() < p_gol_frank_od ? 1 : 0
                    goal_mou = rand() < p_gol_mou_od ? 1 : 0
                end
                
                if frank_o == false && mou_o == true
                    goal_frank = rand() < p_gol_frank_do ? 1 : 0
                    goal_mou = rand() < p_gol_mou_do ? 1 : 0
                end
                
                if frank_o == false && mou_o == false
                    goal_frank = 0
                    goal_mou = 0
                end
                
                marcador_frank = marcador_frank + goal_frank
                marcador_mou = marcador_mou + goal_mou
            end
            
            gano_frank[i] = marcador_frank > marcador_mou
        end
        
        prob_frank_gane[k] = mean(gano_frank)
    end
    
    return prob_frank_gane
end

# ======================================================================
# MICROBENCHMARK
# ======================================================================

# Ejecutar benchmark
benchmark_resultado = @benchmark frank_vs_mou()

# Mostrar resultados
println("\nResultados del benchmark:")
println("-------------------------")
println(benchmark_resultado)

# Estadísticas detalladas
println("\nEstadísticas detalladas:")
println("------------------------")
println("Tiempo mínimo: ", minimum(benchmark_resultado.times) / 1e6, " ms")
println("Tiempo máximo: ", maximum(benchmark_resultado.times) / 1e6, " ms")
println("Tiempo mediano: ", median(benchmark_resultado.times) / 1e6, " ms")
println("Tiempo promedio: ", mean(benchmark_resultado.times) / 1e6, " ms")
println("Desviación estándar: ", std(benchmark_resultado.times) / 1e6, " ms")
