# Introducción a metodos numericos en R - Parte 3:
# Optimizacion numerica
#
# Angelo Gutierrez Daza
# 2025
#
# Programacion II
# Programa de Estudios Superiores
# Banco de Guatemala
#
# Codigo probado utilizando la version 4.5.1 de R

graphics.off(); rm(list=ls());

################################################################################
# 1) Optimizacion sin restricciones
################################################################################

# Veremos dos formas: La facil y la dificil

### La facil: Comando optim()

# La forma mas rapida de optimizar es usando la funcion optim() que viene 
# incluido con la instalacion base de R
una_funcion <- function(x) return(x^2+2) # Definimos la funcion f(x)=x^2+2

# Vamos a encontrar el minimo de esta funcion

# Primero, debemos dar al algoritmo numerico un valor inicial
x0 <- 10 

# Luego usamos optim() para minimizar una_funcion
resultados <- optim(x0,una_funcion) 

resultados     # El output de la funcion es una lista
resultados$par # El minimizador de la funcion se guarda en el campo "par"

# Tambien podemos usar funciones con varios argumentos 
# Sin embargo, debemos ingresarlos como un vector
otra_funcion <- function(x) return( x[1]^2+x[2]^2 )
x0 = c(10,10) # El valor inicial debe ser un vector, esta vez
optim(x0,otra_funcion) # Note que el resultado no es exactamente cero

# Podemos reducir esta distancia con un criterio mas exigente
optim(x0,otra_funcion,control = list(reltol=1e-12))

# optim() permite tambien resolver problemas con restricciones de desigualdad

# Aca, minimizamos la misma funcion sujeta a y>=1
optim(x0,otra_funcion,method="L-BFGS-B",lower=c(-Inf,1)) 

### La menos facil: Librerias adicionales

# Algunos problemas de optimizacion pueden ser demasiado complejos o 
# dificiles para los algoritmos utilizados por optim()

# Afortunadamente, R cuenta con un gran numero de librerias que implementan todo
# tipo de algoritmos de optimizacion numerica
# Estas pueden ser consultadas en: 
# https://cran.r-project.org/web/views/Optimization.html

# Aca ilustamos el uso de una de estas: nloptr()

install.packages("nloptr")

# Otra vez un ejemplo sencillo de minimizacion
library("nloptr")
otra_funcion <- function(x) return( (x[1]-1)^2+(x[2]-2)^2 )
x0 = c(10,10) 

# El algoritmo BFGS es adecuado para funciones "suaves"
resultados <- lbfgs(x0,otra_funcion)       

# El algoritmo Nelder-Mead, basado en simplex, no requiere calcular derivadas
resultados <- neldermead(x0,otra_funcion)  


################################################################################
# 2) Optimizacion con restricciones
################################################################################

# La librerioa nloptr permite resolver problemas de optimizacion con
# restricciones de igualdad y desigualdad

# Este ejemplo es tomado directamente de la documentacion de la libreria

# Vamos a resolver el siguiente problema de optimizacion
# \min_{x} x1*x4*(x1 + x2 + x3) + x3
# s.t.
# x1*x2*x3*x4 >= 25
# x1^2 + x2^2 + x3^2 + x4^2 = 40
# 1 <= x1,x2,x3,x4 <= 5

# Para hacerlo, reescribimos la desigualdad como:
# 25 - x1*x2*x3*x4 <= 0
# y la igualdad como:
# x1^2 + x2^2 + x3^2 + x4^2 - 40 = 0

# Primero, definimos la funcion
# f(x) = x1*x4*(x1 + x2 + x3) + x3

eval_f <- function(x) {
    return( 
        list( "objective" = x[1]*x[4]*(x[1] + x[2] + x[3]) + x[3],
              "gradient"  = c( x[1] * x[4] + x[4] * (x[1] + x[2] + x[3]),
                               x[1] * x[4],
                               x[1] * x[4] + 1.0,
                               x[1] * (x[1] + x[2] + x[3]) 
                              ) 
              ) 
            )
}

# Incluir el gradiente hace que el algoritmo sea mas eficiente

# Ahora definimos las restricciones del problema de optimizacion como funciones

# Restricciones de desigualdad:
eval_g_ineq <- function(x) {
    constr <- c( 25 - x[1] * x[2] * x[3] * x[4] )
    grad   <- c( -x[2]*x[3]*x[4],
                 -x[1]*x[3]*x[4],
                 -x[1]*x[2]*x[4],
                 -x[1]*x[2]*x[3]
                )
    return( list( "constraints"=constr, "jacobian"=grad ) )
}

# Restricciones de Igualdad
eval_g_eq <- function(x) {
    constr <- c( x[1]^2 + x[2]^2 + x[3]^2 + x[4]^2 - 40 )
    grad <- c( 2.0*x[1],
               2.0*x[2],
               2.0*x[3],
               2.0*x[4] 
              )
    return( list( "constraints"=constr, "jacobian"=grad ) )
}

# Ahora definimos los valores iniciales
x0 <- c( 1, 5, 5, 1 )

# Y unos valores minimos y maximos que sirvan para acotar la solucion
lb <- c( 1, 1, 1, 1 )  # Valores minimos
ub <- c( 5, 5, 5, 5 )  # Valores maximos

# Especifiquemos el tipo de algoritmo y otras opciones
local_opts <- list(
 "algorithm" = "NLOPT_LD_MMA",
 "xtol_rel" = 1.0e-7
  )

opts <- list(
 "algorithm" = "NLOPT_LD_AUGLAG",
 "xtol_rel" = 1.0e-7,
 "maxeval" = 1000,
 "local_opts" = local_opts
  )

# Ahora si, invoquemos la funcion de optimizacion
res <- nloptr( 
    x0=x0,
	eval_f=eval_f,
    lb=lb,
	ub=ub,
	eval_g_ineq=eval_g_ineq,
	eval_g_eq=eval_g_eq,
	opts=opts
    )

print(res)


################################################################################
# 3) Optimizacion global
################################################################################

# Algunos problemas de optimizacion son "no convexos", y pueden tener varios
# minimos locales. En estos casos, los algoritmos vistos anteriormente pueden no 
# encontrar el minimo global.
#
# Una forma de abordar este problema es utilizando algoritmos de optimizacion
# global, que exploran el espacio de busqueda de forma mas amplia
# Un ejemplo de estos algoritmos es el "Particle Swarm Optimization":
# https://en.wikipedia.org/wiki/Particle_swarm_optimization
# Aca ilustramos su uso utilizando la libreria "pso"

library(pso)

# Usemos una funcion de prueba comunmente utilizada en la literatura
# La funcion de Ackley, que tiene varios minimos locales
ackley_function <- function(x, a = 20, b = 0.2, c = 2 * pi) {
    n <- length(x)
    term1 <- -a * exp(-b * sqrt((1/n) * sum(x^2)))
    term2 <- -exp((1/n) * sum(cos(c * x)))
    result <- term1 + term2 + a + exp(1)
    return(result)
}

# Grafiquemos la funcion
x_seq <- seq(-5, 5, length.out = 100)
y_seq <- seq(-5, 5, length.out = 100)
z_matrix <- outer(x_seq, y_seq, ackley_function)
contour(x_seq, y_seq, z_matrix, nlevels = 50, col = terrain.colors(50))
points(0, 0, col = "red", pch = 19)
# El minimo global esta en (0,0)

# Definimos los limites del espacio de busqueda
lower_bounds <- c(-10, -10)
upper_bounds <- c(10, 10)

optimal_solution <- psoptim(
    par = c(NA, NA), # Valor inicial: NA para que el algoritmo lo genere
    fn = ackley_function,
    lower = lower_bounds,
    upper = upper_bounds,
    control = list(maxit = 5000, s = 100)
)

# Veamos los resultados
print(optimal_solution$par)   
print(optimal_solution$value) 

# A modo de comparacion, veamos los resultados utilizando optim()
optim_results <- optim(
    par = c(5, 5), # Valor inicial
    fn = ackley_function,
    method = "L-BFGS-B",
    lower = lower_bounds,
    upper = upper_bounds,
    control = list(maxit = 5000)
)

print(optim_results$par)
print(optim_results$value)


################################################################################
# 4) Optimizacion lineal
################################################################################

# La optimizacion lineal es un caso especial de optimizacion donde la funcion
# objetivo y las restricciones son lineales.

# Ejemplo:
# Una empresa desea maximizar sus ganancias produciendo dos productos, A y B.
# Cada unidad de A genera una ganancia de $3 y cada unidad de B genera una 
# ganancia de $4. La produccion de cada unidad de A requiere 2 horas de trabajo
# y 1 unidad de materia prima, mientras que la produccion de cada unidad de B 
# requiere 1 hora de trabajo y 2 unidades de materia prima. La empresa dispone 
# de un total de 100 horas de trabajo y 80 unidades de materia prima.

# Formalmente, este problema puede expresarse como:
# Maximizar: 3A + 4B
# Sujeto a:
# 2A + 1B <= 100 (horas de trabajo)
# 1A + 2B <= 80  (materia prima)
# A >= 0
# B >= 0

# Estos problemas pueden resolverse de forma eficiente utilizando algoritmos
# especializados, como el metodo del simplex o los metodos de puntos interiores
# En R, podemos utilizar la libreria "lpSolve" para resolver problemas de
# optimizacion lineal.

install.packages("lpSolve")
library(lpSolve)
# Definimos los coeficientes de la funcion objetivo
objective_coeffs <- c(3, 4) # Coeficientes de la funcion objetivo
# Definimos la matriz de coeficientes de las restricciones
constraint_matrix <- matrix(c(2, 1, 1, 2), nrow = 2, byrow = TRUE)
# Definimos los signos de las restricciones
constraint_sense <- c("<=", "<=")
# Definimos los limites de las restricciones
constraint_rhs <- c(100, 80)
# Resolvemos el problema de optimizacion lineal
result <- lp("max", objective_coeffs, constraint_matrix, 
                    constraint_sense, constraint_rhs)

# Veamos los resultados
print(result$solution) # Valores optimos de A y B
print(result$objval)   # Valor maximo de la funcion objetivo

# Vale la pena mencionar que la teoria de optimizacion lineal merecio a 
# sus autores, Leonid Kantorovich y Tjalling C. Koopmans, el Premio Nobel de 
# Economia en 1975: https://www.nobelprize.org/prizes/economic-sciences/1975/summary/