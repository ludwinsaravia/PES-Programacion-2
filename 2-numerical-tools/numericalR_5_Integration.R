# Introducción a metodos numericos en R - Parte 5:
# Integracion numerica
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
# 1) Integracion numerica a través de reglas de cuadratura
################################################################################

# La instalacion base de R cuenta con una funcion para realizar integracion
# numerica que ya vimos anteriormente
una_funcion <- function(x) return((x^2)*cos(x)) # Definimos una funcion
integrate(una_funcion,-1,5) # Integramos, asi de sencillo

# Tambien podemos utilizarla para integrar sobre todo el dominio de la funcion
# (siempre que sea convergente)
otra_funcion <- function(x) return(1/((x+1)*sqrt(x)))
integrate(otra_funcion, lower = 0, upper = Inf)

# Pero integrate() no nos sirve para integrar funciones de varias variables
# Nuevamente, podemos acudir a una de tantas librerias de R
install.packages('cubature')
library(cubature)

# Definamos una funcion de R^2 a R^1
la_integral <- function(x) return(x[1]^2+x[2]^2) 
num.x <- 2 # Numero de inputs
num.y <- 1 # Numero de outputs

# Integracion usando algoritmo deterministico
cuhre(la_integral,lowerLimit=c(-2,-2),upperLimit=c(2,2)) 

# La solucion exacta es
print(128/3)

################################################################################
# 2) Integracion numerica a través de Monte-Carlo
################################################################################

# La integracion por Monte-Carlo es especialmente util para funciones de
# muchas variables, donde los metodos deterministas se vuelven ineficientes 
# o directamente inutilizables

# En esta, utilizamos el hecho de que la integral de una funcion f(x) en un
# dominio D es igual al valor esperado de f(X) donde X es una variable
# aleatoria uniformemente distribuida en D, multiplicado por el volumen de D
# Es decir, si X ~ U(D), entonces:
# ∫_D f(x) dx = V(D) * E[f(X)] = V(D) * ∫_D f(x) p(x) dx
# donde p(x) es la funcion de densidad de X, y V(D) es el volumen de D
# Por lo tanto, podemos aproximar la integral generando N muestras de X
# y promediando los valores de f(X)
# La precision de la aproximacion mejora con N, y el error es O(1/sqrt(N))

# Veamos un ejemplo sencillo
# Supongamos que queremos integrar f(x1,x2) = x1^2 + x2^2
# en el cuadrado [0,2]x[0,2]
set.seed(123) # Fijamos semilla para reproducibilidad
N <- 1000 # Numero de muestras
x1 <- runif(N,-2,2) # Generamos N muestras de X1 ~ U(-2,2)
x2 <- runif(N,-2,2) # Generamos N muestras de X2 ~ U(-2,2)
f_x <- x1^2 + x2^2 # Evaluamos f en las muestras
V <- 4*4 # Volumen del dominio
V*mean(f_x) # Promediamos los valores de f

# Comparacion con el resultado con el algoritmo deterministico
cuhre(la_integral,lowerLimit=c(-2,-2),upperLimit=c(2,2))

# Una alternativa es utilizar secuencias de baja discrepancia en lugar de
# numeros aleatorios, lo que puede mejorar la precision de la aproximacion
# Esto se conoce como integracion quasi-Monte-Carlo
install.packages('randtoolbox')
library(randtoolbox)
set.seed(123) # Fijamos semilla para reproducibilidad
N <- 5000 # Numero de muestras
x <- sobol(N, dim=2) # Generamos N muestras de una secuencia de Sobol
x1 <- 4*x[,1] - 2 # Escalamos a [-2,2]
x2 <- 4*x[,2] - 2 # Escalamos a [-2,2]
f_x <- x1^2 + x2^2 # Evaluamos f en las muestras
V <- 4*4 # Volumen del dominio
V*mean(f_x) # Promediamos los valores de f

# Podemos utilizar la funcion mc_integrate de la libreria "cooltools" para
# hacer integracion por Monte-Carlo utilizando secuencias de baja discrepancia 
# de Halton
install.packages('cooltools')
library(cooltools)
la_integral_vec <- function(X) return(rowSums(X^2)) 
mcintegral(la_integral_vec, a=c(-2,-2), b=c(2,2), n=5000, qmc = TRUE)


# La librería cubature también tiene una función para hacer integración por
# Monte-Carlo, llamada vegas(), la cual usa un algoritmo adaptativo
# más sofisticado que mejora la precisión de la estimación
vegas(la_integral,lowerLimit=c(-2,-2),upperLimit=c(2,2)) 
