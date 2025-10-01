# Introducción a metodos numericos en R - Parte 4:
# Solucion de sistemas de ecuaciones no-lineales
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
# 1) Solucion de una ecuaciones no-lineales
################################################################################

# La instalacion base de R cuanta con la funcion uniroot() que permite
# encontrar raices de funciones de R a R de forma eficiente, dando 
# como valor inicial un intervalo

# Por ejemplo, consideremos f(x) = x^2+11x-6
una_funcion <- function(x) return(x^2+11*x-6)

# Ahora podemos usar uniroot() para encontrar la raiz de la funcion
# Debemos especificar un intervalo donde sabemos que hay una raiz
resultado <- uniroot(una_funcion, c(-5, 5))
raiz <- resultado$root
raiz

# Si nuestra funcion es un polinomio, podemos usar la funcion polyroot()
# para encontrar todas sus raices (reales y complejas)
coeficientes <- c(-6, 11, 1) # Coeficientes del polinomio
raices <- polyroot(coeficientes)
raices

# Parte real
Re(raices)

# Parte compleja
Im(raices)

# Modulo
Mod(raices)

################################################################################
# 2) Solucion de sistemas de ecuaciones
################################################################################

# R cuenta con muchos paquetes para resolver sistemas de ecuaciones no-lineales

# A continuacion se muestra como utilizar el paquete "nleqslv"
install.packages('nleqslv')
library(nleqslv)

# El paquete nleqsl nos permite encontrar las raices de problemas de la 
# forma f(x)=0, donde x es un vector en Rn

# Primer ejemplo: Una funcion muy facil: f(x)=x-4
muy_facil <- function(x){ return(x-4) } # Claramente, la solucion es x=4

x0 <- 2  # Debemos dar al algoritmo num?eico un valor inicial
resultados <- nleqslv(x0,muy_facil)  # Usamos nleqslv para encontrar x
x_opt <- resultados$x  # Sacamos el valor de x que resuelve la ecuacion
x_opt

# Segundo ejemplo: Misma funcion con argumento adicional
facil <- function(x,a,b){ return(x-a+b) } # Claramente, la solucion es x=a

# Note como el cuarto argumento es para la funcion "facil" (a=5)
resultados <- nleqslv(x0,facil,jac=NULL,b=0,a=5) 
x_opt_2 <- resultados$x 
x_opt_2

# Tercer ejemplo: Sistemas de ecuaciones
menos.facil <- function(X){
  x <- X[1] 
  y <- X[2]
  
  # La funcion recibe un vector y regresa un vector
  return(c(x+y-2,x*y-1))
  } 

x0 <- c(1,1) # Note que esta vez el valor inicial es un vector con dos elementos	
resultados <- nleqslv(x0,menos.facil)
x_opt_3 <- resultados$x 
x_opt_3
