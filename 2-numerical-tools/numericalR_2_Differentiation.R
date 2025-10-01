# Introducción a metodos numericos en R - Parte 2:
# Diferenciacion numerica y simbolica
#
# Angelo Gutierrez Daza
# 2025
#
# Programacion II
# Programa de Estudios Superiores
# Banco de Guatemala
#
# Codigo probado utilizando la version 4.5.1 de R

graphics.off()
rm(list = ls())


################################################################################
# 1) Derivacion Numerica
################################################################################

### Derivadas y Jacobianos ###
# Para calcular derivadas, gradientes, hessianas, etc, de forma numerica,
# podemos usar la libreria "numDeriv"
install.packages('numDeriv')
library(numDeriv)

### Derivada Numerica
# Para calcular derivadas y gradientes de una funcion, evaluadas en un punto
# determinado, podemos usar grad()

# Probemos con una funcion de R^1 a R^1
la_funcion <- function(x)
    return(x^2)
grad(la_funcion, 2)             # Derivada de la funcion f(x)=x^2 evaluada en 2
grad(la_funcion, 0)             # Derivada de la funcion f(x)=x^2 evaluada en 0
grad(la_funcion, -2)            # Derivada de la funcion f(x)=x^2 evaluada en -2

# Derivada de la funcion f(x)=x^2 evaluada en el intevalo discreto -2:0.1:2
grad(la_funcion, seq(-2, 2, 0.1))

# Ahora, intentemos con una funcion de R^2 a R^1
otra_funcion <- function(x)
    return(x[1]^2 + x[2]^2)
grad(otra_funcion, c(2, 2))   # Derivada de la funcion evaluada en (2,2)
grad(otra_funcion, c(1, 2))   # Derivada de la funcion evaluada en (1,2)
grad(otra_funcion, c(0, 0))   # Derivada de la funcion evaluada en (0,0)
grad(otra_funcion, c(-2, -2)) # Derivada de la funcion evaluada en (-2,-2)

## Jacobiano ##
# La funcion jacobian() del paquete numDeriv nos permite encontrar el jacobiano
# de una funcion de R^n a R^m

# Jacobiano de funcion de R^1 a R^2
funcion_1_a_2 <- function(x)
    return(c(x^2, x^3))
jacobian(funcion_1_a_2, 1) # Jacobiano evaluado en 1
jacobian(funcion_1_a_2, 2) # Jacobiiano evaluado en 2

# Jacobiano de funcion de R^2 a R^2
funcion_2_a_2 <- function(x)
    return(c(x[1]^2 + x[2]^2, x[1]^3 + x[2]^3))
jacobian(funcion_2_a_2, c(1, 1)) # Jacobiano evaluado en (1,1)
jacobian(funcion_2_a_2, c(1, 1)) # Jacobiano evaluado en (2,2)

## Hessiana ##
# Finalmente, el comando hessian() nos permite calcular la matriz Hessiana de
# una funcion de R^n a R^1
la_ultima_funcion <- function(x)
    return(x[1]^2 + x[2]^3)
hessian(la_ultima_funcion, c(1, 1)) # Hessiana evaluada en el punto (1,1)
hessian(la_ultima_funcion, c(1, 2)) # Hessiana evaluada en el punto (1,2)
hessian(la_ultima_funcion, c(2, 1)) # Hessiana evaluada en el punto (2,1)
hessian(la_ultima_funcion, c(0, 0)) # Hessiana evaluada en el punto (0,0)


################################################################################
# 2) Derivacion Simbolica
################################################################################

# R no solo realiza computacion numerica
# Podemos utilizarlo tambien para realizar computacion simbolica
# Es decir,  para realizar operaciones matematicas sobre expresiones y simbolos
# que dan como resultado otros simbolos

# La forma mas rapida de hacerlo es usando el comando expression() para definir
# expresiones simbolicas

# Ejemplo: Definir la funcion x^2
sym_fun <- expression(x^2)
sym_fun
sym_fun[1]
sym_fun[[1]]

# Podemos evaluar la funcion para valores determinados usando el comando eval()
eval(sym_fun, envir = list(x = 2))

# O, de forma alternativa
x = 2
eval(sym_fun)

# Podemos identificar ademas que variables son simbolicas utilizando all.vars
all.vars(sym_fun)

# Otros comandos como D() permiten encontrar derivadas simples de una funcion
# de una variable de forma simbolica
D_sym_fun <- D(sym_fun, "x")
D_sym_fun
eval(D_sym_fun)

# Finalmente, podemos utilizar el comando deriv() para encontrar el gradiente
# y la matriz Hessiana de una funcion de varias variables

# Primero, definamos una funcion de varias variables
sym_more_fun <- expression(x^3 + y^3)

sym_more_fun
sym_more_fun[1]
sym_more_fun[[1]]
all.vars(sym_more_fun)

# Ahora, obtengamos el gradiente usando deriv()
D_sym_fun_2 <- deriv(sym_more_fun, c("x", "y"))
D_sym_fun_2

# Podemos evaluar esta expresion en un punto determinado
x = 1
y = 1

eval(D_sym_fun_2)

# Tambien podemos usar derive para encontrar la matriz Hessiana de f(x,y)
hessian_mat <- deriv(sym_more_fun, c("x", "y"), hessian = T)
hessian_mat

# Ahora necesitamos attr() para recuperar la matriz Hessiana
hessian_fun <- attr(eval(hessian_mat), "hessian")
hessian_fun[1, , ]

# Tambien podemos usar attr() para recuperar el gradiente
gradient_fun <- attr(eval(hessian_mat), "gradient")
gradient_fun

# Podemos usar sapply() para encontrar el jacobiano
sym_more_fun_2 <- expression(x^3 + y^3, x^2 + y^2)

sym_jacobian <- sapply(sym_more_fun_2, deriv, c("x", "y"))
eval(sym_jacobian[1])
eval(sym_jacobian[2])

# Estas funciones permiten construir "input" para algoritmos numericos
obj_fun <- expression(x^3 + x^2 + x + 1)
foc <- D(obj_fun, "x")
foc
new_fun <- function(x) {
    return(eval(foc))
}
new_fun(1)
x11()
plot(-10:10, new_fun(-10:10))

# Ahora usemos la funcion nleqslv
library(nleqslv)
resultados <- nleqslv(1, new_fun)  # Usamos nleqslv para encontrar x
x_opt <- resultados$x  # Sacamos el valor de x que resuelve la ecuacion
x_opt


# Las funciones simbolicas anteriores hacen parte de R base.
# Para calculos mas complejos, se pueden consultar las librerias "Deriv",
# "Ryacas" y "caracas" (un wrapper de "SymPy" en Python)
