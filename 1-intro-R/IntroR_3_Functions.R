# Fundamentos de R - Parte 3:
# Funciones
#
# Angelo Gutierrez-Daza
# Programa de Estudios Superiores
# Banco de Guatemala
# 2025
#
# Probado usando R 4.5.1


################################################################################
# 1) Funciones Basicas 
################################################################################

# Comenzemos por limpiar todo el workspace
rm(list=ls()) 
ls()

# Recordemos como crear diferentes escalares, vectores y matrices en R

### Crear vectores
A  <- cbind(1,2)      # Vector de dimension 1x2
B  <- rbind(1,2)      # Vector de dimension 2x1
C  <- c(1,2)          # Comando para concatenar de forma horizontal
D  <- c(A,C)          # Tambien se puede usar para concatenar vectores

### Crear Matrices
AA <- matrix(c(1,2),nrow=2,ncol=1)         # Matriz 2x1
BB <- matrix(c(1,2),nrow=1,ncol=2)         # Matriz 1x2
CC <- matrix(c(1,2,3,4),ncol=2,nrow=2)     # Matriz 2x2 ordenada por columnas
DD <- matrix(c(1,2,3,4),ncol=2,byrow=TRUE) # Matriz 2x2 ordenada por filas

### Manipular Matrices
FF <- DD                  # Crear una matriz duplicado
FF                        # Ver en el command window
FF[1,1] <- 10             # Asignacion de elementos de la matriz
FF                        # Ver en el command window
FF[1,2]                   # Seleccion del elemento en la posicion (1,2)
GG <- cbind(FF,c(5,6))    # Contatenar vector columna a una matriz
GG                        # Ver en el command window
GG <- rbind(FF,c(7,8,9))  # Contatenar vector fila a una matriz
GG                        # Ver en el command window
GG[,1]                    # Seleccionar todos los elementos de la 1er columna
GG[1,1:2]                 # Elementos 1 hasta 2 de la primera fila
GG[-1,2]                  # Elementos de la 2da columna, excepto el primero
GG <- GG[1:2,]            # Reasignacion de datos

# R cuenta con una serie de funciones predeterminadas para manipular datos

############ La funcion mas importante de todas: La ayuda ######################

# Podemos buscar documentacion de cualquier funcion con el simbolo "?" 
?diag                

# De forma alternativa, podemos usar la funcion help()
help("diag")         

# Si usamos "??", buscamos ayuda usando "diag" como palabra clave
??diag               

# De forma alternativa, podemos utilizar el comando help.search()
help.search("diag")  

################################################################################

### Funciones para crear vectores
A<-seq(1,10)    # seq() crear vectores con sucesiones de variables
1:10            # De forma alternativa, podemos utilizar ":"
seq(1,10,2)     # seq() permite crear para sucesiones con distintos intervalos
seq(0,-10,-2)   # Y suseciones de numeros negativos
seq(0,1,len=5)  # P un vector con 5 elementos equidistantes entre 0 y 1
rep(1,10)       # rep() permite crear vectores de elementos repetidos

### Operaciones sobre matrices
AA; BB; GG;     # Recordemos estas matrices
t(BB)           # Transpuesta de una matriz
AA%*%BB         # Multiplicacion de matrices
BB%*%AA         # Multiplicacion de matrices
AA*t(BB)        # Producto punto de matrices
diag(2,3,3)     # Matriz diagonal de "2" de dimension 3x3
diag(c(1,2))    # Matriz diagonal con el vector indicado
diag(5)         # Matriz identidad de 5x5
diag(GG)        # Diagonal de la matriz GG
solve(GG)       # Inversa de la matriz GG
det(GG)         # Determinante de la matriz GG
eigen(GG)       # Valores y vectores propios de la matriz GG
dim(GG)         # Dimensiones de la matriz GG
ncol(GG)        # Numero de columnas de la matriz GG
nrow(GG)        # Numero de filas de la matriz GG

### Funciones para generar y trabajar con numeros aleatorios ###
M  <- runif(10,0,7)    # Vector de 10 realizaciones de una uniforme(0,7)
N  <- rnorm(1000,0,2)  # Vector de 1000 realizaciones de una normal(0,1)
O  <- rlnorm(30)       # Vector de 30 realizaciones log-normal(0,1)

# Podemos combinar varias de las funciones anteriores 
# Aca combinamos "matrix" y "rnorm" para crear una matriz con v.a. normales
MM <- matrix(rnorm(5*5,mean=0,sd=1),5,5) 

### Estadistica descriptiva sobre un vectr
length(M)  # Dimension de M
sum(M)     # Suma de los elementos del vector M
mean(M)    # Promedio del vector M
median(M)  # Mediana del vector M
var(M)     # Varianza del vector M
min(M)     # Elemento minimo del vector M
max(M)     # Elemento maximo del vector M

### Estadistica descriptiva sobre una matriz

# Note que las funciones anteriores pueden aplicarse a matrices, 
# en cuyo caso, la operacion se hace tomando todos los elementos de la matriz
dim(MM)    # Dimension de M
MM
sum(MM)    # Suma de los elementos del vector M
mean(MM)   # Promedio del vector M
var(MM)    # Varianza del vector M (normaliza usando n-1)
sd(MM)     # Desviacion estandar del vector M (normaliza usando n-1)
min(MM)    # Elemento minimo del vector M
max(MM)    # Elemento maximo del vector M

# De forma alternativa, podemos realizar la operacion para cada fila o columna
# de la matriz usando la funcion apply
MM
apply(MM,2,sum)  # Suma de los elementos de cada columna de la matriz MM
apply(MM,2,mean) # Promedio de los elementos de cada columna de la matriz MM
apply(MM,2,var)  # Varianza de los elementos de cada columna de la matriz MM
apply(MM,2,sd)   # Desv. est. de los elementos de cada columna de la matriz MM
apply(MM,2,min)  # Minimo de los elementos de cada columna de la matriz MM
apply(MM,2,max)  # Maximo de los elementos de cada columna de la matriz MM

################################################################################
# 2) Definir Funciones
################################################################################

### Introduccion
# Hemos visto ya varias de las funciones que vienen incorporadas en R
# Por ejemplo, sum() es una funcion: 
# Toma como argumento un vector y regresa la suma de sus elementos
x <- c(1,2)
y <- sum(x)

# Una funcion mas complicada es la funcion plot()
x <- 1:10
y <- x^2
plot(x,y,col="blue") 
# En este caso, "x" y "y" son argumentos posicionales, lo que quiere decir que
# el orden en que se ingresan importa:
plot(y,x,col="blue") # Reversamos el orden de "x" y "y"

# Por otro lado, el argumento "blue" es un argumento nombrado, lo que quiere 
# decir que el orden no importa. De hecho, es un argumento es opcional.

# Otro ejemplo es la funcion integrate(), la cual realiza integracion numerica
integrate(cos,-1,5) # Valor de la integral de cos(x) entre -1 y 5

# Note que el primer argumento de integrate() es una funcion

# Estas funciones vienen por defecto con R (built-in functions)

# Si bien son utiles, suele ser conveniente definir nuestras propias funciones
# Veamos como hacerlo

### Funciones definidas por el usuario

# Recuerdan que la funcion recibe funciones?
# Como podemos entonces evaluar la integral de f(x)= (x^2)*cos(x)  entre -1 y 5?
# La respuesta: Crear para integrate() nuestra propia funcion que evalue f(x) 

mi_funcion <- function(x) return((x^2)*cos(x)) # Asi de sencillo
integrate(mi_funcion,-1,5) 

# Note que "mi_funcion" es solo un nombre arbitrario y que return() especifica
# el valor devuelto por la funcion

### Otros ejemplos

# Una funcion sin argumentos
f <- function() print("Iniesta")
f()

# Una funcion con dos argumentos
g <- function(x,y) return(x^2 + 2*x*y + y^2)
g(2,3)

# Una funcion con argumentos opcionales
h <- function(x, intercepto=0) return(intercepto + 2*x)
h(1)
h(1,intercepto=3)

# Otra funcion util: Una funcion que calcule el estimador MCO, tomando como 
# dadas una matriz de regresores X y el vector Y con la variable dependiente

# Primero, simulemos unos datos
beta_sim <- matrix(c(1,2),ncol=1,nrow=2) # Coeficientes del modelo
X.sim    <- matrix(rnorm(1000),ncol=2)    # Regresores
eps      <- matrix(rnorm(500),ncol=1)     # Ruido
Y.sim    <- X.sim%*%beta_sim+eps         # Variable simulada

# Definir una funcion que calcule el estimador MCO
my_ols  <-  function(X,Y)  {
  beta <- (solve(t(X)%*%X))%*%(t(X)%*%Y)  
  return(beta)
}

# Probar la funcion usando los datos simulados
my_ols(X.sim,Y.sim)

### Reglas para las funciones
# - Pueden tener un numero de argumentos arbitrario (tanto posicionales como 
#    opcionales)
# - Los argumentos pueden ser cualquier objeto de R
# - Los valores retornados pueden ser cualquier objeto de R
# - Las funciones solo pueden devolver un objeto
# - Por lo anterior, para retornar varias variables o datos, se deben agrupar en
#   un vector, lista o data frame

### Variables Locales ###
# Las variables que se definan dentro de una funcion son "variables locales".
# Lo anterior quiere decir que solo existen dentro de la funcion. 
# Ejemplo:
x <- 1                            # Hacemos x igual a 1
f <- function() {
    a<-2;
    z <- 4 + a
    return(z)
    print(x)}  # Una funcion que hace que x sea igual a 2
f()                               # Corremos esta funcion
x                                 # x sigue siendo 1

### Usen funciones cuando puedan! ###
# - Dividen un programa largos en sus componentes logicos mas pequenos, lo que 
#   la hace mas facil de leer
# - Cada componente puede ser disenado de forma individual
# - Fomenta la reutilizacion de codigos
# - Ahorra el esfuerzo de repetir tareas comunes
