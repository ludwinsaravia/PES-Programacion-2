# Fundamentos de Julia - Parte 3:
# Funciones
#
# Angelo Gutierrez-Daza
# Programa de Estudios Superiores
# Banco de Guatemala
# 2025
#
# Probado usando Julia 1.12.0

################################################################################
# 1) La función más importante: ayuda
################################################################################

# Simplemente presiona "?" en el REPL para entrar al modo ayuda
# En VS Code, también puedes usar Ctrl+Shift+P y buscar "Julia: Show Documentation"
# o colocar el cursor sobre una función y presionar Ctrl+K, Ctrl+I para documentación rápida

# También podemos usar apropos() para buscar por palabra clave
apropos("Diagonal")

# El paquete LinearAlgebra es requerido para operaciones estándar de matrices 
using LinearAlgebra # viene con la instalación base

### Funciones para crear vectores
A = 1:10   # Crear vectores con secuencias de variables
A # Nota que este objeto es de tipo "rango", ideal para usar como iterador
typeof(A)

# Para llenar este vector, podemos usar collect()
B = collect(1:10) 
B 
typeof(B)

# Rangos con saltos y en orden inverso
C = 1:2:10    
D = 10:-2:1   

### Funciones para crear matrices

# Algunas básicas
zeros(2,2)	 # Matriz de ceros
ones(2,2)	   # Matriz de 1's
fill(2,3,4)	 # Matriz 3x4 llena de 2's
trues(2,2)	 # Matriz 2x2 de "True"
falses(2,2)	 # Matriz 2x2 de "False"
rand(2,2)  	 # Matriz 2x2 de v.a. uniformes
randn(2,2)	 # Matriz 2x2 de v.a. normal estándar

# Podemos usar diagm para crear matrices "llenas" especificando la diagonal
diagm(0 => [1,2,1])              
diagm(1 => [1,2,3], -1 => [4,5])

# Julia cuenta con versiones más eficientes para la memoria
Diagonal([1,2,3])
Bidiagonal([1,2,3,4,5],[1,1,1,1] , :U)
Bidiagonal([1,2,3,4,5],[1,1,1,1] , :L)

# Mientras que diag (nota la minúscula) recupera la diagonal de la matriz
A_matrix = [1 2 3; 4 5 6; 7 8 9]
diag(A_matrix) 

### Operaciones sobre matrices

# Definamos algunas matrices
A = [ 1 2 ; 3 4 ]  
B = [ 5 6 ; 7 8 ]
C = [ 9 0 ; 1 2 ]
d = [ 1 ; 0 ]            

### Las básicas
B'         # Transpuesta (en realidad matriz conjugada)
A*B        # Multiplicación de matrices
B*d        # Multiplicación de matrices
A.*B       # Producto punto de matrices (elemento por elemento)
A^2        # Potencia de matriz (AxA)
inv(A)     # Inversa
pinv(A)    # Pseudo-Inversa (Moore-Penrose)
det(A)     # Determinante
tr(A)      # Traza
rank(A)    # Rango
eigen(A)   # Valores y vectores propios
eigvals(A) # Eigen-Valores
eigvecs(A) # Eigen-Vectores
size(A)    # Dimensiones
nrow, ncol = size(A)
A\d        # Resuelve A*x==d, mejor que usar inv(A)*d

### Por fila/columna
A
sum(A)               # Suma de todos los elementos de A
sum(A,dims=1)        # Suma de cada columna
sum(A,dims=2)        # Suma de cada fila
maximum(A)           # Máx de todos los elementos de A
maximum(A,dims=1)    # Máx de cada columna
maximum(A,dims=2)    # Máx de cada fila
minimum(A)           # Mín de todos los elementos de A
minimum(A,dims=1)    # Mín de cada columna
minimum(A,dims=2)    # Mín de cada fila

# Para estadísticas, necesitamos el paquete Statistics
using Statistics
mean(A)              # Media de todos los elementos de A
mean(A,dims=1)       # Media de cada columna
mean(A,dims=2)       # Media de cada fila
std(A)               # Desviación estándar de todos los elementos de A
std(A,dims=1)        # Desviación estándar de cada columna
var(A,dims=1)        # Varianza de cada columna
median(A)            # Mediana de todos los elementos de A

# En general, podemos usar . para aplicar una operación elemento por elemento
A .+ B
A .- B
A .* B
A ./ B
A .^ B
A .% B

### Curiosidad
# Julia cuenta con la matriz identidad I que adapta automáticamente su tamaño
# a las operaciones en que se use sin tener que definirla!
A = [1 2; 3 4]
inv(I-A)
B = [1 2 3; 4 5 6; 7 8 9]
# inv(I-B)  # Esta matriz no es invertible
C = A + I
D = B*I

# Algunas funciones que operan sobre escalares
a = 1.2
abs(a)		 # Valor absoluto
abs2(a)		 # Cuadrado
sqrt(a)		 # Raíz cuadrada
cbrt(a)		 # Raíz cúbica
exp(a)		 # Exponencial de a
exp2(a)		 # 2 elevado a la potencia a
log(a)		 # Log natural de a
log(2,a)	 # Log base 2 de a
real(a)		 # Parte real
imag(a)		 # Parte imaginaria
reim(a)		 # Parte real e imaginaria (como tupla)
sign(a)		 # Signo
round(a)	 # Redondear al flotante natural más cercano
ceil(a)		 # Redondear hacia arriba
floor(a)	 # Redondear hacia abajo

# Para operaciones con dos argumentos
b = 3.7
mod(a,b)	 # Módulo a,b

# Las operaciones son también funciones
1+2
+(1,2)
methods(+)
methods(exp)

# Para aplicarlas elemento por elemento, usamos la función map()
A = [1 0; 0 -1]
B = map(abs,A)

# Podemos usar map inclusive con funciones anónimas
C = map(x->3*x+2 , A) 

# Podemos crear arreglos de funciones y evaluarlas como tal
a = [exp, abs]
a[1]
a[1](3)

# Otra curiosidad: En Julia, expresiones como 2x calculan el producto 2*x
x = 2
2x
2x+2

# Podemos usar también operadores de asignación compuesta
x+=1		# x = x+1
x-=2		# x = x-2
x*=3		# x = 3*x
x/=4		# x = x/4
x^=5		# x = x^5

# Una curiosidad más de Julia: Las funciones que terminan en ! ("bang")
# ¡cambian el objeto original!
a = [5,3,4,2,1]
sort(a)    # No modifica a
a
sort!(a)   # ¡Modifica a!
a

# Las funciones ! son importantes para alterar variables existentes
A = [1,2,3]
# A[4] = 4 # ¡Error! (Pero funciona en Matlab!)
push!(A,4)  # Agrega elemento al final
A
# A[4] = [] # ¡Error! (Pero funciona en Matlab!)
pop!(A)     # Remueve último elemento
A
filter!(x->x>1,A)  # Filtra elementos in-place

# Esta clase de funciones es recomendada cuando se requiera modificar objetos
# ya que hacen un uso más eficiente de la memoria al evitar la copia de elementos

# Finalmente, hay funciones para operar sobre conjuntos!
a = [2,1,3]
b = [2,4,5]
union(a,b)	      # Unión
intersect(a,b)    # Intersección
setdiff(a,b)	    # Diferencia (elementos en a pero no en b)
setdiff(b,a)	    # Diferencia (elementos en b pero no en a)

###### Funciones para generar y trabajar con números aleatorios
using Distributions, Plots

# Julia cuenta con las funciones rand() y randn() en su librería base
M =  rand(10000)   # Vector de 10000 realizaciones de una uniforme(0,1)
N = randn(10000,2) # Matriz 10000x2 de realizaciones de una normal(0,1)
histogram(M, bins=50, title="Distribución Uniforme", xlabel="Valor", ylabel="Frecuencia")
histogram(N, bins=50, title="Distribución Normal", xlabel="Valor", ylabel="Frecuencia")

# La librería Distributions nos ofrece una interfaz para generar 
# v.a. de muchas otras distribuciones

# Para usarla, primero especificamos la familia de distribuciones
la_elegida = Normal(0,5)

# Y podemos usar rand() para simular de la distribución
X = rand(la_elegida,1000,2)
histogram(X, bins=50, title="Normal(0,5)", xlabel="Valor", ylabel="Frecuencia")

# Y otras funciones de la librería para calcular momentos
pdf(la_elegida, 0)      # Función de densidad
cdf(la_elegida, 0)      # Función de distribución acumulada
quantile(la_elegida, [0.25, 0.50, 0.95])  # Cuantiles

# Podemos inclusive truncar la distribución
truncada = Truncated(Normal(0, 1), 0, 3)
X = rand(truncada,1000)
histogram(X, bins=50, title="Normal Truncada (0,3)", xlabel="Valor", ylabel="Frecuencia")
pdf(truncada, 0)
cdf(truncada, 0)
quantile(truncada, [0.25, 0.50, 0.95])

# Podemos hacerlo para muchas otras distribuciones además de la normal
fieldnames(Binomial)
fieldnames(Multinomial)
fieldnames(Cauchy)
fieldnames(Beta)
fieldnames(MvNormal)

# Puede consultarse la lista completa en
# https://juliastats.org/Distributions.jl/stable/

# Otras funciones útiles para trabajar con números aleatorios
using Random
A = [1,2,3,4,5,6]
B = randsubseq(A,0.3) # Muestra aleatoria de elementos de A, incluidos con probabilidad 0.3
C = shuffle(A)        # Permutación aleatoria de elementos de A
D = randperm(5)       # Permutación aleatoria de números de 1 a 5
Random.seed!(123)     # Fijar la semilla del generador de números aleatorios

# El paquete StatsBase nos ofrece otra función para generar muestras 
# aleatorias con o sin reemplazo
using StatsBase
A = 1:10
sample(A, 5, replace=true, ordered=false)   # Con reemplazo
sample(A, 5, replace=false, ordered=false)  # Sin reemplazo

################################################################################
# 2) Definir Funciones
################################################################################

### Podemos definir funciones de varias formas: 

# Con una sola línea
mi_primera_funcion(x) = x+1

# Con varias líneas (forma recomendada)
function mi_tercera_funcion(x)
	x+1
end

# Con varias líneas (estilo R, no recomendado)
mi_segunda_funcion = function(x)
	x+1
end

mi_primera_funcion(1)
mi_segunda_funcion(1)
mi_tercera_funcion(1)

# También tenemos a nuestra disposición funciones anónimas
x -> x.^2			# Función anónima
f_anonima = (x) -> x.^2	# Función anónima con nombre
f_anonima(3)
f_anonima([3.0,2.0])

# Por defecto, el output es el último valor calculado en la función
# Se recomienda el uso de return para especificar el output
function funcion_sin_argumentos()
  output = "Hola!"
	return output
end
funcion_sin_argumentos()

# Podemos recibir varios argumentos y devolver varios argumentos como tuplas
function test_fun(x,y)
  z1 = x^2 + 2y
  z2 = x + y
	return z1, z2
end
test_fun(2,1)

# O como arreglos
function test_fun_array(x,y)
  z1 = x^2 + 2y
  z2 = x + y
	return [z1  z2]
end
test_fun_array(2,1)

# Podemos especificar argumentos opcionales
function test_fun_2(x, y=0)
  z1 = x^2 + 2y
  z2 = x + y
	return z1, z2
end
test_fun_2(2,1)  # Con y=1
test_fun_2(2)    # Con y=0 (valor por defecto)

# De forma anónima
f_anonima = (x,y) -> (x^2 + y, x+y) 
f_anonima(2,1)

# Se puede usar tuplas para asignar directamente
function f_otra_mas(a,b)
  a+b, a*b
end
x, y = f_otra_mas(2,3)
x
y

# Los argumentos deben ingresar en orden. Pero se puede usar "keywords" para
# especificar los que entran en cualquier orden
function otra_funcion(x, y; keyword=0)  # keyword tiene valor por defecto
  z = 2x + y + keyword
  return z
end
otra_funcion(1, 2; keyword=0)  # Funciona
otra_funcion(1, 2)             # También funciona (usa valor por defecto)
# otra_funcion(keyword=0, 1, 2) # Esto NO funcionaría

# La diferencia entre el argumento opcional y el keyword es que este último
# debe especificarse por nombre
function otra_funcion_mas(; opcion1=0, opcion2=0)
  return opcion1 + 2*opcion2
end
otra_funcion_mas(opcion1=2, opcion2=1)
otra_funcion_mas(opcion2=1, opcion1=2)  # Orden no importa
otra_funcion_mas()                      # Usa valores por defecto

# Podemos fijar de antemano el tipo de los argumentos
function test_fun_3(var1::Int64, var2::Int64=1; keyword::Int64=2)
	output1 = var1 + var2 + keyword
	return output1
end
# test_fun_3(2.0, 2)  # Esto causaría un error por el tipo
test_fun_3(2, 2)      # Esto funciona

# Y del output
function test_again(x, y)::Int64
    return round(Int64, x*y)  # Convertimos a Int64
end
test_again(1.2, 1.3)
test_again(1, 1)

# Broadcasting: Para evaluar la función en un vector, debemos usar la notación
# ".", sin necesidad de definir internamente la función (como en Matlab)
f(x) = x^2             # En Matlab sería x.^2
g(x, y) = x + 2 + y^2  # En Matlab sería x + 2 + y.^2 
x = 1:5
y = 1:5
f.(x)      # Aplica f a cada elemento de x
g.(x, y)   # Aplica g elemento por elemento

# Multiple dispatch: Julia puede definir múltiples métodos para la misma función
una_funcion(var1, var2) = var1 + var2 + 1
methods(una_funcion)

# Funciones de alto nivel: ¡Funciones que definen funciones!
function f_mama(var1)
	function f_hija(var2)
		nieta = var1 + var2
		return nieta
	end
	return f_hija
end
f_nieta_1 = f_mama(1)	# f_nieta_1: Produce 1+var2
f_nieta_2 = f_mama(2)	# f_nieta_2: Produce 2+var2
f_nieta_1(1)
f_nieta_2(1)


# Hemos visto ya la función map()
# Esta hace parte del paradigma de programación "mapreduce"
# https://es.wikipedia.org/wiki/MapReduce
A = [1, 2, 3, 4, 5]
map(x->3*x+2, A) 
reduce(+, [1,2,3])	                     # reduce genérico: suma todos
mapreduce(x->x*x, +, [1,2,3])            # 1² + 2² + 3² = 14
mapreduce(x->x^3, +, [1,2,3], init=9)    # (1³ + 2³ + 3³) + 9 = 45
mapreduce(x->-x, *, [8,9,10])            # (-8) * (-9) * (-10) = -720
