# Fundamentos de Julia: 
# Parte 4) Estructuras de Control de Flujo
#
# Angelo Gutierrez-Daza
# Programa de Estudios Superiores
# Banco de Guatemala
# 2025
#
# Probado usando Julia 1.12.0


################################################################################
# 1) Valores Lógicos
################################################################################

# Ya nos encontramos antes con los valores lógicos true y false
x = true
y = false

# En Julia, los operadores lógicos son funciones sin ningún misterio
# !	    # not
# &&	    # and
# ||	    # or
# ==	    # is equal?
# !=	    # is not equal?
# ===  	# is equal? (enforcing type: 2===2.0 is false)
# !==	    # is not equal? (enforcing type)
# >	    # bigger than
# >=	    # bigger or equal than
# <	    # less than
# <=	    # less or equal than
# ~	    # bitwise not
# &	    # bitwise and
# |	    # bitwise or
# xor	    # bitwise xor (also typed by \xor or \veebar + tab)

# Se pueden usar en conjunto, como es de esperarse
3 > 2 && 4<=8 || 7 < 7.1

# Incluso con strings
messi   = "goat"
ronaldo = "good"
messi    == "good"
messi    == "goat"
ronaldo != "good"  # Negación estándar
ronaldo  == "goat"

# Como son funciones, hay que usar "." para aplicarlas elemento por elemento
A = (2,2,2)
B = (1,9,9)
A .> B 

# La evaluación de condicionales es "perezosa": 
# Una vez se rompe una condición, no se evalúan las demás
2 > 3 && println("Maluma Baby")
2 > 1 && println("Maluma Baby")

# Existe además la opción de comparar lógicamente, incluyendo el tipo
messi_balonOro = 8      # Actualizado a 2025
ronaldo_balonOro = 5

messi_balonOro     == 8.0  # Igualdad
messi_balonOro    === 8.0  # Igualdad, incluyendo tipo de variable
ronaldo_balonOro  !== 8.0  # Negación, incluyendo tipo de variable

# Esta función nos permite comparar si dos valores son arbitrariamente cercanos
isapprox(1, 1.0001; atol = 0.1)
isapprox(1, 1.0001; atol = 1e-8)

# Julia permite además revisar pertenencia a un conjunto de forma compacta!
a = [1,2,3]
2 in a		
in(2,a)		
4 in a		

# Podemos usar valores lógicos para hacer extracción condicional
# Necesitamos importar Statistics para mean()
using Statistics
a = 1:10
sum(a.>4)  
mean(a.>4) 
a[a.>4]

# Pero es más eficiente usar la función filter()
a = 1:10
filter(isodd,a)	            # Impares
filter(iseven,a)            # Pares
filter(x->x>5,a)            # Mayores que 5
filter(x-> x>=4 && x<=6,a)  # Entre 4 y 6
filter(x->x%3==0,a)         # Múltiplos de 3

# Algunas otras funciones condicionales 
isa(1,Float64)
iseven(2)
isodd(2)
ispow2(4)
isfinite(1.0)
isinf(1.0)
isnan(1.0)

# Finalmente, Julia cuenta con el operador "ternario"
# Este operador es parecido a un "if-elseif-else" pero se usa cuando se requiere
# la elección condicional entre expresiones únicas, en lugar de la ejecución
# de pedazos de código más largos.
#
# Se llama ternario porque consiste en tres operandos:
# a ? b : c
# 
# La expresión "a", antes del "?", es una condición lógica
# El operador ternario evalúa la expresión "b" antes del ":"
# si la condición "a" es verdadera, o "c" si es falsa

# Es más fácil de entender con un ejemplo
x = 1; y = 2;
println(x < y ? "less than" : "not less than")
x = 1; y = 0;
println(x < y ? "less than" : "not less than")

# El espacio entre operadores es obligatorio: a?b:c no es válido
# println(x < y?"less than":"not less than") # ¡Esto causaría un error!

# Se pueden anidar
test(x,y) = println(x < y ? "x is less than y"    :
                    x > y ? "x is greater than y" : "x is equal to y")

test(1,2)
test(2,1)
test(1,1)

# Se puede usar para definir funciones de forma compacta!
fib(n) = n < 2 ? n : fib(n-1) + fib(n-2)
fib(2)
fib(3)
fib(9)

################################################################################
# 2) Condicionales
################################################################################

# Tampoco hay misterio acá: Funcionan como es de esperarse
# En VS Code, puedes usar puntos de interrupción (F9) para depurar condicionales

JBalvin  = 10
Maluma   = 8

if JBalvin < Maluma
    println("Maldad")
elseif JBalvin > Maluma
    println("Morado")
else
    println("Que Pena")
end

################################################################################
# 3) Bucles (Loops)
################################################################################

# También funcionan como es de esperarse, con una notación bastante sencilla
# En VS Code, puedes usar el depurador para inspeccionar variables en bucles

for i in 1:5
	println(i)
end

for i in 1:0.5:3
	println(i)
end

# A diferencia de otros lenguajes, Julia no conserva el iterador local "i"
# fuera del scope del bucle

# Podemos hacer loops como en Matlab también!
for i = 1:5
	println("Maluma Baby")
end

# Podemos además iterar sobre cualquier colección de elementos
a = (1, 2, 3)
for i in a
	println(i)
end

# ¡Y usar notación elegante!
for i ∈ 5:-1:0
	println(i)
end

# Podemos usar notación compacta para múltiples loops!
for i = 1:2, j = 3:4
	println((i, j))
end

# Y utilizar iteradores anidados
for i = 1:2, j = i:4
	println((i, j))	
end

# La instrucción "break" termina el loop
for i = 1:3
	println(i)
	if i == 2 
		break
	end
end

# Y la instrucción "continue" salta a la siguiente iteración
for i = 1:5
	if i == 3 
		continue
	end
	println(i)
end

# También tenemos a nuestra disposición el tradicional "while"
k = 0
while k <= 5
	println(k)
	global k += 1
end

# Note que el scope en los loops de Julia son locales (a diferencia de Matlab)
# En VS Code, puedes inspeccionar variables locales vs globales en el depurador
k = 0; y = 1;
while k <= 5
	y = 2  # Esta asignación es local al while
	global k += 1
end
y  # y sigue siendo 1

k = 0; y = 1;
while k <= 5
	global y = 2  # Esta asignación es global
	global k += 1
end
y  # Ahora y es 2

# Finalmente, note que en Julia podemos usar ¡comprensiones!
[n^2 for n in 1:5]

# Podemos fijar el tipo del output de una comprensión
Float64[n^2 for n in 1:5]	

# Y podemos usar varios iteradores
[x+y for x in 1:3, y = 1:4]

# Y como generadores para funciones:

# Esto 
suma1 = 0
for i = 1:1000
	global suma1 += (1/i)^2
end
suma1

# Es equivalente a esto
suma2 = sum(1/i^2 for i=1:1000)
suma2
