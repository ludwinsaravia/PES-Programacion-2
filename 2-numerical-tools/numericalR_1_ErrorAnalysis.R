# Introducción a metodos numericos en R - Parte 1:
# Error numerico
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
# 1) Error de maquina
################################################################################

# Es comun encontrarse con problemas en Economia y Estadistica que requieran 
# derivar una funcion objetivo para calcular sus condiciones de primer orden,
# optimizar dicha funcion objetivo o resolver el sistema de ecuaciones
# caracterizado por las condiciones de primer orden del problema de optimizacion

# Estos problemas pueden resolverse con lapiz y papel solo en casos especiales 
# bajo supuestos a veces heroicos y restricciones parametricas que no siempre 
# estan acordes con la evidencia empirica

# Con los computadores, podemos resolver problemas mas complejos y relevantes
# que podremos utilizar para responder preguntas de todo tipo, construir 
# pronosticos y elaborar escenarios contra-factuales

# Para esto, es necesario conocer las herramientas con las que R cuenta para 
# resolver el tipo de problemas que encontramos comunmente en Economia

# Antes de hacerlo, es importante entender que cuando llevamos un problema de
# estos al computador, solo pueden resolverse de forma APROXIMADA

# Para entenderlo, note que los computadores representan los numeros con una 
# precision limitada, y solo un subconjunto de los numeros reales se puede usar

# Por lo anterior, siempre existe un "error de aproximacion" 
# Una primera medida del error de aproximacion en que se puede incurrir es el
# "error de maquina", que es la diferencia entre el numero "1" y el siguiente
# numero mas cercano en el computador

# El siguiente comando de R nos permite para ver las caracteristicas de la
# representacion numerica en R, dado el computador en que se esta trabajando
.Machine 

# En particular, el elemento "double.eps" es el error de maquina
error_de_maquina = .Machine$double.eps

# El error de maquina es un numero muy pequeño, pero no es cero
print(error_de_maquina)

# El error de maquina es una cota inferior del error de aproximacion que
# podemos encontrar al resolver un problema numericamente

# Por definición, R no es capaz de distinguir entre un numero y otro que difiere
# en el orden de este error
1 + error_de_maquina/2 == 1
1 + error_de_maquina   == 1

# También es importante conocer el infinito de maquina, el cual es el numero
# positivo mas grande que puede representarse en el computador
.Machine$double.xmax  # Numero positivo mas grande

# Cuando tomamos numeros que son representable por la maquina, pero cuyo valor
# excede el máximo de maquina, decimos que se ha producido un "overflow", o 
# desbordamiento numérico
# Por ejemplo, considere el siguiente calculo
x = 1e+308
y = 1e+10
z = x * y  # Esto produce un overflow
print(z)   # R representa el resultado como "Inf"


# Igual de importante es conocer el minimo positivo representable por la maquina
.Machine$double.xmin  # Numero positivo mas pequeño

# Similarmente, existe un "underflow" o subdesbordamiento numérico, que ocurre
# cuando el valor absoluto de un numero es menor que el minimo representable
# Por ejemplo, considere el siguiente calculo
x = 1e-308
y = 1e-10
z = x * y  # Esto produce un underflow
print(z)   # R representa el resultado como "0"


.Machine$integer.max # Entero mas grande: suele ser 2^31 - 1

# El error de aproximacion de una funcion suele ser proporcional a el error de
# maquina, y potencialmente mas grande
# Por ejemplo, considere la funcion f(x) = x^2
f = function(x){x^2}
# El minimo de f(x) es x=0, y f(0)=0
# Si tratamos de encontrar el minimo de f(x) usando optimizacion numerica
# podemos usar el siguiente comando
optim_results <- optim(par=1, fn=f)

optim_results$par  # El valor de x que minimiza f(x)
optim_results$value # El valor minimo de f(x)

# El resultado es un numero muy cercano a cero, pero no exactamente cero
# Esto es porque el algoritmo de optimizacion no puede distinguir entre
# 0 y un numero muy cercano a cero, digamos 1e-10
# El error de aproximacion en este caso es del orden de 1e-10, que es mucho
# mayor que el error de maquina, que es del orden de 1e-16

# Por lo tanto, es bueno recordar que al usar una rutina de optimizacion
# numerica, podemos encontrar que el minimizador de f(x) = x^2 no es exactamente
# x=0, pero algo muy cercano. De forma importante, esta cercania es algo 
# que en muchas ocasiones podemos controlar
