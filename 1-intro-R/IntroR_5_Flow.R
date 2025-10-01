# Fundamentos de R - Parte 5:
# Estructuras de Control
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
# 1) Valores Logicos
################################################################################

# Ya nos encontramos antes a los valores logicos TRUE y FALSE
x <- TRUE
y <- FALSE

# Ahora veremos como usarlos para escribir funciones mas elaboradas

### Operadores Logicos

# Algunas expresiones en R dan como resultado un TRUE o un FALSE

## Mayor / Mayor o igual / Menor o igual / Menor
2 > 3       # Preguntamos a R si 2 es mayor que 3
X <- (3>=2) # Guardamos en X la respuesta a la pregunta: es 3 mayor o igual a 2?
X
Messi <- 10     
Cristiano <- 7 
Messi > Cristiano # Aca preguntamos a R lo obvio 

# Note que ">" y ">=" actuan como operadores logicos
# Estos operadores permiten verificar que una condicion logica se cumple

## Igualdad
# Aca usamos el operador "==" para verificar igualdad
2==3  
# Note que "=" y "==" no son lo mismo!
James = "Grande"   # Aca asignamos una variable que dice que James es Grande
James == "Grande"  # Aca preguntamos si James es Grande

## Desigualdad
# Para testear desigualdad, usamos "!=" o "!" como negacion
2!=3
!(2==3)  

## Operadores logicos "y" (AND) y o (OR)
# En R, AND y OR son representados por "&" y "|", respectivamente
# Dados dos enunciados "P" y "Q", se tiene:
# P & Q : Verdadero si ambos son verdaderos, Falso en cualquier otro caso
# P | Q : Falso si ambos son falsos, Verdadero en cualquier otro caso
(1<2) & (2<3) # AND: Ambos enunciados son verdaderos -> Resultado es TRUE
(1<2) | (2<1) # OR : Uno de los enunciados es verdadero -> Resultado es TRUE
(1<2) & (2<1) # AND: Uno de los enunciados es falso -> Resultado es FALSE

### Operadores Logicos sobre Vectores
# Los operadores logicos pueden ser aplicados a vectores
x <- c(1,2); 
y <- c(0,1);
x > y            # Note que x[1] es comparado con y[1] y x[2] con y[2]

# Podemos comparar un vector con un escalar, en cuyo caso, cada elemento del 
# vector se compara con el escalar
x > 1            

# Tambien podemos usar los operadores & y | con vectores 
(1<=x) & (x<=2)  

# Note ademas que 1<=x<=2 no funciona

### Aritmetica con Valores Logicos

# Los valores logicos se pueden usar con operaciones algebraicas
# En este caso, TRUE se toma como 1 y FALSE como 0
FALSE + TRUE 
FALSE*TRUE
sum(c(TRUE,TRUE,FALSE))

# Esto puede ser bastante util

# Por ejemplo, podemos usarlos para encontrar cuandos elementos de un vector
# exceden un numero determinado
x <- c(1,2,3,4,5,6,7,8,9)
sum(x>4)  

# O para encontrar la fraccion de elementos que exceden un numero determinado
mean(x>4) 

### Extraccion Condicional
# Podemos usar vectores de valores logicos para indexar vectores numericos
y <- c(1,2,3,4); 
y 
indices.elegidos <- c(T,F,T,F) # Vector donde TRUE seran los indices recuperados
y[indices.elegidos]            # Devuelve los elementos de los indices elegidos

# Usandolos de forma creativa, podemos ahorrar mucho tiempo
y[y>=3] # Recuperar los elementos del vector "y" que son mayores o iguales a 3

# Ejemplo: Creemos una data frame
Equipo <- c("RM","FCB","RM","FCB","FCB","RM","RM") # Par de equipos cualquiera
Millones.USD <- c(21,26,8,9,5,7,9) # Salario de jugadores en millones de USD

# Salario promedio de los jugadores del equipo RM
Salarios <- data.frame(Equipo,Millones.USD) 
mean(Salarios$Millones.USD[Salarios$Equipo=="RM"]) 

################################################################################
# 2) Condicionales
################################################################################

### Condicional IF-THEN-ELSE ###
# Podemos usar valores logicos junto con instrucciones conocidas como 
# "condicionales" para ejecturar una instruccion cuando se cumplan condiciones 

# Uno de los condicionales mas comunes es el condicional IF
# Este ejecuta un comando en caso de que una condicion se cumpla
# La mejor manera de verlo es con un ejemplo: 

# Aca tiramos una moneda y nos preguntamos si cae cara 
# De ser asi, creamos una variable llamada "cara" y le asignamos un 1
moneda <- runif(1)
cara   <- NaN
if  (moneda <=0.5){
    cara <-  1
}
cara 

# Tambien podemos especificar que hacer si la condicion no se cumple
# A continuacion, especificamos lo que ocurre en caso de que caiga sello
moneda <- runif(1)
if  (moneda <=0.5){
    cara <-  1
    print("Cara")
} else{
	cara <- 0
	print("Sello")
}
cara

# De forma alternativa, podemos usar el comando ifelse()
cara <- ifelse(moneda<=0.5, cara <- 1, cara <- 0)
cara

# Note que el primer argumento entre parentesis es evaluado
# Si este es verdadero, se devuelve el segundo valor
# De lo contrario, se devuelve el tercero
# Note ademas que el primer argumento puede ser un vector

# Retomemos como ejemplo los salarios de los jugadores
# Queremos crear una variable dummy que tome valor de 1 para los jugadores que 
# ganen mas de 10 millones de USD
Millones.USD
dummy.cracks <- ifelse(Millones.USD > 10,1,0)
dummy.cracks

### El simbolo Inf (Infinito)
# Finalmente, es bueno saber que R cuenta con el simbolo "Inf" que representa el
# infinito y tiene sus propiedades como tal
Inf
10^100>Inf
-Inf<0
for (i in 1:10){ # Repita la siguiente instruccion variando "i" entre 1 hasta 10
    X[i] <- i  # Asigne el numero "i" al elemento "i" del vector
}
# Asi como sus propiedades aritmeticas
Inf + Inf
Inf - Inf
1   + Inf
10/Inf

################################################################################
# 3) Loops
################################################################################

# Los loops son instrucciones que nos permiten repetir un comando un numero 
# (determinado o no) de veces

# Aca veremos dos tipos de loops comunmente utilizados en R:
# "For Loops" y "While Loops"

### For-Loop 
# Este tipo de loop repite una instruccion un numero determinado de veces
# La mejor forma de verlo es con un ejemplo

# Suponga que no conoce la funcion seq() o el uso de ":" pero quiere crear
# un vector con los numeros entre 1 y 10
X <- rep(NaN,3) # Iniciamos creando un vector sin numeros
for (i in seq(1,10,2)){ # Repita la siguiente instruccion variando "i" entre 1 hasta 10
	X[i] <- i  # Asigne el numero "i" al elemento "i" del vector
}
X

# Como otro ejemplo, suponga que quiere sumar los numeros enteros de 1 a 10
x <- 0 # Inicializamos un contador
for (i in 1:50){ 
	x <- x + i
}
x

# Note lo que hacen estas lineas:
# - Primero, x se inicia en cero
# - Luego, "i" se incrementa en cada iteracion por cada elemento en 1:10
# - En cada iteracion se repite el calculo x <- x + i

# Este ultimo Loop podria ser reemplazado por las siguientes lineas:
x <- 0
i <- 1
x <- x+i
i <- 2
x <- x+i
i <- 3
x <- x+i
x

# Ahora imagine que quiere sumar los numeros entre 1 y 100
# Claramente, los Loops son una mejor opcion

# Note que, de forma alternativa, podriamos haber escrito 
sum(1:10)
# para realizar esta operacion

# Si bien, ambas formas arrojan resultados iguales, esta ultima es la mas
#  eficiente (menor tiempo computacional)

# Como tercer ejemplo, vamos a simular una variable aleatoria para contar el
# numero de caras obtenidas al lanzar una moneda balanceada mil veces
caras <- vector(mode="numeric", length=1000) 
for (i in 1:1000) {
	moneda <- runif(1)
	caras[i] <- ifelse(moneda<=0.5,1,0)
}
sum(caras)  # Numero de caras
mean(caras) # Probabilidad de sacar cara

# En programacion, simpre habra mas de una forma de hacer las cosas
# Algunas formas seran mas faciles o eficientes, dependiendo del objetivo

# 1) Combinar la funcion sum() con la funcion runif() y usar operaciones logicas
sum(runif(1000)<=0.5)            

# 2) Usar la funcion que permite simular variables aleatorias de una binomial   
sum(rbinom(1,size=10000,prob=0.8))

# Tambien podemos usar el "For Loop" para crear categorias
# Por ejemplo, estos son los puntos de los equipos en la tabla de la 
# Premier League (al 18 de Febrero de 2018)
puntos <- c(44,43,43,39,35,34,32,31,31,30,29,29,27,25,24,23,21,19,18,12)

# Noten que el primer lugar le lleva casi 20 puntos al cuarto!

# Queremos crear un vector "Clasificacion" que los separe en tres categorias: 
# Lideres ("L"), Media Tabla ("M"), y Descenso ("D")
clasificacion <- character(0) # Vector de caracteres vacio.
for (i in 1:length(puntos)){
	if (puntos[i]<=25) clasificacion[i] <- "D"
	if (puntos[i]>25 && puntos[i]<=38) clasificacion[i] <- "M"
	if (puntos[i]>38) clasificacion[i] <- "L"
}
clasificacion

### While-Loop
# Los While loops actuan de forma similar al For Loop
# La diferencia fundamental es que estos repiten una instruccion mientras una
# condicion logica se mantenga, por lo que el numero de iteraciones puede no 
# conocerse de antemano

# Ejemplo: Simular una v.a. contando el numero de lanzamientos de una moneda
# hasta que cae la primera cara
lanzamientos <- 0
cara         <- 0

while (cara==0){
	lanzamientos <- lanzamientos + 1 # Cuente el numero de lanzamientos
	moneda <- runif(1)               # Lanzar una moneda
	if (moneda <= 0.5) cara <-1      # Verificar si cae cara
}

# Este loop continua hasta que el condicional "cara==0" arroje un FALSE. 
# Note que, en este caso, el For-Loop no es apropiado

################################################################################
# 4) Funciones + Loops
################################################################################

# Uno de los grandes usos de los loops es la simulacion montecarlo

# Como ejemplo vamos a usar un loop para calcular el numero de juegos en 
# promedio que puede jugar Falcao antes de lesionarse a partir de simulaciones

# Suponemos que la probabilidad de que Falcao se lesione en un juego es del 10%
# Repetimos el experimento 1000 veces y promediamos para obtener una 
# aproximacion al valor esperado de juegos antes de lesionarse

num.simulaciones <- 1000
num.partidos     <- rep(0,num.simulaciones)

for (i in 1:num.simulaciones){
	partidos  <- 0 # Comencemos a contar partidos jugados
	lesion    <- 0 # Y empezemos la temporada sin lesiones
	while (lesion==0){ # Mientras no se lesione
		partidos <- partidos + 1        # Cuente el numero de partidos
		moneda   <- runif(1)            # Lance una moneda
		if (moneda <= 0.1) lesion <- 1  # Verifique si Falcao se lesiona
	}
    # Guarda el numero de partidos que Falcao jugo antes de lesionarse
	num.partidos[i] <- partidos 
}

# Promedio de partidos seguidos que juega Falcao antes de lesionarse
print(mean(num.partidos)) 


# Si combinamos el uso de loops con funciones, podemos definir funciones mucho
# mas elaboradas que las definidas hasta ahora

# A contiuacion, una funcion para calcular el numero de partidos que juega 
# Falcao antes de lesionarse, dada una probabilidad incondicional de lesion

Partidos.Falcao <- function(p_lesion) {
    num.simulaciones <- 10000
    num.partidos     <- rep(0,num.simulaciones)

    # Repetir el experimento mil veces
    for (i in 1:num.simulaciones){
        partidos  <- 0
        lesion    <- 0
        while (lesion == 0){
                partidos <- partidos + 1         
                moneda   <- runif(1)             
                if (moneda <= p_lesion) lesion <- 1
        }
        num.partidos[i] <- partidos
    }
    print(mean(num.partidos))
}

# Note que los brackets delimitan el cuerpo de la funcion

# Ahora podemos usar la funcion para repetir el calculo con otras probabilidades
Partidos.Falcao(0.1)
Partidos.Falcao(0.5)

# Y usar otro loop para evaluarla en varios puntos y hacer una grafica

pVector <- seq(0.1,0.9,length=50)
partidosVector  <- rep(0,50)
for (i in 1 : length(pVector)){
    partidosVector[i] <- Partidos.Falcao(pVector[i])
}

plot(pVector,partidosVector)
