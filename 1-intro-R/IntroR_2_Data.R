# Fundamentos de R - Parte 2:
# Manipulación de Datos
#
# Angelo Gutierrez-Daza
# Programa de Estudios Superiores
# Banco de Guatemala
# 2025
#
# Probado usando R 4.5.1

################################################################################
# 1) Variables y Matrices
################################################################################

# Limpiemos el workspace para comenzar
rm(list=ls()) 


# Los comandos se separan por un ";" o por una nueva linea
# Si un comando queda incompleto al ingresarlo, la consola mostrara un "+"

### Asignacion de variables escalares ###
a <- 1    # Crea la variable "a" cuyo valor es "1"
a         # Cuando usemos el simbolo "a", R recupera y usa el valor asignado
b = 1     # Tambien podemos utilizar = en lugar de <- para asignar variables
c <- 1+1  # Podemos utilizar R como una calculadora
d <- a+b  # Podemos hacer operaciones aritmeticas con las var en el workspace
d <- d+1  # Podemos sumar de forma iterativa una variable
d         # Vemos que el nuevo valor reemplaza el anterior en el workspace

# Tambien podemos asignar el resultado de una funcion 
e <- runif(1) # Generamos una variable aleatoria uniforme y la llamamos "e" 

# Noten que R es sensible a mayusculas y minusculas
a  # Nos muestra el valor de la variable "a"
A  # Nos arroja un error indicando que no existe esta variable en la memoria

# R exige que el nombre de una variable empiece por una letra
# El nombre puede contener "." pero no espacios
# Es buena practica asignar nombres a las variables que sean descriptivos y 
# faciles de recordar
numero.balones.oro.messi   <- 5
numero_balones_oro_ronaldo <- 5

# Limpiemos ahora el workspace
rm(list=ls()) 
ls()

### Crear vectores ###
A  <- cbind(1,2)   # Vector de dimension 1x2
B  <- rbind(1,2)   # Vector de dimension 2x1
C  <- c(1,2)       # Comando para concatenar de forma horizontal
D  <- c(A,C)       # Tambien se puede usar para concatenar vectores

### Crear Matrices ###
AA <- matrix(c(1,2),nrow=2,ncol=1)         # Matriz 2x1
BB <- matrix(c(1,2),nrow=1,ncol=2)         # Matriz 1x2
CC <- matrix(c(1,2,3,4),ncol=2,nrow=2)     # Matriz 2x2 ordenada por columnas
DD <- matrix(c(1,2,3,4),ncol=2,byrow=TRUE) # Matriz 2x2 ordenada por filas

### Manipular Matrices ###
FF <- DD                  # Crear una matriz duplicado
FF                        # Ver en el command window
FF[1,1] <- 10             # Asignacion de elementos de la matriz
FF                        # Ver en el command window
FF[1,2]                   # Seleccion del elemento en la posicion (1,2)
GG <- cbind(FF,c(5,6))    # Contatenar vector columna a una matriz
GG                        # Ver en el command window
GG <- rbind(GG,c(7,8,9))  # Contatenar vector fila a una matriz
GG                        # Ver en el command window
GG[,1]                    # Seleccionar todos los elementos de la 1er columna
GG[1,1:2]                 # Elementos 1 hasta 2 de la primera fila
GG[-1,2]                  # Elementos de la 2da columna, excepto el primero
GG <- GG[1:2,]            # Reasignacion de datos

################################################################################
# 2) Tipos de Datos 
################################################################################

# R necesita trabajar con numeros, texto, valores logicos, etc 
# A esto se le conoce como el "tipo" de la variables
# A continuacion vemos algunos de los tipos mas comunmente utilizados

### Tipo 1: Numerico (Numeric) 
a <- c(3,4)  # El mas sencillo de todos, utilizado para "floating point numbers"
mode(a)      # La funcion mode() se refiere al tipo primitivo del dato
class(a)     # La funcion class() es un poco mas especifica
             # La diferencia la notaremos cuando veamos listas y dataframes

### Tipo 2: Texto (String)
b <- "FC"    # En R, los textos van entre doble comilla
mode(b)      # Los textos tienen modo y clase "character"
class(b)     # Tambien tienen clase "character"

# Podemos usar la funcion "paste" para concatenar texto y usarlo con funciones
paste("FC", "Barcelona")        # El mejor 
paste("FC", "Barcelona",sep="") # Separe usando un string vacio
hora_inicio <- paste("Hoy es", date())         # Usar junto a otras funciones

# Note la importancia de distinguir entre texto y variable
hist(rnorm(100),col="midnightblue") # Esta linea corre
hist(rnorm(100),col=midnightblue)   # Esta no

### Tipo 3: Valores Logicos (Booleans)

# Para los valores logicos verdadero (True) y Falso (False)
x <- TRUE
y <- FALSE
mode(x)   
class(x)

# Como otros tipos primitivos de datos, podemos concatenar valores logicos
X <- c(TRUE,TRUE,FALSE)
mode(X)

# Tambien podemos abreviar
X <- c(T,T,F)
mode(X)

# Para verificar el tipo de una variable, podemos utilizar la funcion is.numeric():
x <- c("100","200")  # Creemos un vector de "strings"

# La funcion is.numeric() verifica si es verdad que x es de tipo "numeric"
is.numeric(x)   

# Como es texto, no podemos utilizar funciones como "sum"
sum(x)

# Podemos convertir el tipo de la variable a numerico usando as.numeric()
x <- as.numeric(x)   # Convertir a tipo "numeric"
is.numeric(x)        # Revisemos que ahora si es de el tipo deseado
sum(x)               # Ahora si podemos usar la funcion sum()

# Finalmente, note que al concatenar datos de tipo "string" y "numeric", estos
# ultimos se transforman en "strings"
x <- c("Futbol",1)
mode(x)
x   

# ¿Como podemos entonces almacenar datos de diferentes tipos en un mismo objeto?
# Para hacerlo, debemos utilizar "listas": 
FIFA <- list(jugador="Messi",balones_de_oro="5")

# Los elementos de esta lista tienen "jugador" y "balones_de_oro"
# Para acceder a los elementos, utilizamos el simbolo $:
FIFA$jugador
    
FIFA$balones_de_oro

#Podemos ademas utilizar la funcion names() para conocer los campos de la lista
names(FIFA)

# Las listas son un objeto muy importante
# Gran numero de funciones de R devuelven listas como output

################################################################################
# 3) Trabajar con Datos
################################################################################

# Necesitamos saber leer datos, guardarlos, seleccionar subconjuntos de estos,
# cambiarlos, etc 

# En R, la manipulacion de datos gira en torno a "data frames"

# Los dataframes son una especie de lista que guarda "columnas" de datos 
# relacionados. Por lo general, cada columna corresponde a una variable pero 
# cada columna puede contener un tipo diferente de dato

# Para ilustrar su uso, utilizaremos una correlacion interesante que hay entre 
# el numero de peliculas en que ha aparecido Nicolas Cage cada 12 meses con el 
# numero de editoras mujeres de la revista "Harvard Law Review"

# Primero, organizamos los vectores de datos

# No. de peliculas en que aparecio Nicolas Cage entre 2005 y 2009
NC <- c(2,3,4,1,4)     

# No. de muejeres editoras en "Harvard Law Review" entre 2005 y 2009
ME <- c(9,14,19,12,19) 

# Ahora construimos un data frame.
Correlacion.Espurea <- data.frame(NC,ME)

# Podemos ver que los data frames son una clase particular de lista
mode(Correlacion.Espurea)  # El data frame tiene modo "list"
class(Correlacion.Espurea) # y clase "data.frame"

# Veamos como luce un data frame
Correlacion.Espurea 

# R da un numero a las filas y usa el nombre de la variable como columna

# Podemos dar nombres mas descriptivos a las columnas como sigue:
Correlacion.Espurea <- data.frame(nicolas.cage=NC,mujeres.editoras=ME)
Correlacion.Espurea

# Tambien podemos reemplazar los numeros de fila por letras o otros numeros
# La funcion row.names() permite saber el nombre de las filas
row.names(Correlacion.Espurea) 

# Creemos un vector de textos para reemplazar el nombre de las filas
fechas <- c("Year 2005","Year 2006","Year 2007","Year 2008","Year 2009")

# La funcion row.names() tambien permite modificar el nombre de las filas
row.names(Correlacion.Espurea) <- fechas 
row.names(Correlacion.Espurea)
Correlacion.Espurea

# Un data frame es una lista: podemos acceder a las columnas con el simbolo "$"
Correlacion.Espurea$nicolas.cage
Correlacion.Espurea$mujeres.editoras

# Sin embargo, los nombres no funcionaran directamente
nicolas.cage

# Para poder usar las columnas existentes en un data frame como si fueran 
# variables, podemos usar el comando attach()
attach(Correlacion.Espurea)
nicolas.cage

# Tambien podemos acceder a las variables de un dataframe usando la misma 
# notacion que para matrices
Correlacion.Espurea[1,2]  # Primera fila, segunda columna
Correlacion.Espurea[,2]   # Toda la segunda columna
Correlacion.Espurea[2,]   # Toda la segunda fila

# Muchas funciones de R pueden interactuar directamente con data frames

# La funcion summary() arroja estadisticas descriptivas de los datos
summary(Correlacion.Espurea)          

# Por su parte, la funcion plot() tambien recibe data frames
x11(); plot(Correlacion.Espurea)  

# Finalmente, podemos usar lm() para correr una regresion usando el data frame
# y anadimos una linea a la grafica de los datos usando la funcion abline()
mi.regresion <- lm(mujeres.editoras ~ nicolas.cage, data=Correlacion.Espurea)
abline(mi.regresion)

# Note que la funcion lm devuelve una lista como output
mi.regresion

# Podemos usar summary() sobre esta lista para obtener una tabla de resultados
summary(mi.regresion)

################################################################################
# 5) Cargar Datos Desde Archivos Externos
################################################################################

### Directorio de Trabajo ###
# Antes de cargar datos desde un archivo externo, debemos asegurarnos que el 
# directorio de trabajo de R sea el mismo donde se encuentran el archivo
# Con el comando setwd() cambiamos el directorio donde nos encontramos ubicados
# setwd("C:/Users/USUARIO/Desktop/")    

# En el directorio hay varios archivos con una serie de tiempo del PIB anual de
# Guatemala en USD de 2010, en diferentes formatos

# Por ahora, vamos a enfocarnos en archiovos CSV y texto plano

### Archivo de texto
datos.desde.texto <- read.table(".input/PIB.txt",header=TRUE)
datos.desde.texto
x11(); plot(datos.desde.texto)

# Podemos llamar archivos que no esten en el directorio usando la ruta completa
# datos.desde.texto <- read.table("C:/Users/USUARIO/CARPETA/PIB.txt",header=TRUE)
# datos.desde.texto
# x11(); plot(datos.desde.texto)

### Archivo CSV (Comma-Separated Values)
datos.desde.csv <- read.csv("./PIB.csv")
datos.desde.csv
x11(); plot(datos.desde.csv)

# Note que tambien podemos usar la interface de Rstudio para importar datos 
