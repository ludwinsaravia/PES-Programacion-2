# Fundamentos de R - Parte 4:
# Gráficas usando instalacion base de R
#
# Angelo Gutierrez-Daza
# Programa de Estudios Superiores
# Banco de Guatemala
# 2025
#
# Probado usando R 4.5.1

################################################################################
# 1) Figuras Basicas: Comandos de Alto Nivel
################################################################################

# Los comandos graficos de R pueden ser de "alto nivel" o "bajo nivel"
# Los comandos de alto nivel implementan la figura inicial
# Los comandos de bajo nivel agregan puntos, lineas, texto y otros detalles

# A continuacion veremos algunos comandos de alto nivel

### Nube de puntos

# Primero, creemos algunos datos.
x <- seq(-3,3,length=200)
y <- cos(x)

# La funcion "plot" crea una nube de puntos
plot(x,y)

# Noten que la grafica aparecera en una de las pantallas de R-studio

# Suele ser conveniente tenerla como una ventana a parte
# Para esto, usamos el comando x11() antes de la grafica
x11()
plot(x,y)

# Nota: El comando para abrir una ventana depende del sistema operativo:
# - Windows: x11()
# - Linux: X11()
# - Mac: No tengo idea

# La funcion plot() tiene muchas opciones que permiten personalizar la grafica
# Podemos consultarlas en la ayuda de este comando
# Aqui, una pequena muestra de como agregar algunos detalles
x11(); plot(x,y,
                col="blue",
                type="l",
                xlab="Hora del Dia",ylab="Hambre",
                main="Una grafica mas") 

# Noten ademas como podemos partir un comando en varias lineas para hacer el
# codigo mas compacto y facil de leer

# Note ademas que plot(), con un solo argumento vectorial, grafica una serie a
# traves del tiempo
x11(); plot(rnorm(100),col="blue",type="l")

### Histograma 
x <- rnorm(1000)
x11(); hist(x) # Histograma sencillo
x11(); hist(x,breaks=100,col="midnightblue") # Histograma con opciones

### Barras
x <- c(5,5,3,1)
names(x) <- c("Cristiano","Messi","Cruiyff","Ronaldinho") 
x
x11(); barplot(x)

### Lineas de contorno
x <- seq(-2,2,length=200) # Valores eje x
y <- x                    # Valores eje y

# Valores de la funcion f(x,y)=-(x^2+y^2) para cada combinacion de puntos (x,y)
z <- outer(x,y,function(x,y) -(x^2+y^2)) 

# Lineas de contorno de la funcion f(x,y)=-(x^2+y^2)
x11(); contour(x,y,z,col="blue",lwd=3)

### Superficie en tres dimensiones
x <- seq(-2,2,length=50) # Valores eje x
y <- x                   # Valores eje y

# Valores de la funcion f(x,y)=-(x^2+y^2) para cada combinacion de puntos (x,y)
z <- outer(x,y,function(x,y) -(x^2+y^2)) 

# Grafica simple de la funcion
x11(); persp(x,y,z) 

# Pero podemos hacer que se vea mucho mejor
x11()
persp(x, y, z, 
	theta = 30, phi = 30,
	expand = 0.5, col = "lightblue",
    ltheta = 120, shade = 0.75, ticktype = "detailed",
    xlab = "x", ylab = "y", zlab = "f(x,y)")

################################################################################
# 2) Figuras Basicas: Comandos de Bajo Nivel
################################################################################

# Para usar comandos de bajo nivel, debemos primero construir una grafica con
# los alto de alto nivel

# Luego invocamos el comando de bajo nivel para anadir cosas

# Aca usamos el comando "lines" para agregar una linea adicional a la grafica
x <- seq(-2*pi,2*pi,length=200)   # Vector de valores entre -2*pi y *pi
y <- sin(x)                       # Funcion seno
z <- cos(x)                       # Funcion coseno

# Creamos una grafica de la funcion seno usando el comando plot()
x11(); plot(x,y,type="l")     

# Anadimos la grafica de la funcion coseno usando el comando lines()
lines(x,z,col="blue")             

# Otros comandos de bajo nivel que pueden ser utiles:
?points   # Agregar puntos
?lines    # Agregar lineas
?text     # Agregar texto
?abline   # Agregar lineas rectas
?polygon  # Agregar poligonos
?arrows   # Agregar flechas
?legend   # Agregar una leyenda a la figura

# Finalmente, unos comandos para cerrar todas las graficas en el escritorio
graphics.off()

################################################################################
# 3) Exportar Figuras
################################################################################

# Podemos exportar la figura como un archivo pdf, eps, jpg, png, entre otros

# A continuacion guardamos el mismo histograma en tres formatos distintos
x <- rlnorm(100)

# Guardar como .pdf
postscript("mi_figura.pdf")  
hist(x)
dev.off()

# Guardar como .eps
postscript("mi_figura.eps") 
hist(x)
dev.off()

# Guardar como .jpg
jpeg("mi_figura.jpg") 
hist(x)
dev.off()

# Guardar como .png
jpeg("mi_figura.jpg") 
hist(x)
dev.off()

# Para mas formatos y opciones adicionales, puede consultar la ayuda:
?png
