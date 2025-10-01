# Introducción a metodos numericos en R - Parte 6:
# Interpolación numerica
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
# 1) Interpolacion en una dimension
################################################################################

# La interpolacion es el proceso de estimar valores intermedios entre un
# conjunto de puntos conocidos. Es util en muchas aplicaciones, como la
# visualizacion de datos, la solucion de ecuaciones diferenciales, y el
# ajuste de curvas.

# R tiene varias funciones para realizar interpolacion. La mas basica es
# approx(), que realiza interpolacion lineal entre puntos conocidos
x <- c(1,2,3,4,5) # Puntos conocidos en x
y <- c(2,3,5,7,11) # Puntos conocidos en y
interp_lineal <- approx(x,y,xout=seq(1,5,by=0.1)) # Interpolacion lineal
plot(x,y,col='red',pch=19,main='Interpolacion Lineal',xlab='x',ylab='y')
lines(interp_lineal,col='blue')

# Podemos ver que la interpolacion lineal conecta los puntos conocidos con
# lineas rectas. Esto puede ser suficiente para algunos casos, pero en otros
# puede ser deseable una interpolacion mas suave.

# La funcion spline() realiza interpolacion spline, que es una forma de
# interpolacion polinomial por partes que garantiza continuidad y suavidad
interp_spline <- spline(x,y,xout=seq(1,5,by=0.1)) # Interpolacion spline
lines(interp_spline,col='green')
legend('topleft',legend=c('Puntos conocidos','Interpolacion Lineal',
       'Interpolacion Spline'),col=c('red','blue','green'),pch=c(19,NA,NA),
       lty=c(NA,1,1))

# La interpolacion spline es mucho mas suave que la lineal, y puede ser
# mas adecuada para representar datos continuos. Sin embargo, puede ser
# mas propensa a oscilaciones no deseadas, especialmente en los extremos
# del intervalo de interpolacion. Por lo tanto, es importante elegir el
# metodo de interpolacion adecuado segun la naturaleza de los datos y
# el problema a resolver.

# Otra metodo de interpolacion es la interpolacion polinomial, que ajusta un
# polinomio de grado n a los n+1 puntos conocidos. Esto puede ser util
# para datos que siguen una tendencia polinomial, pero puede ser propenso
# a oscilaciones no deseadas (fenomeno de Runge) si el grado del polinomio
# es demasiado alto. La funcion poly() puede ser utilizada para generar
# un modelo polinomial en R.    
modelo_polinomial <- lm(y ~ poly(x, 4, raw=TRUE)) # Ajuste polinomial de grado 4
x_seq <- seq(1,5,by=0.1)
y_pred <- predict(modelo_polinomial, newdata=data.frame(x=x_seq))

# Noten que estamos utilizando una regresion lineal para ajustar el polinomio,
# ya que un polinomio es lineal en sus coeficientes.

# Finalmente, podemos graficar el ajuste polinomial junto con los otros
# metodos de interpolacion
plot(x,y,col='red',pch=19,main='Interpolacion Polinomial',xlab='x',ylab='y')
lines(interp_lineal,col='blue')
lines(interp_spline,col='green')
lines(x_seq,y_pred,col='purple')
legend('topleft',legend=c('Puntos conocidos','Interpolacion Lineal',
       'Interpolacion Spline','Ajuste Polinomial'),col=c('red','blue','green','purple'),
       pch=c(19,NA,NA,NA),lty=c(NA,1,1,1))


