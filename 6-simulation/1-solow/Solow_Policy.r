# Modelo de Solow: Función de Política
#
# Ángelo Gutiérrez Daza
# 2018
#
# Taller de Métodos de Simulación Dinámica
# Programa de Estudios Superiores
# Banco de Guatemala
#
# Código probado utilizando la versión 3.4.3 de R

###################################################################################################################################
###################################################################################################################################
#                                     1) Modelo de Solow: Función de Política
###################################################################################################################################
###################################################################################################################################

# Comencemos limpiando el escritorio
graphics.off(); rm(list=ls());

# Vamos a escribir una función que simule una senda de capital, producto y consumo en unidades efectivas, asumiendo que
# tanto la productividad como la oferta laboral crecen a tasas constantes "g" y "n"

solow_policy <- function(
  s      , # Tasa de ahorro de los hogares
  delta  , # Tasa de depreciación del capital
  alpha  , # Participación del capital en la producción
  n      , # Tasa de crecimiento de la población
  g      , # Tasa de crecimiento de la productividad
  N      , # Número de valores de capital que usaremos en las gráficas
  K.min  , # Valor mínimo desde donde graficaremos la función
  K.max    # Valor hasta donde graficaremos la función
  ){
  
  # Grid de capital sobre el cual evaluaremos la función
  k_t <- seq(K.min,K.max,len=N) 
  
  # Para graficar la función de política del consumo y la producción, no hace falta mucho
  y_t <- k_t^alpha
  c_t <- (1-s)*y_t

  # Ahora evaluamos la función usando la ley de movimiento del capital en unidades efectivas
  k_tp <- (s*(k_t^alpha)+(1-delta)*k_t)/(1+g+n)

  # Finalmente, encontremos la función de politica de la inversión
  i_t <- k_tp-(1-delta)*k_t

  ## Ahora grafiquemos

  # Capital
 x11();
  plot(k_t,k_tp,col="black",type="p",ylab="k(t+1)",xlab="k(t)",
       main="Función de política: k(t+1)",xlim=c(K.min,K.max))
  lines(k_t,y=k_tp,col="red")  # Añadamos líneas para hacerla más llamativa
  lines(k_t,y=k_t,col="black") # Añadamos línea de 45 grados

  # Inversión
 x11();
  plot(k_t,i_t,col="black",type="p",ylab="i(t)",xlab="k(t)",
       main="Función de política: i(t)",xlim=c(K.min,K.max))
  lines(k_t,i_t,col="red")  # Añadamos líneas para hacerla más llamativa

  # Consumo
 x11();
  plot(k_t,c_t,col="black",type="p",ylab="c(t)",xlab="k(t)",
       main="Función de política: c(t)",xlim=c(K.min,K.max))
  lines(k_t,c_t,col="red")  # Añadamos líneas para hacerla más llamativa

  # Producción
 x11();
  plot(k_t,y_t,col="black",type="p",ylab="y(t)",xlab="k(t)",
       main="Función de política: y(t)",xlim=c(K.min,K.max))
  lines(k_t,y_t,col="red")  # Añadamos líneas para hacerla más llamativa
  

  
}

# Ahora usemos nuestra función en unos valores de parámetros arbitrarios
s     = 0.2   # Tasa de ahorro de los hogares
delta = 0.1   # Tasa de depreciación del capital
alpha = 0.3   # Participación del capital en la producción
n     = 0.01  # Tasa de crecimiento de la población
g     = 0.01  # Tasa de crecimiento de la productividad
N     = 100   # Número de valores de capital que usaremos en las gráficas

# Primero calculemos el estado estacionario del modelo
k_ss <- (s/(g+n+delta))^(1/(1-alpha))

# Ahora definamos el dominio de la gráfica
K.min = 0.01   # Valor mínimo desde donde graficaremos la función
K.max = 2*k_ss # Valor hasta donde graficaremos la función

# Finalmente, usemos nuestra función para graficar la función de politica
solow_policy(s,delta,alpha,n,g,N,K.min,K.max)
