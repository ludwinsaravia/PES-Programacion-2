# Modelo de Solow: Simulación
#
# Ángelo Gutiérrez Daza
# 2018
#
# Taller de Métodos de Simulación Dinámica
# Programa de Estudios Superiores
# Banco de Guatemala
#
# Código probado utilizando la versión 3.2.2 de R

###################################################################################################################################
###################################################################################################################################
#                                    2) Modelo de Solow: Simulación
###################################################################################################################################
###################################################################################################################################

# Comencemos limpiando el escritorio
graphics.off(); rm(list=ls());

# Vamos a escribir una función que simule una senda de capital, producto y consumo,
# dados unos valores de los parámetros y unas sendas para la productividad y la población.
# Note que simularemos el modelo en niveles (no en términos per-cápita o unidades efectivas de trabajo).

solow_sim <- function(s,delta,alpha,T,K0,A,L){
  
  # Preasignemos memoria 
  K <- vector(mode="numeric",length=T)
  Y <- vector(mode="numeric",length=T)
  C <- vector(mode="numeric",length=T)
  I <- vector(mode="numeric",length=T)
  
  # Definimos el nivel inicial del capital 
  K[1]<-K0
  
  # Simulamos las sendas de capital, consumo, inversión y producto
  for(t in 1:T){
    Y[t]   <- (K[t]^alpha)*((A[t]*L[t])^(1-alpha))
    I[t]   <- s*Y[t]
    C[t]   <- (1-s)*Y[t]
    K[t+1] <- I[t]+(1-delta)*K[t]
  }
  
  #Elimino último dato de capital que sobra
  K <- K[1:T]
  
  # Guardamos los resultados en una lista
  Simulacion <- list(K_sim=K, Y_sim=Y, C_sim=C, I_sim=I)
  
  # Definimos el output de la función
  return(Simulacion)
  
}

# Ahora vamos a simular el modelo para distintos valores de los parámetros

## Simulación 1: Productividad y Trabajo Constnates
# En esta simulación, nos abstraemos de la productividad y el crecimiento poblacional

# Definición de parámetros
T       <- 100         # Número de observaciones a simular
s       <- 0.2         # Tasa de ahorro
delta   <- 0.1         # Tasa de depreciación del capital
alpha   <- 0.3         # Participación del capital en el producto


######################## Simulación 1: At=Lt=1 ####################################

# Procesos de productividad y oferta laboral
A <- rep(1,T)  # Productividad constante y normalizada a 1.
L <- rep(1,T)  # Población constante y normalizado a 1.

# Calculemos el estado estacionario del modelo
Kss = (s/delta)^(1/(1-alpha)) # Capital de estado ss
Yss = (Kss^alpha)             # Producción de ss
Iss = delta*Kss               # Inversión de ss
Css = Yss-Iss                 # Consumo de ss

# Ahora asignemos el valor inicial a partir del capital de estado estacionario en este caso
K0  = 0.9*Kss  # Stock de capital menor al de estado estacionario

# Ahora simulamos las sendas del modelo
Simulacion.1 <- solow_sim(s,delta,alpha,T,K0,A,L)

# Recuperamos las sendas graficadas
K_sim <- Simulacion.1$K_sim
C_sim <- Simulacion.1$C_sim
I_sim <- Simulacion.1$I_sim
Y_sim <- Simulacion.1$Y_sim

## Ahora grafiquemos

# Capital
x11();
plot(K_sim,col="black",type="p",ylab="K(t)",xlab="t",main="Simulación: K(t)")
lines(K_sim,col="red")        # Añadamos líneas para hacerla más llamativa
points(rep(Kss,T),col="black") # Añadamos una línea para el SS

# Inversión
x11();
plot(I_sim,col="black",type="p",ylab="I(t)",xlab="t",main="Simulación: I(t)")
lines(I_sim,col="red")         # Añadamos líneas para hacerla más llamativa
points(rep(Iss,T),col="black") # Añadamos una línea para el SS

# Consumo
x11();
plot(C_sim,col="black",type="p",ylab="C(t)",xlab="t",main="Simulación: C(t)")
lines(C_sim,col="red")        # Añadamos líneas para hacerla más llamativa
points(rep(Css,T),col="black") # Añadamos una línea para el SS

# Producción
x11();
plot(Y_sim,col="black",type="p",ylab="Y(t)",xlab="t",main="Simulación: Y(t)")
lines(Y_sim,col="red")        # Añadamos líneas para hacerla más llamativa
points(rep(Yss,T),col="black") # Añadamos una línea para el SS


################# Simulación 2: At y Lt creciendo a g y n  ####################

# Productividad
A    <- vector(mode="numeric",length=T)
g    <- 0.01  # Tasa de crecimiento de la productividad
A[1] <- 1     # Valor inicial de la productividad

for (t in 2:T){ 
  A[t] = (1+g)*A[t-1] 
}

# Oferta Laboral
L    <- vector(mode="numeric",length=T)
n    <- 0.01  # Tasa de crecimiento de la oferta laboral
L[1] <- 1     # Valor inicial de la oferta laboral

for (t in 2:T){ 
  L[t] = (1+n)*L[t-1] 
}

# Iniciamos en el mismo punto de la última simulación
K0  = 0.9*Kss  # Stock de capital menor al de estado estacionario

# Ahora simulamos las sendas del modelo
Simulacion.2 <- solow_sim(s,delta,alpha,T,K0,A,L)

# Recuperamos las sendas graficadas
K_sim <- Simulacion.2$K_sim
C_sim <- Simulacion.2$C_sim
I_sim <- Simulacion.2$I_sim
Y_sim <- Simulacion.2$Y_sim

## Ahora grafiquemos los valores en niveles

# Capital
x11();
plot(K_sim,col="black",type="p",ylab="K(t)",xlab="t",main="Simulación: K(t)")
lines(K_sim,col="red")        # Añadamos líneas para hacerla más llamativa

# Inversión
x11();
plot(I_sim,col="black",type="p",ylab="I(t)",xlab="t",main="Simulación: I(t)")
lines(I_sim,col="red")         # Añadamos líneas para hacerla más llamativa

# Consumo
x11();
plot(C_sim,col="black",type="p",ylab="C(t)",xlab="t",main="Simulación: C(t)")
lines(C_sim,col="red")        # Añadamos líneas para hacerla más llamativa

# Producción
x11();
plot(Y_sim,col="black",type="p",ylab="Y(t)",xlab="t",main="Simulación: Y(t)")
lines(Y_sim,col="red")        # Añadamos líneas para hacerla más llamativa


# Grafiquemos el capital en unidades efectivas 
k_sim <- K_sim/(A*L) # Unidades efectivas de capital
kss   <- (s/(delta+n+g))^(1/(1-alpha)) # Capital de estado ss en este caso

# Capital
x11();
plot(k_sim,col="black",type="p",ylab="k(t)",xlab="t",main="Simulación: k(t)")
lines(k_sim,col="red")         # Añadamos líneas para hacerla más llamativa
points(rep(kss,T),col="black") # Añadamos una línea para el SS



################# Simulación 3: Lt = 1 y At una caminata aleatoria  ####################

# Parrámetros nuevamente, por facilidad
T       <- 200         # Número de observaciones a simular
s       <- 0.2         # Tasa de ahorro
delta   <- 0.1         # Tasa de depreciación del capital
alpha   <- 0.3         # Participación del capital en el producto

# Productividad
A_Trend             <- vector(mode="numeric",length=T)
A_Trend[1]          <- 1     # Valor inicial de tendencia de la productividad
tendencia           <- 0     # Constante para crear tendencia lineal
choque_transitorio  <- 0     # Choque transitorio 
choque_permanente   <- 0.1   # Choque permanente
T_choque            <- 100  # Periodo del choque

# Cree vectores de choques
B_transitorio <- rep(0,T); B_permanente <- rep(0,T); 
B_transitorio[T_choque] <- choque_transitorio
B_permanente[T_choque]  <- choque_permanente

# Simule proceso de la productividad
for (t in 2:T){ 
  A_Trend[t] <- tendencia     + A_Trend[t-1]  + B_permanente[t]
  A[t]       <- A_Trend[t]    + B_transitorio[t]
}

# Grafique proceso de la producitividad
x11();
plot(A,col="black",type="p",ylab="A(t)",xlab="t",main="Simulación: A(t)")
lines(A,col="red")

# Oferta Laboral
L <- rep(1,T)  # Población constante y normalizado a 1.

# Iniciamos en estado estacionario cuando L=1 y A crece a g=tendencia
Kss = (s/(delta+tendencia))^(1/(1-alpha)) # Capital de estado ss en este caso
K0  = Kss  # Stock de capital menor al de estado estacionario

# Ahora simulamos las sendas del modelo
Simulacion.3 <- solow_sim(s,delta,alpha,T,K0,A,L)

# Recuperamos las sendas graficadas
K_sim <- Simulacion.3$K_sim
C_sim <- Simulacion.3$C_sim
I_sim <- Simulacion.3$I_sim
Y_sim <- Simulacion.3$Y_sim

## Ahora grafiquemos los valores en niveles

# Capital
x11();
plot(K_sim,col="black",type="p",ylab="K(t)",xlab="t",main="Simulación: K(t)")
lines(K_sim,col="red")        # Añadamos líneas para hacerla más llamativa

# Inversión
x11();
plot(I_sim,col="black",type="p",ylab="I(t)",xlab="t",main="Simulación: I(t)")
lines(I_sim,col="red")         # Añadamos líneas para hacerla más llamativa

# Consumo
x11();
plot(C_sim,col="black",type="p",ylab="C(t)",xlab="t",main="Simulación: C(t)")
lines(C_sim,col="red")        # Añadamos líneas para hacerla más llamativa

# Producción
x11();
plot(Y_sim,col="black",type="p",ylab="Y(t)",xlab="t",main="Simulación: Y(t)")
lines(Y_sim,col="red")        # Añadamos líneas para hacerla más llamativa