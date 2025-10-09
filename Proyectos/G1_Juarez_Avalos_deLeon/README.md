# Proyecto Grupal: Análisis del rating ELO de Hikaru Nakamura
## Integrantes: Jorge Ávalos, José De León, Marcos Juárez
## Curso: Programación II
## PES 2025

### Descripción del Proyecto
El presente proyecto tiene como objetivo estimar el talento verdadero del Gran 
Maestro de ajedrez Hikaru Nakamura a lo largo de su carrera, medido en términos
de su rating ELO. Utilizando datos históricos de su rating, publicados el sitio 
web [chess.com](https://www.chess.com), se aplicó un filtro de Kalman para 
realizar una modelación de estado espacio. Esto permitió filtrar el ruido 
inherente a las mediciones y obtener una estimación más precisa de su talento 
verdadero a lo largo del tiempo. Tambien se suavizó la serie de tiempo 
utilizando el filtro de Kalman hacia atrás y hacia adelante, proporcionando 
una estimación más refinada del talento de Nakamura.

### Filtros Utilizados en el Análisis
Se emplearon dos tipos de filtros de Kalman:

1. **Filtro de Kalman de media local**: este modelo emplea una ecuación de observación
y una ecuación de estado. El modelo asume que la observación es igual al estado no
observado más un ruido blanco gaussiano, y que el estado evoluciona con el tiempo
agregando otro ruido blanco gaussiano. Este enfoque es útil para capturar cambios
graduales en el talento de Nakamura a lo largo del tiempo.

Sus ecuaciones son las siguientes:
```latex
$$ y_t = \mu_t + \varepsilon_t, \;\varepsilon_t \sim \mathcal{N} \left(0, \sigma^2_{\varepsilon} \right) $$ 
$$ \mu_t = \mu_{t-1} + \eta_{t}, \; \eta_{t} \sim \mathcal{N} \left(0, \sigma^2_{\eta} \right)$$
```

2. **Filtro de Kalman con tendencia local**: este modelo también utiliza una ecuación
de observación y una ecuación de estado, pero incluye un componente de nivel en
el estado. Esto permite modelar no solo el nivel del talento, sino también su tasa 
de cambio a lo largo del tiempo. Al nivel también se le agrega un ruido blanco 
gaussiano, que permite capturar tendencias más complejas en la evolución del talento.

Sus ecuaciones son las siguientes:
```latex
$$ y_t = \mu_t + \varepsilon_t, \;\varepsilon_t \sim \mathcal{N} \left(0, \sigma^2_{\varepsilon} \right) $$ 
$$ \mu_t = \mu_{t-1} + \beta_{t-1} + \eta_{1_t}, \; \eta_{1_t} \sim \mathcal{N} \left(0, \sigma^2_{\eta_1} \right)$$ 
$$ \beta_t = \beta_{t-1} + \eta_{2_t}, \; \eta_{2_t} \sim \mathcal{N} \left(0, \sigma^2_{\eta_2} \right)$$
```

### Herramientas Utilizadas
El proyecto fue desarrollado utilizando el lenguaje de programación R, aprovechando
sus capacidades para el análisis de datos y paquetes especializados en modelación
de estado espacio y filtros de Kalman, así como el manejo y visualización de datos.

A continuación se muestra el código de R con las librerías necesarias para
correr el análisis:
```R
library(KFAS)
library(stats)
library(dplyr)
library(ggplot2)
```