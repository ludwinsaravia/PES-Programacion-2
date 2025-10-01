# Fundamentos de R - Parte 1:
# Instalación, Comentarios, Ejecución, Instalación de Paquetes y Navegación
#
# Angelo Gutierrez-Daza
# Programa de Estudios Superiores
# Banco de Guatemala
# 2025
#
# Probado usando R 4.5.1


################################################################################
### 0) Configuración
################################################################################

# 1) Instalar binario de la última versión directamente desde la página web:
# https://www.r-project.org/
#
# 2) Instalar R-Studio desde:
# https://rstudio.com/products/rstudio/download/preview/
#
# También me gusta usar la extensión de VSCode, así que he añadido algunos atajos
# de teclado para limpiar terminal, limpiar workspace, cambiar directorio de
# trabajo y archivo fuente.


################################################################################
### 1) Comentarios
################################################################################

# Comentar en cualquier lugar con "#"
a = 1 # También válido después de un comando

# No hay soporte para comentarios multilínea en R base
# Usar CTRL+SHIFT+C para comentar/descomentar en RStudio

# Cualquier línea de comentario que incluya al menos cuatro guiones (-), signos
# de igual (=), o gatos (#) al final automáticamente crea una sección de código.


################################################################################
### 2) Ejecutar Código
################################################################################

# Ctrl+Enter es el atajo de teclado predeterminado para ejecutar texto seleccionado
# El código seleccionado será enviado al REPL

# Algunos comandos aleatorios para probar
print("Hello World")
a <- 1
A <- c(1, 2)
M <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)

# La ejecución es silenciosa a menos que se use en modo interactivo o en VSCode

# Atajos de teclado interesantes de RStudio
#
# Ctrl+L           - Limpiar Terminal
# Ctrl+Shift+R     - Insertar Sección
# Ctrl+Shift+H     - Cambiar directorio de trabajo
# Ctrl+Shift+Enter - Evaluar el archivo actual
# Ctrl+Shift+F10   - Reiniciar sesión de R
# Ctrl+Shift+S     - Ejecutar archivo actual
# Ctrl+Shift+Enter - Ejecutar con eco
# Ctrl+1           - Mover foco al Editor de Código
# Ctrl+2           - Mover foco a la Consola
# Ctrl+I           - Indentar código
# Ctrl+Shift+A     - Reformatear código
# Alt + -          - Insertar: Asignación <-
# Ctrl+Shift+M     - Insertar: Pipe $>%
# Shift+F9         - Alternar Breakpoints Depuración
# F10              - Ejecutar Siguiente Línea Depuración
# Shift+F4         - Entrar en Función Depuración
# Shift+F6         - Terminar Función/Loop Depuración
# Shift+F5         - Continuar Depuración
# Shift+F8         - Detener Depuración Depuración
# Alt+Shift+K      - Mostrar Atajos de Teclado


################################################################################
### 3) Espacio de Trabajo
################################################################################

# Información de la sesión actual
sessionInfo()

# Variables en el espacio de trabajo
ls()

# Remover variables del espacio de trabajo
rm(a)
ls()

# Remover varias variables
a <- 1
b <- 2
c <- 3
rm(b, c)
ls()

# Remover todas las variables del espacio de trabajo
rm(list = ls())
ls()


################################################################################
# 4) Directorio de Trabajo
################################################################################

# Algunos comandos usuales funcionan directamente
getwd() # Ver directorio de trabajo actual
temp_dir <- getwd()
temp_dir

# Cambiar usando ruta relativa (notar la barra diagonal en ventana)
setwd("./OtroDirectorio")
getwd()


################################################################################
### 5) Instalación de Paquetes
################################################################################

# La principal atracción de R: Gran cantidad de librerías de buena calidad para
# análisis estadístico y procesamiento de datos

# Dos formas de instalarlas:

# a) Desde "The Comprehensive R Archive Network"
# Ventajas:
#   - Instalación fácil de librería y dependencias
#   - Repositorio curado: Si está aquí, usualmente funciona
#   - Documentación: Estandarizada y completa
# Desventajas:
#   - Algunos paquetes no están aquí debido al proceso estricto de envío
#   - Lo mismo para las últimas versiones
# Lista completa de paquetes disponibles y documentación:
# https://cran.r-project.org/web/packages/available_packages_by_name.html
# O por tema:
# https://cran.r-project.org/web/views/

# Para instalar, simplemente usar
install.packages("installr") # Este paquete añade librería para actualizar R en Windows

# Cargar librerías después de la instalación:
library(installr)
updateR()

# Instalar varios paquetes:
install.packages(c("installr", "here"))

# b) Instalar directamente desde un repositorio de GitHub

# Necesitaremos devtools y Rtools para que esto funcione:
# install_github("agutieda/BMR")

################################################################################
### 6) Paquetes Útiles
################################################################################

# Algunas librerías que uso en otras partes de estas notas o para mi propia investigación
pkg_list <- c(
    # Utilidades
    "installr",
    "Rcpp",
    "RcppArmadillo",
    "devtools",
    "IRkernel",
    "magrittr",
    "rprojroot",
    "here",
    # Trabajar con datos
    "tidyverse",
    "data.table",
    # Importar datos
    "haven",
    "fredr",
    # Métodos Numéricos
    "nleqslv",
    "nloptr",
    "numDeriv",
    "cubature",
    # Econometría
    "lfe",
    "vars",
    "fixest",
    # Series de Tiempo
    "zoo",
    "xts",
    "tseries",
    "forecast",
    "KFAS",
    # Web scraping
    "rvest",
    "xml2",
    # Temas de figuras
    "hrbrthemes",
    "viridis"
)

install.packages(pkg_list)

# Instalar servidor de lenguaje para soportar linting en VSCode
library(devtools)
install.packages('languageserver')
#remotes::install_github("REditorSupport/languageserver")

# Instalar kernel de R para Jupyter
install.packages("IRkernel")
IRkernel::installspec()
