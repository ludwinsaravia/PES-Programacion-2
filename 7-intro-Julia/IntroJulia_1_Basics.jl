# Fundamentos de Julia - Parte 1:
# Instalación, Comentarios, Ejecución, Instalación de Paquetes y Navegación
#
# Angelo Gutierrez-Daza
# Programa de Estudios Superiores
# Banco de Guatemala
# 2025
#
# Probado usando Julia 1.12.0

####################################################################################################
## 0) Configuración e Instalación
####################################################################################################

#=
Recomiendo usar VS Code como entorno de desarrollo integrado (IDE) para Julia.

Pasos para configurar el entorno de desarrollo:

1) Instalar Julia:
   - Opción recomendada: Usar Juliaup (gestor de versiones de Julia)
     Descargar desde: https://github.com/JuliaLang/juliaup
     ver también: https://julialang.org/install/
   - Opción alternativa: Instalar binario directamente desde: https://julialang.org/

2) Instalar VS Code:
   - Descargar desde: https://code.visualstudio.com/

3) Instalar la extensión de Julia para VS Code:
   - Abrir VS Code
   - Ir a Extensiones (Ctrl+Shift+X)
   - Buscar "Julia" e instalar la extensión oficial de Julia
   - Esta extensión incluye soporte para depuración, IntelliSense, navegación de código y más

4) Extensiones adicionales que recomiendo para VS Code:
   - "Fast Unicode Math" para usar notación LaTeX para caracteres unicode
   - "Even Better TOML" para editar archivos de configuración de Julia

VS Code con la extensión de Julia proporciona un entorno completo con:
- REPL integrado
- Depurador interactivo
- IntelliSense y autocompletado
- Navegación de código
- Soporte para notebooks Jupyter
- Plot viewer integrado
=#

####################################################################################################
## 1) Comentarios
####################################################################################################

# Comentar en cualquier lugar con "#"
a=1 # También válido después de un comando

#=
Usar estos dos símbolos
para escribir comentarios
de múltiples líneas
=#

##
#=
Podemos crear celdas de código con "##", "#---" o "# %%"
En VS Code, podemos asignar atajos para movernos entre celdas
Los atajos por defecto son:
- Ctrl+Enter: ejecutar celda actual
- Shift+Enter: ejecutar celda y moverse a la siguiente
- Alt+Enter: ejecutar celda y insertar nueva celda debajo
=#
##

####################################################################################################
## 2) Ejecutar Código
####################################################################################################

#=
Atajos de teclado en VS Code para ejecutar código:

- Ctrl+Enter: Ejecutar código seleccionado o línea actual en el REPL
- Shift+Enter: Ejecutar código seleccionado y avanzar a la siguiente línea
- Alt+Enter: Ejecutar celda actual
- F5: Ejecutar archivo completo
- Ctrl+F5: Ejecutar archivo sin depuración

El código destacado será enviado al REPL integrado de VS Code.
Si no hay código seleccionado, VS Code ejecuta la línea actual.

Para iniciar el REPL de Julia en VS Code:
- Usar Ctrl+Shift+P y buscar "Julia: Start REPL"
- O simplemente ejecutar código Julia, que iniciará automáticamente el REPL
=#

# Algunos comandos aleatorios para probar
print("Hola")
a = 1
A  = [1,2]
M = [1 2 ; 3 4]

# La ejecución es silenciosa a menos que se use en modo interactivo o en VS Code

# Atajos útiles de VS Code para Julia:
#=
Ctrl+Shift+P     - Abrir el panel de comandos
Ctrl+Enter       - Evaluar en el cursor
F5               - Ejecutar archivo actual
Ctrl+`           - Alternar terminal/REPL
Ctrl+Shift+`     - Crear nuevo terminal
Ctrl+J Ctrl+O    - Abrir plot viewer
Ctrl+Shift+D     - Abrir vista de depuración
F9               - Alternar punto de interrupción (breakpoint)
F10              - Paso sobre (step over) en depuración
F11              - Paso dentro (step into) en depuración
=#


####################################################################################################
# 3) Espacio de Trabajo
####################################################################################################

# Información de la sesión actual
versioninfo()

# Variables en el espacio de trabajo
varinfo()

#=
A diferencia de Matlab y R, no hay formas directas o indirectas en Julia para 
eliminar una variable del espacio de trabajo.

En su lugar, se recomienda usar Revise.jl para desarrollo iterativo:
# https://timholy.github.io/Revise.jl/stable/

Tampoco existe un comando "clear workspace":
https://discourse.julialang.org/t/why-julia-has-no-clear-variable/6541

La filosofía de Julia es que las variables locales en funciones se limpian 
automáticamente, y para desarrollo se recomienda reiniciar el REPL cuando sea necesario.

En VS Code, puedes reiniciar el REPL usando:
- Ctrl+Shift+P -> "Julia: Kill REPL" seguido de "Julia: Start REPL"
- O usar el comando "Julia: Restart REPL" directamente
=#

]
####################################################################################################
# 4) Directorio de Trabajo
####################################################################################################

# Algunos comandos usuales funcionan directamente
pwd() # Ver directorio de trabajo actual
cd("./OtroDirectorio") # Moverse usando rutas relativas...
cd("/Users/miusuario/Desktop/") # O rutas completas (ejemplo para macOS/Linux)

#=
Alternativamente, podemos escribir ";" en el REPL para entrar al modo shell
y usar nuestros comandos shell estándar para movernos y ejecutar comandos.
Para salir del modo shell, simplemente presionar backspace.

En VS Code, también podemos usar el terminal integrado (Ctrl+`) que nos permite
ejecutar comandos del sistema operativo directamente.
=#

####################################################################################################
# 5) Instalación de Paquetes
####################################################################################################

#=
- El ecosistema de paquetes de Julia está creciendo rápidamente y muchos de los
  paquetes existentes son de muy alta calidad
- Podemos instalar paquetes usando el REPL entrando al "Modo de Paquetes"
  Para hacer esto, simplemente presionar "]" en el REPL
- Después de instalar un paquete, aún necesitamos cargarlo:
    * Usar "using" para importar todo el paquete
    * Usar "import" para importar solo un subconjunto de funciones de un paquete
- Alternativamente, podemos usar la función Pkg.add()

En VS Code, el modo de paquetes del REPL funciona igual que en la terminal.
La extensión de Julia también proporciona herramientas para gestión de paquetes
a través de la interfaz gráfica.
=#

# Instalemos el Kernel de Julia para Jupyter
using Pkg
Pkg.add("IJulia")

#=
Otros comandos que podemos usar en el modo de paquetes incluyen:
st         # verificar estado de paquetes
up IJulia  # actualizar IJulia
rm IJulia  # remover paquete
up         # actualizar todos los paquetes
status     # mostrar estado de todos los paquetes
=#


####################################################################################################
# 6) Paquetes Útiles
####################################################################################################

# Algunas librerías que uso en otras partes de estas notas o para mi investigación
pkgList = [
# Utilidades
"IJulia"                 # Crear notebooks de Jupyter para Julia
"Revise"                 # Modificar código y usar los cambios sin reiniciar Julia
"RCall"                  # Ejecutar comandos de R desde Julia
"PyCall"                 # Ejecutar comandos de Python desde Julia  
"PythonCall"             # Alternativa moderna a PyCall (Julia 1.6+)
"OhMyREPL"               # Hacer que el REPL se vea bonito
"Pluto"                  # Notebooks interactivos
"PlutoUI"                # Interfaz para notebooks Pluto
"PrecompileTools"        # Herramientas para precompilación (Julia 1.9+)
# Métodos Numéricos ------------------------------------------------------------------------------
"Optim"                  # Biblioteca para optimización sin restricciones
"JuMP"                   # Excelente para resolver problemas de optimización de cualquier tipo
"Interpolations"         # Interpolación numérica
"FastGaussQuadrature"    # Calcular nodos y pesos para Cuadratura Gaussiana
"Cuba"                   # Wrapper con métodos de integración de la biblioteca CUBA
"Cubature"               # Integración multidimensional
"Zygote"                 # Diferenciación automática
"ForwardDiff"            # Diferenciación automática hacia adelante
# Matemáticas Simbólicas -------------------------------------------------------------------------
"Calculus"               # Derivadas, gradientes y hessianos simbólicos
"SymPy"                  # Wrapper del paquete SymPy de Python
"Symbolics"              # Sistema de computación simbólica nativo de Julia
# Variables Aleatorias ---------------------------------------------------------------------------
"Distributions"          # Para trabajar con variables aleatorias distintas a la uniforme
"StatsBase"              # Funciones estadísticas básicas
"Random"                 # Generación de números aleatorios (parte de la librería estándar)
# Econometría y Estadísticas ---------------------------------------------------------------------
"StatsKit"               # Meta-paquete con rutinas estadísticas
"GLM"                    # Modelos lineales generalizados
"MixedModels"            # Modelos de efectos mixtos
"StateSpaceModels"       # Modelos de espacio de estados usando notación Durbin y Koopman
"QuantEcon"              # Biblioteca con utilidades para economía
"FixedEffectModels"      # Estimación de modelos lineales con VI y variables categóricas de alta dimensión
# Trabajar con Datos -----------------------------------------------------------------------------
"DataFrames"             # Agregar tipo dataframe
"DataFramesMeta"         # Macros para trabajar con DataFrames
"Chain"                  # Mejora el comportamiento del operador pipe
"CSV"                    # Cargar archivos CSV
"XLSX"                   # Leer y escribir archivos Excel
"JSON3"                  # Trabajar con datos JSON
"TOML"                   # Trabajar con archivos TOML
"FredData"               # Importar datos de FRED
"PandasLite"             # Funcionalidad tipo pandas para DataFrames
# Gráficos ---------------------------------------------------------------------------------------
"Plots"                  # Wrapper de varias librerías para graficar
"PlotlyJS"               # Backend PlotlyJS para gráficos interactivos
"StatsPlots"             # Agregar "recetas" para hacer gráficos estadísticos y soporte para dataframes
"PGFPlotsX"              # Gráficos de alta calidad usando PGFPlots
"Makie"                  # Sistema de gráficos nativo de Julia de alta performance
"CairoMakie"             # Backend Cairo para Makie (gráficos estáticos)
"GLMakie"                # Backend OpenGL para Makie (gráficos interactivos)
"GraphMakie"             # Visualización de grafos con Makie
]

# Instalemos todos
Pkg.add(pkgList)

# Usar esto para actualizar, si ya están instalados
Pkg.update(pkgList)
# Usar ] up para actualizar todos los paquetes

# Construir todos los paquetes
for i in 1:length(pkgList)
	try
		eval(Meta.parse(string("Pkg.build(\"",pkgList[i],"\")")))
		println(string("Paquete ",pkgList[i]," construido"))
	catch
		println(string("!!!!!!!! Falló la construcción del paquete ",pkgList[i]))
	end
end

# Precompilar todos los paquetes
for i in 1:length(pkgList)
	try
		eval(Meta.parse(string("using ",pkgList[i])))
		println(string("Paquete ",pkgList[i]," precompilado"))
	catch
		println(string("!!!!!!!! Falló la precompilación del paquete ",pkgList[i]))
	end
end

# Esto precompila las dependencias actuales
# Pkg.precompile()

##################################################################################################
# 7) Problemas Potenciales Durante la Instalación
####################################################################################################

#=******************* Problemas Potenciales Instalando IJulia ***********************

Por razones que no comprendo completamente, este paquete instalará su propia versión 
de miniconda en lugar de usar la existente, si has instalado Anaconda previamente.

En principio, podemos especificar la ruta de la instalación actual de Jupyter antes
de descargar IJulia usando:

ENV["JUPYTER"] = "/usr/local/bin/jupyter"  # ejemplo para macOS/Linux
# ENV["JUPYTER"] = "C:\\ProgramData\\Anaconda3\\Scripts\\jupyter-notebook.exe"  # ejemplo para Windows
Pkg.build("IJulia")

Pero esto no siempre funciona. Jupyter encontrará el kernel de Julia sin problema,
pero usar notebook() para crear un notebook de Jupyter dentro de Julia puede
activar un prompt preguntando si quieres descargar una instalación local de
miniconda (aún así, 1GB pesado) que existirá solo en el directorio local .julia.

En VS Code, puedes usar notebooks de Jupyter directamente con la extensión de Julia
sin necesidad de configuración adicional.

******************************************************************************=#

#=********** En caso de problemas instalando RCall o PyCall****************
# Para PyCall (versión anterior):
ENV["PYTHON"] = "/usr/bin/python3"  # ejemplo para macOS/Linux
# ENV["PYTHON"] = "C:\\Python39\\python.exe"  # ejemplo para Windows
using Pkg
Pkg.build("PyCall")
using PyCall

# Para PythonCall (versión moderna, recomendada para Julia 1.6+):
# PythonCall maneja automáticamente la configuración de Python

# Para RCall:
ENV["R_HOME"] = ""
ENV["R_HOME"] = "/usr/lib/R"  # ejemplo para Linux
# ENV["R_HOME"] = "/Library/Frameworks/R.framework/Resources"  # ejemplo para macOS
# ENV["R_HOME"] = "C:\\Program Files\\R\\R-4.3.0\\"  # ejemplo para Windows
using Pkg
Pkg.build("RCall")
using RCall

******************************************************************************=#

#=******************* Instalando Librerías Sin Conexión *****************************

Si alguna vez tienes que instalar librerías sin conexión (o enfrentando un 
firewall corporativo que impide acceder a GitHub), aún puedes instalar algunas librerías:

1) Encuentra una computadora con acceso online y el mismo OS de la computadora objetivo
2) Instala Julia y todos los paquetes deseados en esta computadora
3) Reemplaza la carpeta .julia en la computadora objetivo con la de la computadora
   sin restricciones e inicia Julia
    * Esta carpeta usualmente está bajo el directorio USER
    * En macOS/Linux: ~/.julia
    * En Windows: C:\Users\[usuario]\.julia

Julia recompilará los paquetes y los ejecutará. Pero puedes tener algunos problemas
usando paquetes con dependencias externas, como aquellos que actúan como wrappers
de librerías de Python.

VS Code facilita la gestión de paquetes con su interfaz integrada para el
administrador de paquetes de Julia.

******************************************************************************=#
