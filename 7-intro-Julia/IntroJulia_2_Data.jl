# Fundamentos de Julia - Parte 2:
# Definición de Variables e Importación de Datos
#
# Angelo Gutierrez-Daza
# Programa de Estudios Superiores
# Banco de Guatemala
# 2025
#
# Probado usando Julia 1.12.0

################################################################################
##  1) Variables y Matrices

# Las variables se definen como es usual
a = 1
b = 1+1
c = a+b

# Podemos redefinir variables iterativamente
c = c+1

# Las matrices se definen de forma similar a Matlab...
A = [1   2]  # vector 1x2
B = [1 ; 2]  # vector 2x1

# A diferencia de Matlab, ; y , tienen el mismo efecto de definir filas
C = [1 , 2]  # vector 2x1
D = [1 ; 2]  # el mismo vector 2x1

# A diferencia de Matlab, los arrays pueden contener diferentes tipos de variables, incluso otros arrays
A = ["Genial" 2; 3.1 true; [[1 2]] 4]

# Julia hace copia dura de escalares (a diferencia de Python)
a = 1
b = a + 1
a = 10
b

# Pero a diferencia de Matlab y R, no hace copia dura de arrays por defecto
A = ["Algo" 2; 3.1 true];
B = A;
A[1,1] = "Cambiado";
A
B

# Usar copy() para evitar esto
A = ["Algo" 2; 3.1 true];
B = copy(A);
A[1,1] = "Cambio";
A
B

# Y deepcopy() para copiar también cualquier array interno
A = [1 2 3]
B = ["Algo", A]
C = copy(B)
D = deepcopy(B)
A[1] = 45
C[2]
D[2]

# Los nombres de variables son sensibles a mayúsculas pero podemos usar cualquier carácter UTF-8!
α = 5
α
🐐 = "Messi"
🐐


# Así como operar sobre ellos
🍕  = 5
🍔  = 4
🍕>🍔

# Estos también se pueden usar para definir funciones
∑(x,y) = x + y
∑(1,2)

# Algunas constantes ya están almacenadas
pi
π
ℯ
ℯ - exp(1)
Base.MathConstants.golden

# Usar variables griegas hará más fácil leer el código
# Pero no recomiendo usar emojis y otros caracteres más exóticos
# Ya que no aparecen en el workspace de forma clara y harán más difícil
# testear el código

# Se permite underscore en nombres de variables. A diferencia de R, los puntos no están permitidos.
numero_balones_oro_messi   = 8  # Actualizado a 2025 😉
# numero.balones.oro.ronaldo = 5  # Esto causaría un error

### Crear vectores ###
A = [1   2]     # Vector de dimensión 1x2
B = [1 ; 2]     # Vector de dimensión 2x1
C = hcat(1,2)   # Comando para concatenar de forma horizontal
D = hcat(A,C)   # También se puede usar para concatenar vectores

### Crear Matrices ###
AA = [1 ; 2 ]        # Matriz 2x1
BB = [1   2]         # Matriz 1x2
CC = [1 2 ; 3 4]     # Matriz 2x2

### Manipular Matrices ###
FF = CC                   # Crear una matriz duplicada
FF                        # Ver en VS Code o en el REPL
FF[1,1] = 10              # Asignación de elementos de la matriz
FF                        # Ver en VS Code o en el REPL
FF[1,2]                   # Selección del elemento en la posición (1,2)
GG = hcat(FF,[5; 6])      # Concatenar vector columna a una matriz
GG                        # Ver en VS Code o en el REPL
GG = vcat(GG,[7 8 9])     # Concatenar vector fila a una matriz
GG                        # Ver en VS Code o en el REPL
GG = [GG;GG]              # Otra forma de concatenar
GG[:,1]                   # Seleccionar todos los elementos de la 1era columna
GG[1,1:2]                 # Elementos 1 hasta 2 de la primera fila
GG[2:end,2]               # Elementos de la 2da columna, excepto el primero
GG = GG[1:2,:]            # Extraer sub-matriz
AA[end]		              # Último elemento de AA
AA[end-1]	              # Penúltimo elemento de AA

# Nota: Julia es row-dominant (orden por filas)
A = [10 20 30 ; 40 50 60 ; 70 80 90]
A[1]
A[2]
A[3]
A[4]
A[5]
A[6]
A[7]
A[8]
A[9]

################################################################################
# 2) Tipos de Datos
################################################################################

# Podemos verificar el tipo con typeof()
a = 1
typeof(a)

# Y usar bitstring() para ver la representación binaria
bitstring(a)

# Usar .0 para especificar flotantes en lugar de enteros
a = 1.0
typeof(a)
bitstring(a)

# y isa() para verificar el tipo de variable
isa(a,Float64)

# Otras funciones útiles
iseven(2)
isodd(2)
ispow2(4)
isfinite(a)
isinf(a)
isnan(a)

# Algunos comandos para verificar representación máx/mín de cada tipo
typemax(Int64)
typemin(Int64)
typemax(Float64)
typemin(Float64)
eps(Float64)

# Y verificar épsilon de máquina y mantisa
1.0 + eps(Float64)
precision(Float64)

# Julia tiene tipos "racionales" e "irracionales"
typeof(π)
a = 1 // 2
typeof(a)
b = 3//7
c = a+b
numerator(c)
denominator(c)
a = 1 // 0  # Infinity
a = 0 // 0  # NaN

# Podemos usar :: para fijar un tipo de variable
algo::Float64 = 3.14
# a = "Hola"  # Esto causaría un error por el tipo fijo

# Otros tipos de variables
a = 0x3				# Entero sin signo en base hexadecimal
a = 0b11			# Entero sin signo en base binaria
a = 3.0				# Flotante 64
a = 4 + 3im		    # Número imaginario
a = complex(4,3)	# Lo mismo
a = true			# Valor lógico
a = "String"	    # string
const aa = 1        # constante

# En problemas que requieran mucha precisión, podemos usar
BigFloat(2.0^66) / 3

# Julia cuenta con un sistema interno de promoción de tipos de variable
# que es clave para su desempeño. Esta utiliza una jerarquía entre tipos de
# variable para inicializarlas y "promocionarlas", dependiendo de la necesidad
a = Any[1 2 3; 4 5 6] # Any es un tipo de objeto abstracto del cual todos los demás tipos hacen parte
typeof(a)
b = convert(Array{Float64}, a)
typeof(b)
c = (1,1.0)
typeof(c)
d = [1,1.0]
typeof(d)
e = promote(c...)  # ... permite "desempaquetar" la tupla
typeof(e)
supertype(Float64)
subtypes(Integer)
supertype(Rational)

# Podemos consultar todos los tipos en:
# https://docs.julialang.org/en/v1/manual/types/

################################################################################
# 3) Trabajar con Datos
################################################################################

# La librería DataFrames permite utilizar esta estructura como en R
using DataFrames

# Los dataframes son una especie de lista que guarda "columnas" de datos
# relacionados. Por lo general, cada columna corresponde a una variable pero
# cada columna puede contener un tipo diferente de dato

# Para ilustrar su uso, utilizaremos una correlación interesante que hay entre
# el número de películas en que ha aparecido Nicolas Cage cada 12 meses con el
# número de editoras mujeres de la revista "Harvard Law Review", disponible en:
# http://tylervigen.com/view_correlation?id=9224

# Primero, organizamos los vectores de datos

# No. de películas en que apareció Nicolas Cage entre 2005 y 2009
NC = [2, 3, 4, 1, 4]

# No. de mujeres editoras en "Harvard Law Review" entre 2005 y 2009
ME = [9, 14, 19, 12, 19]

# Ahora construimos un data frame.
CorrelacionEspurea = DataFrame(NC=[2,3,4,1,4], ME=[9,14,19,12,19])

# Veamos cómo luce un data frame
describe(CorrelacionEspurea)

# Vemos que tiene tipo DataFrame
typeof(CorrelacionEspurea)

# Podemos acceder a las variables de un dataframe usando la misma notación
# que para matrices
CorrelacionEspurea[1,2]   # Primera fila, segunda columna
CorrelacionEspurea[:,2]   # Toda la segunda columna
CorrelacionEspurea[2,:]   # Toda la segunda fila

# También podemos acceder directamente usando campos
CorrelacionEspurea.ME
CorrelacionEspurea.NC

# O usando el nombre como índice
CorrelacionEspurea[:,:ME]

# La función names() permite saber el nombre de las columnas
names(CorrelacionEspurea)

# Para graficar desde un DataFrame, es necesario usar @df y StatsPlots
using StatsPlots
fechas = ["Año 2005","Año 2006","Año 2007","Año 2008","Año 2009"]
@df CorrelacionEspurea plot(fechas, [:NC :ME], label=["Nicolas Cage" "Editoras Harvard"], 
                           title="Correlación Espuria", xlabel="Año", ylabel="Cantidad")

################################################################################
# 4) Cargar Datos Desde Archivos Externos
################################################################################

### Directorio de Trabajo ###
# Antes de cargar datos desde un archivo externo, debemos asegurarnos que el
# directorio de trabajo de Julia sea el mismo donde se encuentran los archivos

# En VS Code, el directorio de trabajo se muestra en el explorador de archivos
# y podemos usar el terminal integrado para navegar

# Con el comando cd() cambiamos el directorio donde nos encontramos ubicados
filePath = @__DIR__  # Obtiene el directorio del archivo actual
cd(filePath)
pwd()

# En el directorio Auxiliary/ hay varios archivos con una serie de tiempo del PIB anual de
# Guatemala en USD de 2010, en diferentes formatos

# Vamos a ver cómo cargar el archivo para cada formato

### Archivo CSV (Comma-Separated Values) ###
# La forma moderna recomendada es usar CSV.jl
using CSV, DataFrames

# Para archivos en el subdirectorio Auxiliary
datos_desde_csv = CSV.read("Auxiliary/PIB.csv", DataFrame)
datos_desde_csv

# Para graficar
using Plots
plot(datos_desde_csv.Year, datos_desde_csv.Guatemala, 
     title="PIB Guatemala", xlabel="Año", ylabel="PIB (USD 2010)", 
     legend=false, linewidth=2)

### Archivo de Excel ###
# Usamos XLSX.jl que es más moderno y eficiente
using XLSX

# Archivo xlsx (formato moderno recomendado)
datos_desde_xlsx = DataFrame(XLSX.readtable("Auxiliary/PIB.xlsx", "PIB"))
plot(datos_desde_xlsx.Year, datos_desde_xlsx.Guatemala,
     title="PIB Guatemala desde Excel", xlabel="Año", ylabel="PIB (USD 2010)",
     legend=false, linewidth=2)

### Archivo de texto ###
# Para archivos de texto delimitados, también podemos usar CSV.jl
datos_desde_texto = CSV.read("Auxiliary/PIB.txt", DataFrame, delim='\t')
plot(datos_desde_texto.Guatemala)

################################################################################
# 5) Usando FRED (Federal Reserve Economic Data)
################################################################################

# Cargamos la librería FredData
using FredData, StatsPlots

# Primero debemos crear una cuenta en FRED y solicitar "API Key"
# Registrarse en: https://fred.stlouisfed.org/docs/api/api_key.html
my_API_KEY = "API_KEY"  # Reemplaza con tu propia API key

# La función Fred nos creará una conexión con el API de FRED
f = Fred(my_API_KEY)

# Podemos usar el comando get_data para descargar directamente una serie
# usando su token, si ya lo conocemos
gdpFRED_1 = get_data(f, "GDPC1")  # PIB real de Estados Unidos

# La función crea una estructura cuyos campos contienen la información
# de la variable descargada
gdpFRED_1

# Extraemos el DataFrame
tableFRED1 = gdpFRED_1.data

# Podemos acceder a los datos en el campo "data"
@df tableFRED1 plot(:date, :value, 
                   title="PIB Real de Estados Unidos", 
                   xlabel="Fecha", 
                   ylabel="Billones de dólares de 2017",
                   legend=false, 
                   linewidth=2)

# También podemos ingresar todas las opciones disponibles en el API
# Datos con fecha vintage específica
data_vintage = get_data(f, "GDPC1"; vintage_dates="2020-01-01")

# Datos con frecuencia y transformación específica
data_annual = get_data(f, "GDPC1"; frequency="a", units="chg")  # Cambio porcentual anual


# Pueden consultar más opciones en:
# https://research.stlouisfed.org/docs/api/fred/
# https://github.com/micahjsmith/FredData.jl

#=
Notas importantes sobre FRED:
- Necesitas una API key gratuita de FRED
- Hay límites de velocidad en las consultas (no más de 120 requests por minuto)
- Las series tienen diferentes frecuencias (diaria, mensual, trimestral, anual)
- Puedes buscar series en: https://fred.stlouisfed.org/
- VS Code muestra los DataFrames de forma clara en el visor de variables
=#
