# Fundamentos de Julia: 
# Parte 5) Gráficas y Visualización
#
# Angelo Gutierrez-Daza
# Programa de Estudios Superiores
# Banco de Guatemala
# 2025
#
# Probado usando Julia 1.12.0
#
# En VS Code, las gráficas se muestran automáticamente en el panel de plots
# Puedes usar Ctrl+Shift+P y buscar "Julia: Open Plot Navigator" para navegar entre gráficas

################################################################################
# 1) La librería Plots
################################################################################

filePath = @__DIR__
cd(filePath)
pwd()

# En Julia existen varias librerías para realizar figuras de forma rápida
# La librería estándar más conocida es "Plots"
# En VS Code, las gráficas aparecen automáticamente en el panel de plots

using Plots
# En Julia moderna, GR viene incluido y se activa automáticamente
gr()  # Esto selecciona el "backend" a usar. Explicaré más de esto abajo.

# Varios de estos ejemplos son tomados de la documentación
# de Plots y de la galería de figuras actualizada disponible en:
# https://docs.juliaplots.org/stable/generated/gr/

######################
# Líneas
######################

### Gráfica simple: Una línea
x = 0:0.1:2pi
y = cos.(x)
plot(x, y, c="blue", linewidth=3)

# Nota que el ";" al final de la línea silencia el output en el REPL
# En VS Code, la gráfica se muestra automáticamente en el panel de plots

# En VS Code no necesitamos closeall(), pero lo incluimos para compatibilidad
# closeall()  # Para cerrar ventanas externas (no necesario en VS Code)

x = 0:0.1:2pi
y = cos.(x)
plot(x, y, c="blue", linewidth=3);

# No hay output en el REPL, pero VS Code muestra la gráfica automáticamente
# Podemos asignar un nombre a la figura y usar display() si es necesario
x = 0:0.1:2pi
y = cos.(x)
my_plot = plot(x, y, c="blue", linewidth=3);
display(my_plot)  # En VS Code esto es opcional

### Gráfica más compleja: Dos líneas y opciones en los ejes

# Podemos construir las figuras por piezas
# Primero, usamos plot para construir la figura base

x = 0:0.1:2pi
y1 = cos.(x)
y2 = sin.(x)
plot(x, y1, c="blue", linewidth=3);

# Luego el comando plot! para mutar la figura existente
# Acá lo hemos usado para añadir una línea
plot!(x, y2, c="red", line=:dash)

# Podemos usar argumentos como "title" y "xlabel" para controlar los ejes
title!("Funciones Trigonométricas")
xlabel!("ángulo")
ylabel!("sin(x) y cos(x)")

# También podemos usar plot! de nuevo para hacer cosas como ajustar los ejes
plot!(xlims=(0,2π), ylims=(-2, 2))

# De forma alternativa, podríamos usar:
# xaxis!("ángulo",(0,2π))
# yaxis!("sin(x) y cos(x)", (-2,2))

# Otras útiles son: xlims!, ylims!, xticks!, yticks!

# O ingresar todas las opciones directamente
# misma gráfica
x = 0:0.1:2pi
y1 = cos.(x)
y2 = sin.(x)
plot(x, y1, 
    c="blue", 
    linewidth=3, 
    title="Funciones Trigonométricas",
    xlabel="ángulo",
    ylabel="sin(x) y cos(x)",
    label="cos(x)"
)
plot!(x, y2, c="red", line=:dash, label="sin(x)")
plot!(xlims=(0,2π), ylims=(-2, 2))


# También podemos ingresar matrices en lugar de vectores, 
# en cuyo caso, cada columna es interpretada como una serie
using Random
Random.seed!(2025)  # Actualizado

time = 30
y1 = cumsum(randn(time))
y2 = cumsum(randn(time))
y3 = cumsum(randn(time))
y4 = cumsum(randn(time))
y5 = cumsum(randn(time))
Y  = [y1 y2 y3 y4 y5]

plot(1:time, Y,
    xlabel="tiempo",
    ylabel="posición",
    label=["y1" "y2" "y3" "y4" "y5"],
    legend=:bottomleft,
    title="Caminatas Aleatorias"
)

# Otro ejemplo del "multiple dispatching" de Julia: 
# Si usamos como argumento una función en lugar de una serie de 
# datos, Julia grafica la función en un rango dado
f(x) = 5exp(-x^2)
g(x) = x^2
plot([f, g], -3, 3, label=["f(x) = 5e^(-x²)" "g(x) = x²"])

# Gráficas paramétricas
xₜ(t) = sin(t)
yₜ(t) = sin(2t)
plot(xₜ, yₜ, 0, 2π, legend=false, fill=(0,:orange), title="Curva Paramétrica")

# Este comportamiento es fácil de extender a funciones 3D
t = range(0, stop=10, length=1000)
x = cos.(t)
y = sin.(t)
z = sin.(5t)
plot(x, y, z, title="Espiral 3D", xlabel="x", ylabel="y", zlabel="z")

#= El input de plot() puede tomar muchas formas
plot()                                    # objeto "Plot" vacío
plot(4)                                   # Inicializar con 4 series vacías
plot(rand(10))                            # 1 serie ... x = 1:10
plot(rand(10,5))                          # 5 series... x = 1:10
plot(rand(10), rand(10))                  # 1 serie 
plot(rand(10,5), rand(10))                # 5 series... y es igual para todas
plot(sin, rand(10))                       # y = sin.(x)
plot(rand(10), sin)                       # lo mismo... y = sin.(x)
plot([sin,cos], 0:0.1:π)                  # 2 series, sin.(x) y cos.(x)
plot([sin,cos], 0, π)                     # sin y cos en el rango [0, π]
plot(1:10, Any[rand(10), sin])            # 2 series: rand(10) y map(sin,x)
# plot(dataset("Ecdat", "Airline"), :Cost)  # Columna :Cost de un DataFrame
=#

######################
# Scatters
######################

# Por supuesto que Julia hace Scatters
Random.seed!(1)
n = 50
x = rand(n)
y = rand(n)
scatter(x, y, title="Scatter Simple", xlabel="x", ylabel="y") 

# De forma alternativa, podemos usar el comando "plot"
n = 50
x = rand(n)
y = randn(n)
z = randn(n)*0.5
ms = rand(50) * 20
plot(x, [y z], seriestype=:scatter, markersize=ms, 
     label=["Serie Y" "Serie Z"], title="Scatter con Tamaños Variables") 

# Hacer scatterplots en 3D es igual de fácil
n = 50
x = rand(n)
y = rand(n,3)
z = rand(n,3)
ms = rand(50) * 10
scatter(x, y, z, markersize=ms, title="Scatter 3D")

# Uno más bonito
n = 100
ts = range(0, stop=8π, length=n)
x = ts .* map(cos, ts)
y = (0.1ts) .* map(sin, ts)
z = 1:n
plot(x, y, z, 
    zcolor=reverse(z),
    m=(10, 0.8, :blues, Plots.stroke(0)), 
    legend=false,
    colorbar=true,
    w=5,
    title="Espiral Colorida 3D"
)
plot!(zeros(n), zeros(n), 1:n, w=10)

# Podemos combinarlo con plot() para hacer figuras sofisticadas
x = 0:10:100
y = rand(11, 4)
z = rand(100)
plot(x, y, label="líneas", w=3, palette=:grays, fill=0, α=0.6)
scatter!(z,
    zcolor=abs.(z .- 0.5),
    m=(:heat, 0.8, Plots.stroke(1, :green)), 
    ms=10 * abs.(z .- 0.5) .+ 4,
    label="gradiente"
)

# Muchas funciones de esta librería tienen comportamiento similar

######################
# Histogramas
######################

Random.seed!(1)
x = randn(1000)
y = randn(1000)
z = randn(1000)
histogram(x, bins=20, α=0.8, label="A", title="Histogramas Múltiples")
histogram!(y, bins=20, α=0.6, label="B")
histogram!(z, bins=20, α=0.4, label="C")

######################
# Anotaciones
######################
y = rand(10)
plot(y, annotations=(3, y[3], Plots.text("esto es #3", :left)), 
     legend=false, title="Gráfica con Anotaciones")
annotate!([(5, y[5], Plots.text("esto es #5", 16, :red, :center)), 
           (10, y[10], Plots.text("esto es #10", :right, 20, "courier"))])
scatter!(range(2, stop=8, length=6), rand(6), marker=(50, 0.2, :orange), 
         series_annotations=["serie", "anotaciones", "mapean", "a", "serie", Plots.text("datos", :green)])

######################
# Barras
######################

# Ejemplo simple 
x = 1:20
y = rand(20)*100
bar(x, y, title="Notas de Estudiantes", xlabel="ID Estudiante", ylabel="Notas de clase")

# Barras apiladas
x = 1:20
y = rand(20)*100
z = rand(20)*100
bar(x, [y z], label=["Examen 1" "Examen 2"], title="Comparación de Exámenes")

# O de lado (horizontales)
ticklabel = string.(collect('a':'l'))
bar(1:12, orientation=:h,
          yticks=(1:12, ticklabel),
          yflip=true,
          title="Barras Horizontales"
)

######################
# Contornos
######################
f(x,y) = x^2 + y^2
xNodes = range(-20, 20, length=101) 
yNodes = range(-20, 20, length=101) 
xGrid = repeat(xNodes, 1, 101)
yGrid = repeat(yNodes, 1, 101)'
zGrid = f.(xGrid, yGrid)
contour(xNodes, yNodes, zGrid, title="Contornos de f(x,y) = x² + y²")

# O, más "julia"
x = y = range(-20, 20, length = 101)
contour(x, y, (x, y) -> x^2 + y^2, title="Contornos (Forma Elegante)")

######################
# Superficies
######################
f(x,y) = x^2 + y^2
x = -10:10
y = x
surface(x, y, f, title="Superficie 3D")

# O, de forma alternativa
plot(x, y, f, linetype=:surface, title="Superficie (Alternativa)")

######################
# Figuras con subfiguras
######################

# Podemos crear múltiples figuras y añadirlas en una sola
fig1 = plot(sin, 0, 2pi, xlabel="x1", title="Seno");
fig2 = plot(cos, 0, 2pi, xlabel="x2", title="Coseno");
fig3 = histogram(randn(1000), xlabel="x3", title="Histograma");
fig4 = plot(x->exp(-x^2), -3, 3, xlabel="x4", title="Gaussiana");
plot(fig1, fig2, fig3, fig4, layout=(2,2), size=(800,600))

# Podemos usar la opción layout para controlar el grid
yPlot = randn(100, 4)  # Datos simulados 
plot(yPlot, 
    layout=4,
    title=["Gráfica 1" "Gráfica 2" "Gráfica 3" "Gráfica 4"]
)

plot(yPlot, 
    layout=(4,1), 
    size=(600, 800),
    palette=[:grays :blues :heat :lightrainbow], 
    bg_inside=[:orange :pink :darkblue :black],
    title="Layout Vertical"
)

# Podemos usar la macro @layout para crear layouts más complejos
fig1 = plot(sin, 0, 2pi, xlabel="x1", title="Seno");
fig2 = plot(cos, 0, 2pi, xlabel="x2", title="Coseno");
fig3 = histogram(randn(1000), xlabel="x3", title="Histograma");
fig4 = plot(x->exp(-x^2), -3, 3, xlabel="x4", title="Gaussiana");
fig5 = histogram(randn(1000), xlabel="x5", title="Histograma 2");
l = @layout [a b; c ; d e]
plot(fig1, fig2, fig3, fig4, fig5, layout=l, size=(800,600))

# Un ejemplo más sofisticado
yPlot = randn(1000,5)
l = @layout [a; b [c; d e] ]
plot(yPlot,
     layout=l, 
     seriestype=[:line :histogram :scatter :steppre :bar], 
     legend=false,
     ticks=nothing,
     border=:none,
     title="Layout Complejo"
)

# Un cheatsheet actualizado se puede consultar en 
# https://github.com/JuliaPlots/Plots.jl


################################################################################
# 2) Backends
################################################################################

#=
Plots en realidad no es una librería para graficar por sí misma.

Es una librería que permite invocar, con una misma sintaxis,
diferentes librerías diseñadas para hacer figuras.

Hemos estado usando una de estas, GR, que hemos invocado 
al inicio con el comando gr().

Cada "backend" tiene ventajas y desventajas. Estas son algunas
recomendaciones según el manual de Plots:

Si quiere...	         entonces use...
características	         PlotlyJS, GR, PythonPlot
velocidad                GR, PlotlyJS
interactividad	         PlotlyJS, PythonPlot
belleza      	         PlotlyJS, PGFPlotsX
gráficas en REPL	     UnicodePlots
gráficas 3D	             GR, PlotlyJS, PythonPlot
ventana GUI	             GR, PythonPlot, PlotlyJS
footprint pequeño	     UnicodePlots
exportar a HDF5          HDF5Plots

Backends modernos recomendados (2025):
- GR: Rápido, buena calidad, funciona bien en VS Code
- PlotlyJS: Interactivo, ideal para dashboards
- PythonPlot: Reemplazo moderno de PyPlot
- CairoMakie/GLMakie: Alternativas de alta calidad

En VS Code, GR funciona excelentemente y muestra las gráficas
automáticamente en el panel de plots.

Puede consultar más en:
https://docs.juliaplots.org/stable/backends/

PythonPlot, en particular, es un wrapper de la librería matplotlib
de Python y reemplaza al anterior PyPlot.

Se puede invocar usando pythonplot()
=#


################################################################################
# 3) Exportar Figuras
################################################################################

# Podemos usar savefig para guardar la figura actual
# En VS Code, también puedes hacer clic derecho en el panel de plots y "Save As"
savefig("FiguraSofisticada.png") # Guarda la gráfica actual como .png

# O añadir el nombre de alguna figura específica
savefig(fig1, "LineaSimple.pdf") 

#= Dependiendo del "backend", se puede guardar en los siguientes formatos:
formato  backend
eps	     inspectdr, plotlyjs, pythonplot
html	 plotly, plotlyjs
pdf	     gr, inspectdr, pgfplotsx, plotlyjs, pythonplot
png	     gr, inspectdr, plotly, plotlyjs, pythonplot
ps	     gr, pythonplot
svg	     gr, inspectdr, pgfplotsx, plotly, plotlyjs, pythonplot
tex	     pgfplotsx
text	 hdf5plots, unicodeplots

En VS Code, las gráficas se pueden exportar fácilmente usando
el menú contextual del panel de plots.
=#

################################################################################
# 4) Recetas y Curiosidades
################################################################################

# Algunas librerías como StatsPlots incluyen "recetas" para 
# usar con la librería Plots
using StatsPlots
measles = [38556, 24472, 14556, 18060, 19549, 8122, 28541, 7880, 3283, 4135, 7953, 1884]
mumps = [20178, 23536, 34561, 37395, 36072, 32237, 18597, 9408, 6005, 6268, 8963, 13882]
chickenPox = [37140, 32169, 37533, 39103, 33244, 23269, 16737, 5411, 3435, 6052, 12825, 23332]
ticklabel = string.(collect('A':'L'))
groupedbar([measles mumps chickenPox], bar_position = :dodge, bar_width=0.7, 
           xticks=(1:12, ticklabel), label=["Sarampión" "Paperas" "Varicela"],
           title="Casos de Enfermedades por Mes")

# Estas permiten graficar otro tipo de datos como DataFrames y Tablas 
# usando la macro @df
using DataFrames, StatsPlots
df = DataFrame(a = 1:10, b = 10 .* rand(10), c = 10 .* rand(10))

# Graficar desde un DataFrame
@df df plot(:a, [:b :c], colour = [:red :blue], 
           title="Gráfica desde DataFrame", label=["Serie B" "Serie C"])
@df df scatter!(:a, :b, markersize = 4 .* log.(:c .+ 0.1))

# Ejemplo con datos más realistas
using Random
Random.seed!(123)
n = 1000 
x = randn(n)
y = 10 .+ 5x + randn(n)
z = x.^2 + randn(n)*0.5
w = rand(n)
datosEjemplo = DataFrame(Variable1=x, Variable2=y, Variable3=z, Variable4=w)

# Gráfico de dispersión con filtrado
subset_data = filter(row -> row.Variable1 > 0, datosEjemplo)
@df subset_data scatter(:Variable2, :Variable3, 
                       markersize=:Variable4.*10,
                       title="Scatter con Filtrado",
                       xlabel="Variable 2", ylabel="Variable 3")

# Finalmente, histogramas de densidad por grupos
using RDatasets
try
    school = RDatasets.dataset("mlmRev", "Hsb82")
    @df school density(:MAch, group = (:Sx, :Sector), legend = :topleft,
                      title="Densidad por Grupos")
catch e
    println("RDatasets no disponible, saltando ejemplo")
end

# Podemos usar la macro @gif para crear animaciones!
# Nota: esto puede tomar un tiempo considerable

using Plots

mutable struct Lorenz
    dt; σ; ρ; β; x; y; z
end

function step!(l::Lorenz)
    dx = l.σ*(l.y - l.x)       ; l.x += l.dt * dx
    dy = l.x*(l.ρ - l.z) - l.y ; l.y += l.dt * dy
    dz = l.x*l.y - l.β*l.z     ; l.                  z += l.dt * dz
end

# Crear atractor de Lorenz animado (descomenta para ejecutar)
attractor = Lorenz((dt = 0.02, σ = 10., ρ = 28., β = 8//3, x = 1., y = 1., z = 1.)...)
plt = plot3d(1, xlim=(-25,25), ylim=(-25,25), zlim=(0,50),
                title = "Atractor de Lorenz", marker = 2)
anim = @gif for i=1:300  # Reducido para ejemplo
    step!(attractor)
    push!(plt, attractor.x, attractor.y, attractor.z)
end every 10
