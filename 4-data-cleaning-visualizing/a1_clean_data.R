# Limpieza y visualización de datos
# Caso practico: Analizando los componentes del IPC
# Paso 1: Construir y limpiar la base de datos
#
# Ángelo Gutiérrez Daza
# 2025
#
# Programación II
# Programa de Estudios Superiores
# Banco de Guatemala
#
# Código probado utilizando la versión 4.5.1 de R

# ------------------------------------------------------------------------------
# 0) Inicialización
# ------------------------------------------------------------------------------

# Es buena practica cargar las librerías que se utilizaran al inicio
library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(readxl)
library(magrittr)

# Es buena practica comenzar por limpiar el entorno de trabajo, cerrar
# cualquier gráfico abierto y limpiar la consola
graphics.off()
rm(list=ls())
cat("\014")

# ------------------------------------------------------------------------------
# 1) Carga de datos
# ------------------------------------------------------------------------------

# Para comenzar, debemos cargar datos desde un archivo de Excel
# Para hacerlo, utilizaremos la funcion read_excel() del paquete readxl
# Ver: https://readxl.tidyverse.org/

# Podríamos cargar un solo datos, por ejemplo:
IPC_2023 <- read_excel("input/IPC_2023.xls")

# Pero en este caso, tenemos varios archivos que queremos cargar
# Una forma de hacerlo es utilizando un ciclo for
# En este caso, los archivos se llaman IPC_2018.xls, ..., IPC_2025.xls
# Por lo que podemos utilizar un ciclo for para cargar todos los archivos
# y asignarlos a variables llamadas IPC_2018, IPC_2019, ..., IPC_2025

# Primero veamos cuales son las variables en el conjunto de datos
variables_2023 <- names(IPC_2023)
variables_2023

# Tipo de variable en cada columna
col_tipo <- c(  "numeric" , # Año
                "text"    , # Mes
                "text"    , # Código
                "text"    , # Descripción
                "numeric" , # Rep.
                "numeric" , # Reg. I
                "numeric" , # Reg. II
                "numeric" , # Reg. III
                "numeric" , # Reg. IV
                "numeric" , # Reg. V
                "numeric" , # Reg. VI
                "numeric" , # Reg. VII
                "numeric"   # Reg. VIII
)

# Utilizaremos la función assign() para asignar el resultado de read_excel()
# a una variable cuyo nombre se construye dinámicamente
# Ver: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/assign

# Cargar las bases en un ciclo for
for (iBase in 2018:2025) {
    base_i <- paste0("IPC_", iBase)
    path_i <- paste0("input/", base_i, ".xls")
    assign(base_i, read_excel(path_i, col_types = col_tipo))
}

# ------------------------------------------------------------------------------
# 2) Unir las diferentes bases
# ------------------------------------------------------------------------------

# Ahora que tenemos los datos cargados, debemos juntarles en un solo data frame
# Para hacerlo, utilizaremos la función bind_rows() del paquete dplyr
# Ver: https://dplyr.tidyverse.org/reference/bind.html

# Para juntar solo dos bases, podemos hacer:
# ipc_data <- bind_rows(IPC_2018, IPC_2019)

# Pero para juntar todas las bases, podemos hacer:
# ipc_data <- bind_rows(IPC_2018, IPC_2019, IPC_2020, IPC_2021)

# O, de forma programática:
data_list <- lapply(2018:2025, function(i) get(paste0("IPC_", i)))
ipc_data  <- bind_rows(data_list)

# Ahora que tenemos los datos en un solo data frame, podemos eliminar las
# variables individuales para liberar memoria
rm(list = paste0("IPC_", 2018:2025))
gc() # Limpiar memoria

# ------------------------------------------------------------------------------
# 3) Limpiar algunos textos
# ------------------------------------------------------------------------------

# Algunos años incluyen los nombres o meses en mayúsculas, otros en minúsculas
# y otros en formato título. Para estandarizar, podemos convertir todos los
# nombres de los meses a formato título utilizando la función str_to_title()
# del paquete stringr
# Ver: https://stringr.tidyverse.org/reference/str_to_title.html
ipc_data %<>% mutate(Mes = str_to_title(Mes)) 

# Hagamos lo mismo con las descripciones
ipc_data %<>% mutate(Descripción = str_to_title(Descripción))

# En el caso del código, hay ocasiones donde aparece con un 0 al inicio, y 
# otras veces no. Para estandarizar, podemos agregar un 0 al inicio de los
# códigos que tienen menos de 2 dígitos, utilizando la función str_pad()
# del paquete stringr
# Ver: https://stringr.tidyverse.org/reference/str_pad.html

# Creemos una variable que cuente el número de dígitos en el código
ipc_data %<>% mutate(n_digitos = str_length(Código))

# Ahora creemos un id de codigo con un cero a la izquierda de los códigos 
# con menos de 2 dígitos
ipc_data %<>% mutate(
    id_codigo = if_else(str_length(Código) < 2 & !(Código == 0),
                        str_pad(Código, width = 2, side = "left", pad = "0"),
                        Código)
)

# ------------------------------------------------------------------------------
# 4) Crear variables adicionales de interés
# ------------------------------------------------------------------------------

# a) Identificador de fecha ----------------------------------------------------

# La variable Mes es de tipo texto, por lo que debemos convertirla a un formato
# que R pueda entender como fecha. Para hacerlo, comenzamos por reemplazar los
# nombres de los meses en español por sus equivalentes en inglés, ya que
# lubridate funciona mejor con nombres de meses en inglés

lista_de_reemplazos <- c(
    "Enero"      = "January"   ,
    "Febrero"    = "February"  ,
    "Marzo"      = "March"     ,
    "Abril"      = "April"     ,
    "Mayo"       = "May"       ,
    "Junio"      = "June"      ,
    "Julio"      = "July"      ,
    "Agosto"     = "August"    ,
    "Septiembre" = "September" ,
    "Octubre"    = "October"   ,
    "Noviembre"  = "November"  ,
    "Diciembre"  = "December"
)

ipc_data <- ipc_data %>% 
    mutate(Mes = str_replace_all(Mes, lista_de_reemplazos))

# Ahora convertimos el mes y el año en formato fecha utilizando la función
# parse_date() del paquete lubridate
# Ver: https://lubridate.tidyverse.org/reference/parse_date.html

# La función parse_date() requiere que la fecha tenga un día, por lo que
# agregaremos el día 01 al inicio de cada mes. Además, debemos indicar el
# formato de la fecha, que en este caso es "%d-%B" (día-mes en texto)
# Ver: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/strptime

# Creamos la variable t_date que contiene la fecha en formato Date
ipc_data <- ipc_data %>%
    mutate(t_fecha = parse_date(paste0("01-", Mes, "-", Año), format = "%d-%B-%Y")) 


# b) Crear variable identificando nivel de desagregación -----------------------

# Más adelante quedremos analizar los datos mas desagregados del IPC junto con
# el IPC general. Para ello, podemos utilizar el numero de dígitos del código 
# para inferir el nivel de desagregación

# Veamos cuantas categorías únicas hay en cada nivel de desagregación
ipc_data %>% 
    group_by(n_digitos) %>% 
    summarise(n_categorias = n_distinct(id_codigo))

# Revisemos los casos raros
flag1 <- ipc_data %>% filter(is.na(id_codigo))
print(flag1, n = 5)

# No incluyeron un "0" para el IPC general en 2025 ....
# Agreguémoslo manualmente
ipc_data %<>%
    mutate(id_codigo = if_else(is.na(id_codigo) & Año == 2025, "0", id_codigo))

# La inspección de los datos nos muestra que el "0" corresponde al IPC general,
# los códigos con uno y dos dígitos corresponden a grupos de consumo, y
# los demás códigos corresponden a subcategorías de cada una de ellas.

# Clasificándolos de forma acorde con una dummy de tres categorías
ipc_data %<>%
    mutate(
        id_nivel = case_when(
            id_codigo == 0         ~ "General"   ,
            n_digitos %in% c(1, 2) ~ "Grupo" ,
            n_digitos >= 3         ~ "Item"
        )
    )

# Y agreguemos una variable que indique la categoría padre de cada item
ipc_data %<>%
    mutate(
        id_grupo = case_when(
            n_digitos %in% c(0,1,2) ~ id_codigo,
            n_digitos %in% c(6)     ~ paste0("0",substr(id_codigo, 1, 1)),
            n_digitos %in% c(7)     ~ substr(id_codigo, 1, 2)
        )
    )

# Agreguemos también el título de la categoría padre
# Primero, creemos una tabla con los códigos y descripciones de los grupos
grupo_list <- ipc_data %>%
    filter( id_nivel == "Grupo" , year(t_fecha)==2023) %>%
    select(id_grupo = id_codigo, descr_grupo = Descripción) %>%
    distinct()

# Ahora unamos esta tabla con la tabla original
ipc_data %<>% left_join(grupo_list, by = "id_grupo") 

# Ahora veamos cuantos items hay en cada categoría padre
cat_list <- ipc_data %>%
    filter( id_nivel %in% c("Grupo", "Item") ) %>%
    group_by(id_grupo, year(t_fecha)) %>%
    summarise(n_items = n_distinct(id_codigo), first(descr_grupo)) %>%
    arrange(id_grupo)

print(cat_list, n = 2000)

# Podemos ver que hubo un cambio de categorias a partir de 2024


# # ------------------------------------------------------------------------------
# # 4) Exportar la base limpia
# # ------------------------------------------------------------------------------

# # Exportar la base limpia a un archivo CSV
# # Utilizaremos la función write_csv() del paquete readr
# # Ver: https://readr.tidyverse.org/reference/write_delim.html
write_csv(ipc_data, "./output/clean_data.csv")
