# 📊 Índice de Desarrollo Tecnológico Regional en Guatemala (IDTR)

## 🧩 Descripción del Proyecto

El proyecto consiste en la **construcción de un índice de desarrollo tecnológico regional en Guatemala**, basado principalmente en datos de la **ENCOVI 2021** y complementado con información del **Banco de Guatemala (BANGUAT)** y la **Superintendencia de Telecomunicaciones (SIT)**.

El índice busca **evaluar el nivel de acceso, uso e infraestructura tecnológica por departamento**, permitiendo identificar brechas territoriales y sociales en conectividad digital. Su propósito es servir como herramienta de diagnóstico y apoyo para políticas públicas orientadas al cierre de la brecha tecnológica.

------------------------------------------------------------------------

## ⚙️ Estructura del Proyecto

------------------------------------------------------------------------

## 📚 Librerías utilizadas

El análisis fue realizado en **R**, utilizando las siguientes librerías principales:

| Librería           | Función principal                                 |
|--------------------|---------------------------------------------------|
| `tidyverse`        | Manipulación y limpieza de datos                  |
| `dplyr`            | Agrupamiento y creación de indicadores            |
| `readxl` / `readr` | Lectura de archivos Excel y CSV                   |
| `writexl`          | Exportación de resultados a Excel                 |
| `lubridate`        | Manejo de fechas                                  |
| `stringr`          | Procesamiento de texto                            |
| `ggplot2`          | Visualización de datos                            |
| `magrittr`         | Uso del pipe (`%>%`) para operaciones encadenadas |

------------------------------------------------------------------------

## 🧮 Indicadores principales

Los indicadores utilizados para el cálculo del índice incluyen:

| Variable                     | Descripción                                        | Fuente  |
|-----------------------|-----------------------------|--------------------|
| `var_uso_intern`             | Proporción de personas que usan internet           | ENCOVI  |
| `var_uso_intern_mujeres`     | Proporción de mujeres que usan internet            | ENCOVI  |
| `var_uso_acceso_tel_mov`     | Proporción de personas con acceso a teléfono móvil | ENCOVI  |
| `propor_hogares_internet`    | Proporción de hogares con acceso a internet        | ENCOVI  |
| `propor_hogares_computadora` | Proporción de hogares con computadora              | ENCOVI  |
| `pct_rural`                  | Porcentaje de población rural                      | ENCOVI  |
| `rb_por_100k`                | Radiobases por cada 100 mil habitantes             | SIT     |
| `lineas_por_1k`              | Líneas fijas por cada 1 mil habitantes             | SIT     |
| `pib_per_capita`             | PIB per cápita departamental                       | FUNDESA |

------------------------------------------------------------------------

## 🔢 Entradas (Input)

Los principales archivos de entrada son:

-   `ENCOVI_personas.xlsx`: Base de datos de personas de la ENCOVI.
-   `ENCOVI_hogares.xlsx`: Base de datos de hogares de la ENCOVI.
-   `Equipamiento.xlsx`: Base de datos de equipamiento del hogar.
-   `Pib_per_capita.xlsx`: PIB per cápita por departamento.
-   `Radiobases.xlsx`: Radiobases por departamento.
-   `Telefonía_Fija.xlsx`: líneas fijas por departamento.

------------------------------------------------------------------------

## 📈 Salidas (Output)

El proyecto genera los siguientes productos:

| Archivo                               | Descripción                                                          |
|--------------------------------|----------------------------------------|
| `indicadores_acceso_tecnologico.xlsx` | Tabla resumen con indicadores sin normalizar por departamento        |
| `idx.xlsx`                            | Índice final de desarrollo tecnológico (normalizado 0--100)          |
| `IDTR`                                | Indicador de Desarrollo Tecnológico Regional (IDTR) por departamento |

------------------------------------------------------------------------

## 🧠 Metodología de cálculo

1.  **Limpieza de datos:** depuración de valores faltantes y filtrado por departamento.
2.  **Construcción de indicadores:** cálculo de proporciones ponderadas por el factor de expansión ENCOVI.
3.  **Normalización:** escala 0--100 tomando como referencia América Latina.
4.  **Ponderación:** combinación de dimensiones (acceso, uso e infraestructura) según su relevancia teórica.
5.  **Cálculo del índice final:** promedio ponderado de los indicadores estandarizados.
6.  Generación de gráficos comparativos.

------------------------------------------------------------------------

## 👩‍💻 Autores

**Equipo:**\
- Paulo Garrido\
- Stefani Villeda\
- Luis Monroy\
- David García

**Curso:** Programación II\
**Institución:** Banco de Guatemala -- Programa de Estudios Superiores (PES)\
**Año:** 2025

------------------------------------------------------------------------

## 🌐 Referencias

-   BID (2024). *Índice de Desarrollo de la Banda Ancha en América Latina y el Caribe.*\
-   ITU (2023). *Measuring digital development: Facts and figures.*\
-   ENCOVI (INE, 2021). *Encuesta Nacional de Condiciones de Vida.*\
-   SIT (2023). *Estadísticas de telecomunicaciones en Guatemala.*

------------------------------------------------------------------------
