library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(stringr)
library(readxl)
library(magrittr)
library(readxl)
library(tidyverse)
# install.packages("gdata")
library(gdata)
# install.packages("plotly")
library(plotly)
library(ggplot2)
# install.packages("patchwork")
library(patchwork)



datosPIB <- read_csv("DatosPIB.csv")
datosCC <- read_csv("DatosCC.csv")


# FUNCIONES 

# 1. Función para gráfica de comparación PIB Nominal vs Real
grafica_pib_comparacion <- function(datos, año_inicial, año_final) {
    
    datos_filtrados <- datos %>% filter(Anio >= año_inicial & Anio <= año_final)
    
    datos_largos <- datos_filtrados %>%
        pivot_longer(cols = c(PIB_nominal, PIB_real), 
                     names_to = "tipo_PIB", 
                     values_to = "valor")
    
    ggplot(datos_largos, aes(x = Fecha, y = valor, color = tipo_PIB)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = paste("PIB Nominal vs PIB Real"),
             subtitle =  paste(año_inicial, "a", año_final),
             x = "Año",
             y = "PIB, Miles de millones (Q)",
             color = "Tipo de PIB") +
        theme_bw()+
        scale_color_manual(values = c("PIB_nominal" = "darkred", "PIB_real" = "darkblue"),
                           labels = c("PIB Nominal", "PIB Real"))+
        theme(
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"))
}





# 2. Función para gráfica PIB vs Inflación
grafica_pib_vs_inflacion <- function(datos, año_inicial, año_final) {
    datos_filtrados <- datos %>% filter(Anio >= año_inicial & Anio <= año_final)
    
    ggplot(datos_filtrados, aes(x = Fecha)) +
        geom_line(aes(y = PIB_nominal_var, color = "Crecimiento PIB"), size = 1.2) +
        geom_line(aes(y = Inf_medio_var, color = "Inflación"), size = 1.2) +
        labs(title = "Crecimiento PIB vs Inflación ",
             subtitle = paste(año_inicial, "a", año_final),
             x = "Año",
             y = "Tasa (%)",
             color = "Variable") +
        theme_bw()+
        scale_color_manual(values = c("Crecimiento PIB" = "darkgreen", 
                                      "Inflación" = "orange"))+
        theme(
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"))
}



# 3. Función para gráfica comparativa reciente
grafica_comparativa_reciente <- function(datos, año_inicial, año_final) {
    datos_filtrados <- datos %>% 
        filter(Anio >= año_inicial & Anio <= año_final) %>%
        tail(5)  # Últimos 5 años del rango
    
    datos_largos <- datos_filtrados %>%
        pivot_longer(cols = c(PIB_nominal_var, Inf_medio_var), 
                     names_to = "variable", 
                     values_to = "tasa") %>%
        mutate(variable = case_when(
            variable == "PIB_nominal_var" ~ "Crecimiento PIB",
            variable == "Inf_medio_var" ~ "Inflación"
        ))
    
    ggplot(datos_largos, aes(x = factor(Anio), y = tasa, fill = variable)) +
        geom_col(position = "dodge", alpha = 0.8) +
        labs(title = "Comparación PIB vs Inflación",
             subtitle = paste("Hasta", año_final),
             x = "Año",
             y = "Tasa (%)",
             fill = "Variable") +
        theme_bw()+
        scale_fill_manual(values = c("Crecimiento PIB" = "steelblue", 
                                     "Inflación" = "coral"))+
        theme(
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"))
}

# 4. Función para gráfica de brecha PIB-Inflación
grafica_brecha_pib_inflacion <- function(datos, año_inicial, año_final) {
    datos_filtrados <- datos %>% 
        filter(Anio >= año_inicial & Anio <= año_final) %>%
        mutate(brecha = PIB_nominal_var - Inf_medio_var)
    
    ggplot(datos_filtrados, aes(x = Fecha, y = brecha, fill = brecha > 0)) +
        geom_col(alpha = 0.7) +
        labs(title ="Brecha: Crecimiento PIB - Inflación",
             subtitle = paste( año_inicial, "a", año_final),
             x = "Año",
             y = "Diferencia (puntos porcentuales)") +
        theme_bw()+
        scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
        theme(
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"))
}



# 1. Función para gráfica de composición cuenta corriente
grafica_composicion_cuenta <- function(datos_cuenta, año_inicial, año_final) {
    datos_filtrados <- datos_cuenta %>% 
        filter(Anio >= año_inicial & Anio <= año_final)
    
    composicion <- datos_filtrados %>%
        filter(Descripción %in% c("A.1. BIENES", "A.2. SERVICIOS", 
                                  "A.3. INGRESO PRIMARIO", "A.4. INGRESO SECUNDARIO"))
    
    ggplot(composicion, aes(x = Anio, y = Valor/1000, color = Descripción)) +
        geom_line(size = 1.2) +
        labs(title = paste("Componentes Cuenta Corriente -", año_inicial, "a", año_final),
             subtitle = "En miles de millones de dólares",
             y = "Millones de USD (miles)",
             x="Año",
             color = "Componente") +
        # scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
        theme_bw()+
        theme(
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"))+
        scale_x_continuous(breaks = seq(año_inicial, año_final, by = ceiling((año_final - año_inicial)/5)))
}



# 2. Función para gráfica de cuenta corriente total
grafica_cuenta_corriente_total <- function(datos_cuenta, año_inicial, año_final) {
    datos_filtrados <- datos_cuenta %>% 
        filter(Anio >= año_inicial & Anio <= año_final)
    
    cuenta_corriente <- datos_filtrados %>%
        filter(Descripción == "A. CUENTA CORRIENTE")
    
    ggplot(cuenta_corriente, aes(x = Anio, y = Valor/1000)) +
        geom_line(color = "#2E86AB", size = 1.5) +
        geom_point(color = "#2E86AB", size = 2) +
        geom_area(fill = "#2E86AB", alpha = 0.2) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(title = paste("Cuenta Corriente Total -", año_inicial, "a", año_final),
             subtitle = "En miles de millones de dólares",
             y = "Millones de USD (miles)", 
             x = "Año") +
        theme_bw()+
        theme(
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"))
}




# 3. Función para gráfica de ingreso secundario
grafica_ingreso_secundario <- function(datos_cuenta, año_inicial, año_final) {
    datos_filtrados <- datos_cuenta %>% 
        filter(Anio >= año_inicial & Anio <= año_final)
    
    ingreso_secundario <- datos_filtrados %>%
        filter(Descripción %in% c("A.4.1. Transferencias personales (remesas de trabajadores netas)",
                                  "A.4.2. Otro ingreso secundario (neto)"))
    
    ggplot(ingreso_secundario, aes(x = Anio, y = Valor/1000, color = Descripción)) +
        geom_line(size = 1.2) +
        labs(title = paste("Ingreso Secundario -", año_inicial, "a", año_final),
             subtitle = "En miles de millones de dólares",
             y = "Millones de USD (miles)",
             color = "Tipo") +
        scale_color_manual(values = c("#7FC97F", "#BEAED4")) +
        theme_bw()+
        theme(
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"))
}




# 4. Función para gráfica facetada de variables principales
grafica_facet_variables <- function(datos_cuenta, año_inicial, año_final) {
    datos_filtrados <- datos_cuenta %>% 
        filter(Anio >= año_inicial & Anio <= año_final)
    
    variables_principales <- datos_filtrados %>%
        filter(Descripción %in% c( "A.1. BIENES", "A.2. SERVICIOS",
                                  "A.3. INGRESO PRIMARIO", "A.4. INGRESO SECUNDARIO"))
    
    ggplot(variables_principales, aes(x = Anio, y = Valor/1000)) +
        geom_line(color = "#2E86AB", size = 1) +
        geom_point(color = "#2E86AB", size = 0.8) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
        facet_wrap(~Descripción, scales = "free_y", ncol = 2) +
        labs(title = paste("Variables Principales -", año_inicial, "a", año_final),
             subtitle = "En miles de millones de dólares",
             y = "Millones de USD (miles)",
             x = "Año") +
        theme_bw()+
        theme(strip.text = element_text(face = "bold"),
              plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"))
}



# TABLAS
# 1. Tabla del PIB
tabla_PIB <- function(datos, año_inicial, año_final) {
    
    #  Filtrar datos según los años seleccionados
    datos_filtrados <- datos %>% 
        filter(Anio >= año_inicial & Anio <= año_final)
    
    # Calcular estadísticas resumen
    tabla_resumen <- datos_filtrados %>%
        summarise(
            Periodo = paste(año_inicial, "-", año_final),
            `Media PIB (%)` = round(mean(PIB_nominal_var, na.rm = TRUE), 2),
            `Media Inflación (%)` = round(mean(Inf_medio_var, na.rm = TRUE), 2),
            `Máximo PIB (%)` = round(max(PIB_nominal_var, na.rm = TRUE), 2),
            `Mínimo PIB (%)` = round(min(PIB_nominal_var, na.rm = TRUE), 2),
            `Correlación PIB-Inflación` = round(cor(PIB_nominal_var, Inf_medio_var, use = "complete.obs"), 3),
            `Años Analizados` = n()
        )
    
    # 4 Obtener valores del último año del rango
    ultimo_año <- max(datos_filtrados$Anio, na.rm = TRUE)
    valores_ultimo_año <- datos_filtrados %>%
        filter(Anio == ultimo_año) %>%
        select(Anio, PIB_nominal, PIB_real, Inf_medio_var)
    
    # 5 Extraer un valor específico (por ejemplo, PIB del último año)
    pibn_ultimo_año <- valores_ultimo_año$PIB_nominal[1]
    pibr_ultimo_año <- valores_ultimo_año$PIB_real[1]
    inf_ultimo_año  <- valores_ultimo_año$Inf_medio_var[1]
    
    # 6 Devolver todo en una lista
    resultado <- list(
        tabla = datos_filtrados,
        tabla_resumen = tabla_resumen,
        ultimo_año = valores_ultimo_año,
        pibn_ultimo_año = pibn_ultimo_año,
        pibr_ultimo_año = pibr_ultimo_año,
        inf_ultimo_año = inf_ultimo_año
    )
    
    return(resultado)
}


# Tabla de la cuenta corriente 
tabla_CC <- function(datos, año_inicial, año_final) {
    
    #  Filtrar datos según los años seleccionados
    datos_filtrados <- datos %>% 
        filter(Anio >= año_inicial & Anio <= año_final)
    
    return(datos_filtrados)
}
