
# Cargar datos y librerías
#source("clean_data_anual.R") los datos se cargan de esta libreria pero no necesito cargarlo cada vez que se llama
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(ggspatial)
  


datos_top_paises <- function(anio,comerce,TOP) {
  #anio <-  2024
  fecha_inicio <- as.Date(paste0(anio, "-01-01"))
  fecha_fin <- as.Date(paste0(anio, "-12-31"))
  
  LISTA_TOP_PAISES <- socios |> 
    filter(fecha >= fecha_inicio & fecha <= fecha_fin) |> 
    filter(flujo == comerce) |> 
    arrange(desc(valor))
  
  LISTA_TOP_PAISES <- LISTA_TOP_PAISES |> 
    mutate(`Total_comerc` = sum(LISTA_TOP_PAISES$valor))
  p_LISTA_TOP_PAISES <- LISTA_TOP_PAISES |> 
    slice(1:TOP)
  
  return(p_LISTA_TOP_PAISES)
}



ranking_top_paises <- function(anio,comerce,TOP) {
  
  titulo_comerce <-ifelse(comerce=="Export","Exportado (Millones)","Importado (Millones)") 
  p_LISTA_TOP_PAISES <- datos_top_paises(anio,comerce,TOP)
  ranking <- p_LISTA_TOP_PAISES |> 
    mutate(Porcentaje = (valor/Total_comerc) * 100,
           Porcentaje = sprintf("%.2f%%", Porcentaje)) |> 
    mutate(Ranking = paste0("TOP ", row_number()), valor = valor/10^6) |>
    select(Ranking, Pais = socio, !!titulo_comerce := valor, Porcentaje)
  
  return(ranking)
}


  # Filtrar por año
mapa_top_paises <- function(anio,comerce,TOP) {
  
  p_LISTA_TOP_PAISES <- datos_top_paises(anio,comerce,TOP)
  if (nrow(p_LISTA_TOP_PAISES) == 0) {
    return(ggplot() + 
             annotate("text", x = 0, y = 0, label = "No hay datos para este año", size = 6) +
             theme_void())
  }
  
  p_LISTA_TOP_PAISES$socio[p_LISTA_TOP_PAISES$socio == "USA"] <- "United States of America"
  
  Top_paises <- p_LISTA_TOP_PAISES$socio
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Colorear los países
  world$color <- ifelse(world$name %in% Top_paises, world$name, "Otros")
  
  paleta <- c("red","orange","yellow","green","blue",
              "purple","pink","cyan","brown","magenta")
  colores_paises <- setNames(paleta, Top_paises)
  colores_paises <- c(colores_paises, "Otros" = "lightgray")
  
  etiquetas <- setNames(paste(1:length(Top_paises)), Top_paises)
  etiquetas <- c(etiquetas, "Otros" = "Otros")
  
  niveles <- c(Top_paises, "Otros")
  world$color <- factor(world$color, levels = niveles)
  
  ggplot(world) +
    geom_sf(aes(fill = color), color = "gray30", size = 0.2) +
    coord_sf(crs = '+proj=robin') +
    scale_fill_manual(values = colores_paises, labels = etiquetas) +
    theme_void() +
    labs(
      title = paste("Socios Comerciales de Guatemala en", anio),
      fill = "Top"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      #      legend.position = "bottom"
    )
}

# Ejemplo de uso:
mapa_top_paises(2024,"Export",5)
ranking_top_paises(2024,"Export",5)

