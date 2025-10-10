library(readxl)
library(dplyr)
library(tidyverse)

graphics.off()
rm(list=ls())
cat("\014")
setwd("/Users/stefanivilleda/Desktop/Programación II/Proyecto/programa-II/Proyecto/")
#Carga de base Hogares

PIB <- read_excel("./input/PIB_per_capita.xlsx")


PIB<- PIB |>
    rename(
        departamento =`CÓDIGO DEPARTAMENTO`,
        depto = DEPARTAMENTO,
        PIB_per_capita = `PIB per cápita 2023 (US$ al año)`,
        poblacion= `Población (2023)`  
        )

PIB_per_capita<- PIB |>
    select(departamento,PIB_per_capita, poblacion)
PIB_per_capita

#Quitandole la primera fila que es el PIB per cpaita de la república
PIB_per_capita<- PIB_per_capita[-1, ]

write_csv(PIB_per_capita, "./output/PIB_per_capita.csv")


#Variable
PIB_per_capita_var<-PIB_per_capita$PIB_per_capita




