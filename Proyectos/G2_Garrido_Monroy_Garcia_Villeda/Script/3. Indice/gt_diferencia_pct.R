library(tidyverse)
library(scales)
library(gt)

rm(list = ls())
graphics.off()
cat("\014")

setwd("C:\\Users\\danie\\Documents\\CursoProgra_2R\\programa-II\\Proyecto")
    
    
df_idx <- read_xlsx("output/idx.xlsx")

df_idx <- df_idx |>
    mutate( depto = recode(
        depto,
        `1`  = "Guatemala",
        `2`  = "El Progreso",
        `3`  = "Sacatepéquez",
        `4`  = "Chimaltenango",
        `5`  = "Escuintla",
        `6`  = "Santa Rosa",
        `7`  = "Sololá",
        `8`  = "Totonicapán",
        `9`  = "Quetzaltenango",
        `10` = "Suchitepéquez",
        `11` = "Retalhuleu",
        `12` = "San Marcos",
        `13` = "Huehuetenango",
        `14` = "Quiché",
        `15` = "Baja Verapaz",
        `16` = "Alta Verapaz",
        `17` = "Petén",
        `18` = "Izabal",
        `19` = "Zacapa",
        `20` = "Chiquimula",
        `21` = "Jalapa",
        `22` = "Jutiapa"
    )
    )

vector_guatemala <- df_idx |> filter(depto == "Guatemala")
vector_guatemala

df_idx <- df_idx |>
    group_by(depto) |>
    mutate(
        across(everything(), ~  round(.x * 100 /vector_guatemala[cur_column()], 2))
    )

# gt(df_idx) |>
#     tab_header(
#         title = "Brecha de desarrollo tecnológico en guatemala por departamento",
#         subtitle = "% usando guatemala como referencia"
#     )
gt(df_idx)

















