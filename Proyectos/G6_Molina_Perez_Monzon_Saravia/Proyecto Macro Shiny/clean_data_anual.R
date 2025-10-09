library(tidyverse)

# Comercio Total ANUAL ----------------------------------------------------------


gt_trade_world <- read_csv("world_producto_anual.csv") |> 
  select(
    "ref_period_id", 
    "flow_desc",
    "partner_desc",
    "cmd_code",
    "cmd_desc",
    "aggr_level",
    "net_wgt",
    "gross_wgt",
    "primary_value"
  ) |>
  mutate(fecha = as.Date(as.character(ref_period_id), "%Y%m%d")) |>
  rename(
    flujo = flow_desc,
    socio = partner_desc,
    codigo = cmd_code,
    producto = cmd_desc,
    nivel = aggr_level,
    peso_neto = net_wgt,
    peso_bruto = gross_wgt,
    valor = primary_value
  ) |>
  select(fecha, flujo, socio, codigo, producto, nivel, peso_neto, peso_bruto, valor)



# SOCIOS ANUAL ------------------------------------------------------------------



socios <- read_csv("socio_total_anual.csv") |> 
  select(
    "ref_period_id", 
    "flow_desc",
    "partner_desc",
    "cmd_code",
    "cmd_desc",
    "aggr_level",
    "net_wgt",
    "gross_wgt",
    "primary_value"
  ) |>
  mutate(fecha = as.Date(as.character(ref_period_id), "%Y%m%d")) |>
  rename(
    flujo = flow_desc,
    socio = partner_desc,
    codigo = cmd_code,
    producto = cmd_desc,
    nivel = aggr_level,
    peso_neto = net_wgt,
    peso_bruto = gross_wgt,
    valor = primary_value
  ) |>
  select(fecha, flujo, socio, codigo, producto, nivel, peso_neto, peso_bruto, valor)



