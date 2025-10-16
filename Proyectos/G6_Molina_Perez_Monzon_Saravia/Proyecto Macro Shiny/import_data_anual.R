#solo es necesario hacerlo una vez
#install.packages("comtradr")
#install.packages("tidyverse")

library(comtradr)
library(tidyverse)

set_primary_comtrade_key("Ingresar Key")
country_codes$country

gtm_exports = data.frame()
gtm_imports = data.frame()

#data de exportaciones desde 2010
for( i in 2015:2025) {
  new_data <- ct_get_data(
    reporter = 'GTM',
    partner = c( "World"),
    commodity_code = 'everything',
    start_date = i,
    end_date = i,
    flow_direction = 'export',
    frequency = "A"
  )
  gtm_exports = bind_rows(gtm_exports, new_data)
}
#data de importaciones desde 2010
for( i in 2015:2025) {
  new_data <- ct_get_data(
    reporter = 'GTM',
    partner = c( "World"),
    commodity_code = "everything",
    start_date = i,
    end_date = i,
    flow_direction = 'import',
    frequency = "A"
  )
  gtm_imports = bind_rows(gtm_imports, new_data)
}

gtm_trade_df <- bind_rows(gtm_exports, gtm_imports)

write.csv(x = gtm_trade_df ,file = "world_producto_anual.csv")

#solo es necesario hacerlo una vez
#install.packages("comtradr")
#install.packages("tidyverse")

gtm_exports = data.frame()
gtm_imports = data.frame()

#data de exportaciones desde 2010
for( i in 2015:2025) {
  new_data <- ct_get_data(
    reporter = 'GTM',
    partner = c( "all_countries"),
    commodity_code = 'TOTAL',
    start_date = i,
    end_date = i,
    flow_direction = 'export',
    frequency = "A"
  )
  gtm_exports = bind_rows(gtm_exports, new_data)
}
#data de importaciones desde 2010
for( i in 2015:2025) {
  new_data <- ct_get_data(
    reporter = 'GTM',
    partner = c( "all_countries"),
    commodity_code = "TOTAL",
    start_date = i,
    end_date = i,
    flow_direction = 'import',
    frequency = "A"
  )
  gtm_imports = bind_rows(gtm_imports, new_data)
}

gtm_trade_df <- bind_rows(gtm_exports, gtm_imports)

write.csv(x = gtm_trade_df ,file = "socio_total_anual.csv")

# 
# gtm_exports = data.frame()
# gtm_imports = data.frame()
# 
# #data de exportaciones desde 2010
# for( i in 2010:2013) {
#   new_data <- ct_get_data(
#     reporter = 'GTM',
#     partner = c( "all_countries"),
#     commodity_code = 'everything',
#     start_date = i,
#     end_date = i,
#     flow_direction = 'export',
#     frequency = "A"
#   )
#   gtm_exports = bind_rows(gtm_exports, new_data)
# }
# #data de importaciones desde 2010
# for( i in 2010:2013) {
#   new_data <- ct_get_data(
#     reporter = 'GTM',
#     partner = c( "all_countries"),
#     commodity_code = "everything",
#     start_date = i,
#     end_date = i,
#     flow_direction = 'import',
#     frequency = "A"
#   )
#   gtm_imports = bind_rows(gtm_imports, new_data)
# }
# 
# gtm_trade_df <- bind_rows(gtm_exports, gtm_imports)
# 
# write.csv(x = gtm_trade_df ,file = "world_producto_anual3.csv")
