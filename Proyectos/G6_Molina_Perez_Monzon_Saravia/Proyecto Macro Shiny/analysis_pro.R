#mrk8_kamp
# ======================================================================
# DASHBOARD: Comercio Exterior Guatemala (con diccionarios completos)
# ======================================================================


library(shiny)
library(tidyverse)
library(lubridate)
library(kableExtra)
library(scales)
#source("clean_data_anual.R")
# ----------------------------------------------------------------------
# 0) HELPERS: normalización, diccionarios y estética
# ----------------------------------------------------------------------



# ── 0) Normalización común ─────────────────────────────────────────────────────
prep_trade_data <- function(gt_trade_world, socios) {
  gt <- gt_trade_world |>
    mutate(
      anio  = lubridate::year(fecha),
      valor = valor/1e6,
      flujo = recode(flujo, "Export"="Exportaciones", "Import"="Importaciones")
    ) |>
    drop_na(fecha)
  
  so <- socios |>
    mutate(
      anio  = lubridate::year(fecha),
      valor = valor/1e6,
      flujo = recode(flujo, "Export"="Exportaciones", "Import"="Importaciones")
    ) |>
    drop_na(fecha)
  
  list(gt = gt, so = so)
}

# ── 1) Etiquetas manuales de producto_simple (fácil de mantener) ──────────────
label_product_simple <- function(producto) {
  lookup <- c(
    # Fats, oils
    "Animals, vegetable or microbial fats and oils" =
      "Animal Fats & Oils",
    "Animal or vegetable fats and oils and their cleavage products; prepared animal fats; animal or vegetable waxes" =
      "Animal Fats & Others",
    "Vegetable fats and oils; crude"      = "Vegetable Oils (Crude)",
    "Vegetable fats and oils; refined"    = "Vegetable Oils (Refined)",
    
    # Metals
    "Iron or steel; non-threaded washers, excluding spring washers" = "Steel Washers",
    "Iron or steel; bars and rods, hot-rolled" = "Steel Bars & Rods",
    "Copper; refined and unwrought" = "Refined Copper",
    "Aluminium; unwrought"          = "Unwrought Aluminium",
    
    # Machinery
    "Machinery for packaging or wrapping"            = "Packaging Machinery",
    "Machinery for sorting, screening or separating" = "Sorting Machines",
    "Machinery for making paper or paperboard"       = "Paper Machinery",
    
    # Plastics & rubber
    "Plastics and articles thereof"                         = "Plastics",
    "Rubber; new pneumatic tyres, of a kind used on cars"   = "Car Tyres",
    "Rubber; new pneumatic tyres, of a kind used on buses or trucks" = "Truck Tyres",
    
    # Food & agriculture
    "Coffee, whether or not roasted"                    = "Coffee",
    "Sugar; cane or beet, raw"                          = "Raw Sugar",
    "Cocoa beans, whole or broken, raw or roasted"      = "Cocoa Beans",
    "Meat of bovine animals, fresh or chilled"          = "Beef (Fresh/Chilled)",
    "Bananas, including plantains, fresh or dried"      = "Bananas",
    "Fruit and nuts, edible; peel of citrus fruit or melons" = "Fruit & Nuts",
    
    # Minerals & fuels
    "Petroleum oils, crude"                                   = "Crude Oil",
    "Petroleum gases and other gaseous hydrocarbons"          = "Natural Gas",
    "Coal; anthracite"                                        = "Coal (Anthracite)",
    "Mineral fuels, mineral oils and products of their distillation; bituminous substances; mineral waxes" =
      "Mineral Fuels & Oils",
    
    # Paper & veg (corrección de tu código: estaban duplicados a "Paper and Paperboard")
    "Paper and paperboard; articles of paper pulp, of paper or paperboard" = "Paper & Paperboard",
    "Vegetables and certain roots and tubers; edible"                      = "Vegetables & Roots",
    
    # Textiles
    "Cotton; not carded or combed" = "Raw Cotton",
    "Wool, not carded or combed"   = "Raw Wool",
    "Apparel and clothing accessories; knitted or crocheted" = "Knitted Apparel",
    
    # Chemicals & pharma
    "Medicaments; consisting of mixed or unmixed products" = "Medicines",
    "Fertilizers; mineral or chemical"                     = "Fertilizers",
    "Soap; organic surface-active products"                = "Soap & Detergents",
    
    # Vehicles
    "Motor cars and other motor vehicles"          = "Cars",
    "Parts and accessories of motor vehicles"      = "Car Parts",
    "Tractors; agricultural"                       = "Tractors",
    
    # Electronics
    "Telephones for cellular networks or for other wireless networks" = "Mobile Phones",
    "Computers; automatic data processing machines"                   = "Computers",
    "Electronic integrated circuits"                                   = "Microchips",
    
    # Construction
    "Cement; portland, aluminous, slag, supersulphate" = "Cement",
    "Glass; safety glass"                              = "Safety Glass",
    "Tiles, ceramic"                                   = "Ceramic Tiles"
  )
  
  out <- lookup[producto]
  out[is.na(out)] <- stringr::str_to_title(stringr::word(producto[is.na(out)], 1, 5))
  unname(out)
}

# ----- Diccionario 2: etiqueta simple -> nombre bonito en español --------------
pretty_product_name <- function(x) {
  map <- c(
    "Animal Fats & Oils"       = "Grasas y Aceites (Anim./Veg.)",
    "Animal Fats & Others"     = "Grasas y Derivados (Anim./Veg.)",
    "Vegetable Oils (Crude)"   = "Aceites Vegetales (Crudos)",
    "Vegetable Oils (Refined)" = "Aceites Vegetales (Refinados)",
    "Steel Washers"            = "Arandelas de Acero",
    "Steel Bars & Rods"        = "Barras y Varillas de Acero",
    "Refined Copper"           = "Cobre Refinado",
    "Unwrought Aluminium"      = "Aluminio en Bruto",
    "Packaging Machinery"      = "Maquinaria de Empaque",
    "Sorting Machines"         = "Máquinas de Clasificación",
    "Paper Machinery"          = "Maquinaria Papelera",
    "Plastics"                 = "Plásticos (y Manufacturas)",
    "Car Tyres"                = "Llantas para Auto",
    "Truck Tyres"              = "Llantas para Camión/Bus",
    "Coffee"                   = "Café",
    "Raw Sugar"                = "Azúcar Cruda",
    "Cocoa Beans"              = "Cacao (Grano)",
    "Beef (Fresh/Chilled)"     = "Carne Bovina (Fresca/Refrig.)",
    "Bananas"                  = "Banano/Plátano",
    "Fruit & Nuts"             = "Frutas y Nueces",
    "Crude Oil"                = "Petróleo Crudo",
    "Natural Gas"              = "Gas Natural",
    "Coal (Anthracite)"        = "Carbón (Antracita)",
    "Mineral Fuels & Oils"     = "Combustibles Minerales y Aceites",
    "Paper & Paperboard"       = "Papel y Cartón",
    "Vegetables & Roots"       = "Hortalizas y Tubérculos",
    "Raw Cotton"               = "Algodón (en Rama)",
    "Raw Wool"                 = "Lana (en Rama)",
    "Knitted Apparel"          = "Prendas de Punto",
    "Medicines"                = "Medicamentos",
    "Fertilizers"              = "Fertilizantes",
    "Soap & Detergents"        = "Jabones y Tensioactivos",
    "Cars"                     = "Automóviles",
    "Car Parts"                = "Partes y Accesorios de Autos",
    "Tractors"                 = "Tractores",
    "Mobile Phones"            = "Teléfonos Móviles",
    "Computers"                = "Computadoras",
    "Microchips"               = "Circuitos Integrados",
    "Cement"                   = "Cemento",
    "Safety Glass"             = "Vidrio de Seguridad",
    "Ceramic Tiles"            = "Baldosas Cerámicas"
  )
  out <- unname(ifelse(!is.na(map[x]), map[x], x))
  out
}

# Etiquetas de columnas bonitas (para tablas)
nice_cols <- c(
  "anio" = "Año",
  "Exportaciones" = "Exportaciones (MM US$)",
  "Importaciones" = "Importaciones (MM US$)",
  "x_var_int" = "Crec. X YoY (%)",
  "i_var_int" = "Crec. M YoY (%)",
  "peso_x" = "Peso X (%)",
  "peso_m" = "Peso M (%)",
  "socio" = "Socio Comercial",
  "producto_simple" = "Producto (Nivel 2)",
  "producto_pretty" = "Producto (Nivel 2)"
)

prettify_cols <- function(df) {
  common <- intersect(names(df), names(nice_cols))
  names(df)[match(common, names(df))] <- unname(nice_cols[common])
  df
}

fmt_musd <- label_number(accuracy = 0.1, big.mark = ",")
fmt_pct  <- label_number(accuracy = 0.1, suffix = "%", big.mark = ",")

# Tema visual
theme_comex <- function(base_size = 11, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title      = element_text(face = "bold", size = base_size + 2),
      plot.subtitle   = element_text(color = "#555"),
      plot.caption    = element_text(size = base_size - 2, color = "#777"),
      axis.title.x    = element_text(margin = margin(t = 8)),
      axis.title.y    = element_text(margin = margin(r = 8)),
      legend.position = "bottom",
      panel.grid.minor= element_blank(),
      panel.grid.major= element_line(linewidth = 0.3, color = "#e7e7e7")
    )
}

# ----------------------------------------------------------------------
# 1) FUNCIONES DE PROCESAMIENTO (agregación robusta por año)
# ----------------------------------------------------------------------

# Serie anual (con rango)
build_totals_annual <- function(gt, so, year_min = NULL, year_max = NULL) {
  tot_annual <- bind_rows(
    gt |> select(anio, flujo, valor),
    so |> select(anio, flujo, valor)
  ) |>
    filter(flujo %in% c("Exportaciones", "Importaciones"))
  
  if (is.null(year_min)) year_min <- min(tot_annual$anio, na.rm = TRUE)
  if (is.null(year_max)) year_max <- max(tot_annual$anio, na.rm = TRUE)
  
  tot_annual <- tot_annual |>
    filter(anio %in% year_min:year_max) |>
    group_by(anio, flujo) |>
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = flujo, values_from = valor, values_fill = 0) |>
    arrange(anio)
  
  ult_anio  <- max(tot_annual$anio, na.rm = TRUE)
  prev_anio <- max(min(tot_annual$anio, na.rm = TRUE), ult_anio - 1)
  
  tmp <- tot_annual |>
    filter(anio %in% c(prev_anio, ult_anio)) |>
    pivot_longer(c(Exportaciones, Importaciones),
                 names_to = "Serie", values_to = "valor") |>
    pivot_wider(names_from = anio, values_from = valor)
  
  an_prev <- as.character(prev_anio); an_ult <- as.character(ult_anio)
  tmp$YoY_pct <- 100 * (tmp[[an_ult]] / pmax(tmp[[an_prev]], 1e-9) - 1)
  
  p <- ggplot(tot_annual, aes(anio)) +
    geom_line(aes(y = Exportaciones, color = "Exportaciones"), linewidth = 1.1) +
    geom_line(aes(y = Importaciones, color = "Importaciones"), linewidth = 1.1) +
    geom_point(aes(y = Exportaciones, color = "Exportaciones")) +
    geom_point(aes(y = Importaciones, color = "Importaciones")) +
    scale_color_manual(values = c("Exportaciones" = "#1f77b4", "Importaciones" = "#d62728")) +
    scale_y_continuous(labels = fmt_musd) +
    labs(title = "Guatemala — Comercio Exterior (Anual)",
         subtitle = "Exportaciones e Importaciones (MM US$)",
         x = "Año", y = "Millones de US$",
         color = "Serie", caption = "Fuente: UN Comtrade") +
    theme_comex()
  
  list(table_last_yoy = tmp, plot_series = p, data = tot_annual)
}

# Top-10 socios (agregado por socio-año antes de YoY/pesos)
build_top_partners <- function(so, focus_year, top_n = 10) {
  soc_agg <- so |>
    group_by(socio, anio, flujo) |>
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = flujo, values_from = valor, values_fill = 0) |>
    arrange(socio, anio) |>
    group_by(socio) |>
    mutate(
      x_var_int = 100 * (Exportaciones / lag(Exportaciones) - 1),
      i_var_int = 100 * (Importaciones / lag(Importaciones) - 1)
    ) |>
    ungroup() |>
    group_by(anio) |>
    mutate(
      total_export = sum(Exportaciones, na.rm = TRUE),
      total_import = sum(Importaciones, na.rm = TRUE),
      peso_x = 100 * Exportaciones / pmax(total_export, 1e-9),
      peso_m = 100 * Importaciones / pmax(total_import, 1e-9)
    ) |>
    ungroup()
  
  exp_tbl <- soc_agg |>
    filter(anio == focus_year) |>
    select(socio, Exportaciones, x_var_int, peso_x) |>
    slice_max(order_by = Exportaciones, n = top_n)
  
  imp_tbl <- soc_agg |>
    filter(anio == focus_year) |>
    select(socio, Importaciones, i_var_int, peso_m) |>
    slice_max(order_by = Importaciones, n = top_n)
  
  list(top_exp = exp_tbl, top_imp = imp_tbl, year_last = focus_year)
}

# Top-10 productos (nivel, con diccionarios; agrega por producto_simple-año)
build_top_products <- function(gt, nivel = "2", focus_year, top_n = 10) {
  prod_agg <- gt |>
    filter(nivel == 2) |>
    mutate(producto_simple = producto) |> 
    pivot_wider(names_from = flujo, values_from = valor, values_fill = 0) |>
    arrange(producto_simple, anio) |>
    group_by(producto_simple) |>
    mutate(
      x_var_int = 100 * (Exportaciones / lag(Exportaciones) - 1),
      i_var_int = 100 * (Importaciones / lag(Importaciones) - 1)) |>
    ungroup() |>
    group_by(anio) |>
    mutate(
      total_export = sum(Exportaciones, na.rm = TRUE),
      total_import = sum(Importaciones, na.rm = TRUE),
      peso_x = 100 * Exportaciones / pmax(total_export, 1e-9),
      peso_m = 100 * Importaciones / pmax(total_import, 1e-9)
    ) |>
    ungroup() |>
    mutate(producto_pretty = pretty_product_name(producto_simple))
  
  top_exp <- prod_agg |>
    filter(anio == focus_year) |>
    select(producto_pretty, Exportaciones, x_var_int, peso_x) |>
    group_by(producto_pretty) |>
    summarise(across(c(Exportaciones, x_var_int, peso_x), ~sum(.x, na.rm = TRUE)), .groups = "drop") |>
    rename(Producto = producto_pretty) |> 
    slice_max(order_by = Exportaciones, n = top_n)
  
  top_imp <- prod_agg |>
    filter(anio == focus_year) |>
    select(producto_pretty, Importaciones, i_var_int, peso_m) |>
    group_by( producto_pretty) |>
    summarise(across(c(Importaciones, i_var_int, peso_m), ~sum(.x, na.rm = TRUE)), .groups = "drop") |>
    rename(Producto = producto_pretty) |> 
    slice_max(order_by = Importaciones, n = top_n)
  
  list(top_exp = top_exp, top_imp = top_imp, year_last = focus_year)
}

# Donas (usa nombres bonitos si están disponibles)
make_donut <- function(tbl, categoria_col, peso_col, titulo = NULL) {
  df <- tbl |>
    select(all_of(c(categoria_col, peso_col))) |>
    rename(Categoria = !!categoria_col, Peso = !!peso_col) |>
    mutate(Peso = pmax(Peso, 0)) |>
    arrange(desc(Peso))
  
  suma <- sum(df$Peso, na.rm = TRUE)
  df$Peso <- 100 * df$Peso / pmax(suma, 1e-9)
  delta <- 100 - sum(df$Peso)
  if (abs(delta) > 1e-6) df <- bind_rows(df, tibble(Categoria = "Otros", Peso = delta))
  
  ggplot(df, aes(x = 2, y = Peso, fill = Categoria)) +
    geom_col(width = 1) +
    coord_polar(theta = "y") +
    xlim(0.5, 2.5) +
    scale_y_continuous(labels = fmt_pct) +
    labs(title = titulo, fill = "Categoría") +
    theme_void() + 
    theme_comex() +
    theme(
      legend.position = "right",   # coloca la leyenda a la derecha
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 10, face = "bold")
      )
}

# ----------------------------------------------------------------------
# 2) UI SHINY
# ----------------------------------------------------------------------
# ui <- fluidPage(
#   titlePanel("Comercio Exterior de Guatemala"),
#   sidebarLayout(
#     sidebarPanel(
#       uiOutput("year_range_ui"),
#       uiOutput("focus_year_ui"),
#       selectInput("nivel", "Nivel de producto",
#                   choices = c("0","1","2","3","4","5","6"), selected = "2")
#     ),
#     mainPanel(
#       tabsetPanel(
#         tabPanel("Series Totales",
#                  plotOutput("p_series"),
#                  tableOutput("tabla_yoy")),
#         tabPanel("Socios (Top-10)",
#                  h4(textOutput("anio_soc")),
#                  h5("Exportaciones"), tableOutput("tbl_soc_exp"),
#                  h5("Importaciones"), tableOutput("tbl_soc_imp")),
#         tabPanel("Productos (Top-10)",
#                  h4(textOutput("anio_prod")),
#                  h5("Exportaciones"), tableOutput("tbl_prod_exp"),
#                  h5("Importaciones"), tableOutput("tbl_prod_imp"),
#                  plotOutput("p_dona_exp"), plotOutput("p_dona_imp"))
#       )
#     )
#   )
# )

# ----------------------------------------------------------------------
# 3) SERVER SHINY
# ----------------------------------------------------------------------
# server <- function(input, output, session) {
#   # Carga inicial (usa tus data frames ya en memoria: gt_trade_world, socios)
#   pp <- prep_trade_data(gt_trade_world, socios)
#   gt <- pp$gt; so <- pp$so
#   
#   year_min_all <- min(c(gt$anio, so$anio), na.rm = TRUE)
#   year_max_all <- max(c(gt$anio, so$anio), na.rm = TRUE)
#   
#   output$year_range_ui <- renderUI({
#     sliderInput("year_range", "Rango de años",
#                 min = year_min_all, max = year_max_all,
#                 value = c(max(year_min_all, year_max_all - 10), year_max_all),
#                 step = 1, sep = "")
#   })
#   
#   output$focus_year_ui <- renderUI({
#     req(input$year_range)
#     yrs <- seq(input$year_range[1], input$year_range[2])
#     selectInput("focus_year", "Año de enfoque", choices = yrs, selected = max(yrs))
#   })
#   
#   totals <- reactive({
#     req(input$year_range)
#     build_totals_annual(gt, so, input$year_range[1], input$year_range[2])
#   })
#   output$p_series  <- renderPlot({ totals()$plot_series })
#   output$tabla_yoy <- renderTable({ prettify_cols(totals()$table_last_yoy) }, digits = 1)
#   
#   top_soc <- reactive({
#     req(input$focus_year)
#     build_top_partners(so, input$focus_year, top_n = 10)
#   })
#   output$anio_soc    <- renderText({ paste("Año:", top_soc()$year_last) })
#   output$tbl_soc_exp <- renderTable({ prettify_cols(top_soc()$top_exp) }, digits = 1)
#   output$tbl_soc_imp <- renderTable({ prettify_cols(top_soc()$top_imp) }, digits = 1)
#   
#   top_prod <- reactive({
#     req(input$focus_year, input$nivel)
#     build_top_products(gt, nivel = input$nivel, focus_year = input$focus_year, top_n = 10)
#   })
#   output$anio_prod    <- renderText({ paste("Año:", top_prod()$year_last) })
#   output$tbl_prod_exp <- renderTable({
#     top_prod()$top_exp |>
#       mutate(producto_pretty = pretty_product_name(producto_pretty)) |>
#       select(producto_pretty, Exportaciones, x_var_int, peso_x) |>
#       prettify_cols()
#   }, digits = 1)
#   output$tbl_prod_imp <- renderTable({
#     top_prod()$top_imp |>
#       mutate(producto_pretty = pretty_product_name(producto_pretty)) |>
#       select(producto_pretty, Importaciones, i_var_int, peso_m) |>
#       prettify_cols()
#   }, digits = 1)
#   
#   output$p_dona_exp <- renderPlot({
#     make_donut(
#       top_prod()$top_exp |> mutate(producto_pretty = pretty_product_name(producto_pretty)),
#       "producto_pretty", "peso_x",
#       paste0("Exportaciones por Producto (", top_prod()$year_last, ")")
#     )
#   })
#   output$p_dona_imp <- renderPlot({
#     make_donut(
#       top_prod()$top_imp |> mutate(producto_pretty = pretty_product_name(producto_pretty)),
#       "producto_pretty", "peso_m",
#       paste0("Importaciones por Producto (", top_prod()$year_last, ")")
#     )
#   })
# }

# ----------------------------------------------------------------------
# 4) EJECUTAR
# ----------------------------------------------------------------------
# shinyApp(ui, server)

