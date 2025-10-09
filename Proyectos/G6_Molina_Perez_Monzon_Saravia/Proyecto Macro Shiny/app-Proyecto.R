#VERSIÓN FINAL

# Carga de librerías -------------------------------------------------------------------------------

library(shiny)
library(bslib) #personalización aspecto visual usando framework Bootstrap
library(bsicons) #iconos de Bootstrap
library(tidyverse)
library(systemfonts)
library(scales) #de Tidyverse, formatea valores numéricos

# Llamado de funciones externas  -------------------------------------------------------------------
source("clean_data_anual.R")
source("Plot_Map.R")
source("analysis_pro.R")
source("Funciones.R")


# Definición de UI =================================================================================

ui <- page_fluid(

    # Personalización del tema -------------------------------------------------
    
    theme = bs_theme(
        version  =5, 
        bootswatch = "sandstone",
        base_font = "-apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif",
        heading_font = "Roboto, 'Segoe UI', Helvetica, Arial, sans-serif",
        primary = "#0C2C47", #Navy
        secondary = "#2D5E5E", # Green
        success = "#1E4859",   # Azul petróleo
        warning = "#E2A340",   # Yellow
        info = "#E7EAEA",      # Mint
        light = "#f8f9fa",     # gris claro
        dark = "#0C2C47"       # Navy
        
    ) |> bs_add_rules("
        body {
        font-size: 18px;
        }
        h2 { 
            background-color: var(--bs-primary); 
            color:white;
            padding: 6px 12px;
            border-radius: 4px;
            line-height: 1.3;
            text-align: center}
        h4 { 
            background-color: var(--bs-light);
            color: var(--bs-primary);
            padding: 6px 12px;
            line-height: 1.3;
            font-weight: 700;
            text-align: left}
            
        h6 {font-weight: bold;
            text-align: left}

        .card { background-color: var(--bs-light); }
        .card-header { 
                    padding: 4px 8px;
                    background-color: var(--bs-primary);
                    color:white;
                    text-align: center}
        .btn-primary { background-color:var(--bs-warning); border-color: var(--bs-warning); }
        .nav-tabs .nav-link.active { background-color: var(--bs-info); color: var(--bs-primary)}
        .bslib-value-box .bslib-value-box-value{font-size:4rem;font-weight:800;line-height:1.1; text-align: left;}
        .bslib-value-box .value-box-title { font-size: 1.5rem !important;}
        .bslib-value-box .bslib-value-box-showcase{font-size:84px; align-items: start !important;}
        .bslib-value-box{border-radius:14px}        
    "),
    
    
    h2("Comercio exterior y economía internacional: Guatemala"),
    
    #---
    layout_sidebar(

        # Configuración del Sidebar -------------------------------------------
        sidebar = sidebar(
            card(full_screen = TRUE,         
                card_header("Rango de años"),            
                sliderInput("slider_years", label = "", 
                            min = 2015, max = 2024, value = c(2015, 2024))),
            
            card(full_screen = TRUE,         
                card_header("Socios y productos"),            
                radioButtons(
                            inputId = "radio_mapas",
                            label = h6("Elegir modo: "),
                            choices = c("Exportaciones", "Importaciones"),
                            selected = NULL),
                
                sliderInput("slider_mapas", label = h6("Elegir año: "), 
                            min = 2015, max = 2024, value = 2015),

                selectizeInput(
                    inputId = "tops",
                    label = h6("Cantidad elementos en top"),
                    choices  = list("Top 3" = 3, "Top 5" = 5, "Top 10" = 10),
                    options = list(dropdownParent = "body"))),
            
        position = "left"),
        
        # Configuración de menús -----------------------------------------------
        navset_tab(id = "main",
            
    # Sección "Comercio exterior" ----------------------------------------------
            nav_panel("Comercio exterior",
                        navset_tab(
                            nav_panel("Balanza comercial",
                                    h4("Exportaciones e importaciones"),
                                      
                                    layout_columns(col_widths = c(4,4,4), gap = "16px",
                                                   value_box(title = textOutput("vb_saldo_title"),
                                                             value = textOutput("vb_saldo"),
                                                             showcase = bs_icon("currency-dollar"),
                                                             theme = "warning"),
                                                   
                                                   value_box(title = "Variación", 
                                                             value = textOutput("vb_variacion"),
                                                             showcase = bs_icon("percent"),
                                                             theme = "success")),
                                    card(
                                      card_header("Saldos y variación (%)"),
                                      plotOutput("tab_balanza", height = "320px")),

                                    # layout_columns(
                                    #                 div(
                                    #                     card(card_header("Exportaciones"),
                                    #                         plotOutput("grafico_exportaciones", height = "320px")),
                                    #                     card(card_header("Importaciones"),
                                    #                         plotOutput("grafico_importaciones", height = "320px"))
                                    #                     ))
                            ),
                                  
                            nav_panel("Principales socios",
                                    h4("Top principales socios"),
                                    layout_columns(
                                                    div(
                                                       card(card_header("Mapa Top Socios"),
                                                            plotOutput("grafico_socios", height = "320px")),
                                                       
                                                       card(card_header("Saldos y variación (%)"),
                                                            tableOutput("tab_socios"))
                                                   )),
                            ),
                            
                            nav_panel("Principales productos",
                                    h4("Top principales productos"),

                                    layout_columns(
                                                    div(
                                                        card(card_header("Gráfico top productos"),
                                                             plotOutput("grafico_productos", height = "320px")),
                                                        card(card_header("Saldos y variación (%)"),
                                                            tableOutput("tab_productos")),
                                                       
                                                         )),
                            )
                        )
            ),
    # Sección "Economía internacional" -----------------------------------------
    nav_panel("Economía internacional",
              navset_tab(
                nav_panel("PIB",
                          h4("PIB e Inflación"),
                          layout_columns(col_widths = c(4,4,4), gap = "16px",
                                         value_box(title = "PIB real", 
                                                   value = textOutput("vb_pibr"), 
                                                   showcase = tags$span("Q", style = "font-size: 70px; color: white"),
                                                   theme = "success"),
                                         
                                         value_box(title = "PIB nominal", 
                                                   value = textOutput("vb_pibn"), 
                                                   showcase = tags$span("Q", style = "font-size: 70px; color: white"),
                                                   theme = "warning"),
                                         
                                         value_box(title = "Inflación", 
                                                   value = textOutput("vb_inflacion"),
                                                   showcase = bs_icon("percent"),
                                                   theme = "success")),
                          div(
                            card(card_header("PIB real vs PIB nominal"),
                                 plotOutput("grafico_PIB", height = "320px")),
                            card(card_header("Crecimiento PIB vs Inflación"),
                                 plotOutput("grafico_PIBInf", height = "320px")),
                            card(card_header("Brecha PIB - Inflación"),
                                 plotOutput("grafico_PIBInf_brecha", height = "320px"))),
                ),
                nav_panel("Cuenta corriente",
                          
                          div(
                          layout_columns(col_widths = c(6,6), gap = "16px",
                                         card(card_header("Gráfica Cuenta Corriente"),
                                              plotOutput("grafico_CC", height = "320px")),
                                         card(card_header("Gráfica Variables principales"),
                                              plotOutput("grafico_CC_facet", height = "320px"))),
                          card(card_header("Tabla de resumen"),
                                          tableOutput("tab_cc")))
                          ),
                        )
            )
        )
    )
)


# Definición de server logic =======================================================================
                    
server <- function(input, output) {
    
    # Filtros a través de widgets ------------------------------------------------------------------
    
    
    year_range <- reactive({
        rng <- input$slider_years
        req(length(rng) == 2)
        rng
    })
    
    tot_annual_range <- reactive({
        req(tot_annual, year_range())
        filter(tot_annual, anio >= year_range()[1], anio <= year_range()[2])
    })
    
    map_year <- reactive({
        req(input$slider_mapas)
        input$slider_mapas
    })
    
    map_var <- reactive({
        req(input$radio_mapas)
        input$radio_mapas
    })
    
    top_n <- reactive({
        as.numeric(input$tops)
    })
    
    
     
    # Value Boxes (value) --------------------------------------------------------------------------
    
    output$vb_saldo <- renderText({
        pp <- prep_trade_data(gt_trade_world, socios)
        gt <- pp$gt
        so <- pp$so
        vecanio <- input$slider_years
        year_min <- as.numeric(vecanio[1])
        year_max <- as.numeric(vecanio[2])
        focus_year <- year_max 
        comercio <- ifelse(input$radio_mapas=="Exportaciones","Exportaciones","Importaciones")
        totals <- build_totals_annual(gt, so, year_min = year_min, year_max = year_max)
        SalExpo <- totals$table_last_yoy |> 
                    filter(Serie == comercio) |> 
                    pull(as.character(focus_year)) |> 
                    round(2)
        SalExpo <- paste0(number(SalExpo, big.mark = ",", accuracy = 0.1))
        SalExpo
        
    })   
    
    
    output$vb_saldo_title <- renderText({
      comercio <- ifelse(input$radio_mapas=="Exportaciones","Exportaciones","Importaciones")
      paste(comercio)
    })
    
    
    output$vb_variacion <- renderText({
        pp <- prep_trade_data(gt_trade_world, socios)
        gt <- pp$gt
        so <- pp$so
        vecanio <- input$slider_years
        year_min <- as.numeric(vecanio[1])
        year_max <- as.numeric(vecanio[2])
        focus_year <- year_max 
        comercio <- ifelse(input$radio_mapas=="Exportaciones","Exportaciones","Importaciones")
        totals <- build_totals_annual(gt, so, year_min = year_min, year_max = year_max)
        totals$table_last_yoy |> 
            filter(Serie == comercio) |> 
            pull(YoY_pct ) |> 
            round(2)
        
    })
    
    output$vb_export <- renderText({
        d <- tot_annual_range()
        req(nrow(d) > 0)
        ult <- max(d$anio, na.rm = TRUE)
        val <- d$Exportaciones[d$anio == ult]
        paste0(number(val, big.mark = ",", accuracy = 0.1), " millones USD")
    })
    
    output$vb_import <- renderText({
        d <- tot_annual_range()
        req(nrow(d) > 0)
        ult <- max(d$anio, na.rm = TRUE)
        val <- d$Importaciones[d$anio == ult]
        paste0(number(val, big.mark = ",", accuracy = 0.1), " millones USD")
    })
    
    output$vb_var_export <- renderText({
        d <- tot_annual_range()
        req(nrow(d) > 1)
        ult  <- max(d$anio, na.rm = TRUE)
        prev <- ult - 1
        actual <- d$Exportaciones[d$anio == ult]
        lag1 <- d$Exportaciones[d$anio == prev]
        v    <- 100 * (actual/ lag1 - 1)
        number(v, big.mark = ",", accuracy = 0.1, suffix = "%")
    })
    
    output$vb_var_import <- renderText({
        d <- tot_annual_range()
        req(nrow(d) > 1)
        ult  <- max(d$anio, na.rm = TRUE)
        prev <- ult - 1
        actual <- d$Importaciones[d$anio == ult]
        lag1 <- d$Importaciones[d$anio == prev]
        v    <- 100 * (actual/ lag1 - 1)
        number(v, big.mark = ",", accuracy = 0.1, suffix = "%")
    })
    
    output$vb_pibr <- renderText({
      anio_inicial <- input$slider_years[1]
      anio_final <-input$slider_years[2]
      resultado <- tabla_PIB(datosPIB,anio_inicial,anio_final)$pibr_ultimo_año
      
      paste0(number(resultado, big.mark = ",", accuracy = 0.3), " miles de millones")
    })
    
    output$vb_pibn <- renderText({
      anio_inicial <- input$slider_years[1]
      anio_final <-input$slider_years[2]
      resultado <- tabla_PIB(datosPIB,anio_inicial,anio_final)$pibn_ultimo_año
      
      paste0(number(resultado, big.mark = ",", accuracy = 0.3), " miles de millones")
    })
    
    output$vb_inflacion <- renderText({
      anio_inicial <- input$slider_years[1]
      anio_final <-input$slider_years[2]
      resultado <- tabla_PIB(datosPIB,anio_inicial,anio_final)$inf_ultimo_año
      
      number(resultado, accuracy = 0.1, suffix = "%")
    })
    
    # Mapa -----------------------------------------------------------------------------------------
    output$grafico_socios <- renderPlot({
        comercio <- ifelse(input$radio_mapas=="Exportaciones","Export","Import")
        anio <- as.numeric(input$slider_mapas)
        TOPS <- as.numeric(input$tops)
        mapa_top_paises(anio,comercio,TOPS)
    })
    
    output$tab_socios <- renderTable({
        comercio <- ifelse(input$radio_mapas=="Exportaciones","Export","Import")
        anio <- as.numeric(input$slider_mapas)
        TOPS <- as.numeric(input$tops)
        ranking_top_paises(anio,comercio,TOPS)
    })
    
    output$tab_balanza <- renderPlot({
        pp <- prep_trade_data(gt_trade_world, socios)
        gt <- pp$gt
        so <- pp$so
        vecanio <- input$slider_years
        year_min <- as.numeric(vecanio[1])
        year_max <- as.numeric(vecanio[2])
        focus_year <- year_max 
        totals <- build_totals_annual(gt, so, year_min = year_min, year_max = year_max)
        totals$plot_series
    })
    
    # DONA  --------------------------------------------------------------------------
    
    output$tab_productos <- renderTable({
        pp <- prep_trade_data(gt_trade_world, socios)
        gt <- pp$gt
        so <- pp$so
        TOPS <- as.numeric(input$tops)
        # agarra del otro lado
        year_max <- as.numeric(input$slider_mapas)
        top_prod <- build_top_products(gt, nivel = "2", focus_year = year_max , top_n = TOPS)
        # Importaciones por producto

        if(input$radio_mapas=="Exportaciones"){
            ST_produc <- top_prod$top_exp
        }
        else{
            ST_produc <- top_prod$top_imp
        }
        ST_produc
    })
    output$grafico_productos <- renderPlot({
        pp <- prep_trade_data(gt_trade_world, socios)
        gt <- pp$gt
        so <- pp$so
        TOPS <- as.numeric(input$tops)
        #agarra del otro slider
        year_max <- as.numeric(input$slider_mapas)
        top_prod <- build_top_products(gt, nivel = "2", focus_year = year_max, top_n = TOPS)
        if(input$radio_mapas=="Exportaciones"){
            dona_exp <- make_donut(
                top_prod$top_exp,
                categoria_col = "Producto",
                peso_col      = "peso_x",
                titulo        = paste0("Exportaciones por Producto (", top_prod$year_last, ")")
            )
            
            Donita <- dona_exp
        }
        else{
            dona_imp <- make_donut(
                top_prod$top_imp,
                categoria_col = "Producto",
                peso_col      = "peso_m",
                titulo        = paste0("Importaciones por Producto (", top_prod$year_last, ")")
            )
            
            Donita <-dona_imp
        }
        Donita
        
    })
    
    # Gráficas INTERNACIONAL -------------------------------------------------------------------------------------
    
    # Comparar PIB nominal vs real
    output$grafico_PIB <- renderPlot({
      anio_inicial <- input$slider_years[1]
      anio_final <-input$slider_years[2]
      grafica_pib_comparacion(datosPIB,anio_inicial,anio_final)
    })
    
    # Ver relación PIB–inflación # cambiar nombre
    output$grafico_PIBInf <- renderPlot({
      anio_inicial <- input$slider_years[1]
      anio_final <-input$slider_years[2]
      grafica_pib_vs_inflacion(datosPIB,anio_inicial,anio_final)
    })
    
    # Ver brecha PIB–inflación
    output$grafico_PIBInf_brecha <- renderPlot({
      anio_inicial <- input$slider_years[1]
      anio_final <-input$slider_years[2]
      grafica_brecha_pib_inflacion(datosPIB,anio_inicial,anio_final)
    })
    
    # Visualizar total cuenta corriente
    output$grafico_CC <- renderPlot({
      anio_inicial <- input$slider_years[1]
      anio_final <-input$slider_years[2]
      grafica_cuenta_corriente_total(datosCC,anio_inicial,anio_final)
    })
    
    # Ver detalle de ingreso secundario
    output$grafico_IS <- renderPlot({
      anio_inicial <- input$slider_years[1]
      anio_final <-input$slider_years[2]
      grafica_ingreso_secundario(datosCC,anio_inicial,anio_final)
    })
    
    # Facet de principales variables
    output$grafico_CC_facet <- renderPlot({
      anio_inicial <- input$slider_years[1]
      anio_final <-input$slider_years[2]
      grafica_facet_variables(datosCC,anio_inicial,anio_final)
    })
    
    # Tablas ---------------------------------------------------------
    
    output$tab_cc <- renderTable({
      anio_inicial <- input$slider_years[1]
      anio_final <-input$slider_years[2]
      tabla_CC(datosPIB,anio_inicial,anio_final)
    })
  
}

# Run the app ------------------------------------------------------------------
shinyApp(ui = ui, server = server)

