# ============================================================================
# enfoque_pais.R - Módulo de Análisis por País
# Shock a un país proveedor → impacto en todos los productos
# ============================================================================

# ============================================================================
# UI
# ============================================================================

enfoque_pais_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(class = "container-fluid mt-4",
        
        fluidRow(
          column(12,
                 h2(class = "section-title", "🌍 Análisis de Shock por País"),
                 div(class = "info-box",
                     p(strong("Seleccione un país proveedor"), " y simule una caída en las importaciones procedentes de él. ",
                       "Se analiza el impacto agregado sobre todos los productos que España importa de ese país."),
                     p(style = "margin-top: 8px; margin-bottom: 0;",
                       tags$em("Los resultados se muestran a nivel HS6 (6 dígitos del Sistema Armonizado), ",
                               "el máximo nivel de desagregación armonizado internacionalmente."))
                 )
          )
        ),
        
        # Configuración
        fluidRow(
          column(4,
                 card(
                   card_header(class = "bg-light", "🎯 Configuración del Shock"),
                   card_body(
                     selectizeInput(ns("pais"), "País proveedor:",
                                    choices = NULL, width = "100%",
                                    options = list(placeholder = "Escriba para buscar país...")),
                     sliderInput(ns("magnitud"), "Caída en importaciones:",
                                 min = 0, max = 100, value = 50, step = 5, post = "%"),
                     hr(),
                     h6(tags$strong("🌐 Alcance geográfico del shock")),
                     div(class = "text-muted small mb-2",
                         "¿Este shock afecta igual a todos los países de la UE o solo a España?"),
                     radioButtons(ns("alcance"), NULL,
                                  choices = c(
                                    "Igual para toda la UE" = "global",
                                    "Específico España" = "espana",
                                    "Personalizado (España vs resto UE)" = "custom"
                                  ),
                                  selected = "global"),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'custom'", ns("alcance")),
                       sliderInput(ns("mag_ue"), "Caída resto UE:",
                                   min = 0, max = 100, value = 20, step = 5, post = "%")
                     ),
                     hr(),
                     actionButton(ns("aplicar"), "⚡ Aplicar Shock", class = "btn btn-primary btn-lg w-100")
                   )
                 )
          ),
          column(8,
                 # Métricas resumen
                 fluidRow(
                   column(3, div(class = "metric-card",
                                 shinycssloaders::withSpinner(p(class = "metric-value", textOutput(ns("total_pais")))),
                                 p(class = "metric-label", "Import. desde País (12m)"))),
                   column(3, div(class = "metric-card",
                                 shinycssloaders::withSpinner(p(class = "metric-value", textOutput(ns("cuota_pais")))),
                                 p(class = "metric-label", "% sobre Total Import."))),
                   column(3, div(class = "metric-card",
                                 shinycssloaders::withSpinner(p(class = "metric-value", textOutput(ns("num_productos")))),
                                 p(class = "metric-label", "Productos Afectados"))),
                   column(3, div(class = "metric-card",
                                 shinycssloaders::withSpinner(p(class = "metric-value", textOutput(ns("perdida_total")))),
                                 p(class = "metric-label", "Pérdida Estimada")))
                 )
          )
        ),
        
        # Gráficos
        fluidRow(
          column(6,
                 card(
                   card_header(class = "bg-light", "📊 Top 20 Productos más Afectados (HS6)"),
                   card_body(
                     div(class = "text-muted small mb-2",
                         "Productos con mayor pérdida absoluta en euros. Indica dónde se concentra el impacto económico del shock."),
                     shinycssloaders::withSpinner(
                       plotlyOutput(ns("grafico_top_productos"), height = "500px")
                     )
                   )
                 )
          ),
          column(6,
                 card(
                   card_header(class = "bg-light", "📊 Dependencia por Producto del País (HS6)"),
                   card_body(
                     div(class = "text-muted small mb-2",
                         "Porcentaje que representan las importaciones desde este país sobre el total importado de cada producto. ",
                         tags$span(style = "color: #F44336; font-weight: 600;", ">50%"), " riesgo alto, ",
                         tags$span(style = "color: #FF9800; font-weight: 600;", "25-50%"), " riesgo medio, ",
                         tags$span(style = "color: #90CAF9; font-weight: 600;", "<25%"), " riesgo bajo."),
                     shinycssloaders::withSpinner(
                       plotlyOutput(ns("grafico_dependencia"), height = "500px")
                     )
                   )
                 )
          )
        ),
        
        # Tabla detallada
        fluidRow(
          column(12,
                 card(
                   card_header(class = "bg-light", "📋 Detalle de Productos Afectados (HS6)"),
                   card_body(
                     div(class = "text-muted small mb-2",
                         "Tabla completa de productos a nivel HS6. ",
                         tags$strong("Import. País:"), " valor importado desde el país seleccionado (12 meses). ",
                         tags$strong("Import. Total:"), " importaciones de ese producto desde todos los orígenes. ",
                         tags$strong("Dependencia:"), " cuota del país sobre el total. ",
                         tags$strong("Pérdida Estimada:"), " impacto directo según el % de shock configurado."),
                     div(style = "margin-bottom: 10px;",
                         downloadButton(ns("descargar_productos"), "📥 Descargar Excel", class = "btn btn-sm btn-outline-secondary")
                     ),
                     shinycssloaders::withSpinner(DTOutput(ns("tabla_productos")))
                   )
                 )
          )
        )
    )
  )
}

# ============================================================================
# SERVER
# ============================================================================

enfoque_pais_server <- function(id, conexion_db) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ------------------------------------------------------------------
    # 1. Cargar lista de países proveedores
    # ------------------------------------------------------------------
    paises_disponibles <- reactive({
      req(conexion_db)
      
      query <- "
        SELECT iso3a, SUM(euros) as total
        FROM datacomex.taric_codigo_iso3a
        WHERE flujo = 'I'
        AND fecha >= (SELECT MAX(fecha) FROM datacomex.taric_codigo_iso3a WHERE flujo = 'I') - INTERVAL '11 months'
        GROUP BY iso3a
        ORDER BY total DESC"
      
      dbGetQuery(conexion_db, query)
    })
    
    observe({
      req(paises_disponibles())
      df <- paises_disponibles()
      choices <- setNames(df$iso3a, df$iso3a)
      updateSelectizeInput(session, "pais", choices = c("Seleccione país..." = "", choices), server = TRUE)
    })
    
    # ------------------------------------------------------------------
    # 2. Obtener fecha máxima y total general
    # ------------------------------------------------------------------
    info_fechas <- reactive({
      req(conexion_db)
      res <- dbGetQuery(conexion_db, "SELECT MAX(fecha) as f FROM datacomex.taric_codigo_iso3a WHERE flujo = 'I'")
      fecha_max <- as.Date(res$f[1])
      fecha_desde <- fecha_max %m-% months(11)
      list(desde = fecha_desde, hasta = fecha_max)
    })
    
    total_general <- reactive({
      req(info_fechas(), conexion_db)
      info <- info_fechas()
      query <- sprintf(
        "SELECT SUM(euros) as total FROM datacomex.taric_codigo_iso3a WHERE flujo = 'I' AND fecha >= '%s' AND fecha <= '%s'",
        info$desde, info$hasta)
      res <- dbGetQuery(conexion_db, query)
      res$total[1]
    })
    
    # ------------------------------------------------------------------
    # 3. Datos del país seleccionado (todos los productos)
    # ------------------------------------------------------------------
    datos_pais <- reactive({
      req(input$pais, nchar(input$pais) > 0, info_fechas(), conexion_db)
      info <- info_fechas()
      
      id_notif <- showNotification("Consultando datos del país...", duration = NULL, type = "message")
      on.exit(removeNotification(id_notif))
      
      raw <- comerciotools::cargar_pg_datacomex(
        dataset = "taric",
        iso3a = input$pais,
        flujo = "I",
        desde = info$desde,
        hasta = info$hasta,
        .conexion_db = conexion_db
      )
      
      if(is.null(raw) || nrow(raw) == 0) return(NULL)
      
      # Filtrar solo códigos HS6 (exactamente 6 dígitos)
      raw <- raw %>% filter(nchar(codigo) == 6)
      if(nrow(raw) == 0) return(NULL)
      
      # Agregar por producto HS6
      raw %>%
        group_by(codigo) %>%
        summarise(valor_12m = sum(euros, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(valor_12m))
    })
    
    # Total por producto (de todos los orígenes) para calcular dependencia
    datos_totales_por_producto <- reactive({
      req(datos_pais(), info_fechas(), conexion_db)
      info <- info_fechas()
      
      codigos <- datos_pais()$codigo
      if(length(codigos) == 0) return(NULL)
      
      # Query total by product HS6 (all origins)
      query <- sprintf(
        "SELECT codigo, SUM(euros) as total_producto
         FROM datacomex.taric_codigo_iso3a
         WHERE flujo = 'I' AND fecha >= '%s' AND fecha <= '%s'
         AND LENGTH(codigo) = 6
         AND codigo IN (%s)
         GROUP BY codigo",
        info$desde, info$hasta,
        paste0("'", codigos, "'", collapse = ","))
      
      dbGetQuery(conexion_db, query)
    })
    
    # ------------------------------------------------------------------
    # 4. Datos consolidados
    # ------------------------------------------------------------------
    datos_consolidados <- reactive({
      req(datos_pais(), datos_totales_por_producto())
      
      dp <- datos_pais()
      dt <- datos_totales_por_producto()
      
      dp %>%
        left_join(dt, by = "codigo") %>%
        mutate(
          total_producto = coalesce(total_producto, valor_12m),
          dependencia_pct = (valor_12m / total_producto) * 100
        ) %>%
        arrange(desc(valor_12m))
    })
    
    # ------------------------------------------------------------------
    # 5. Shock results (calculated on button click)
    # ------------------------------------------------------------------
    shock_result <- reactiveVal(NULL)
    
    observeEvent(input$aplicar, {
      req(datos_consolidados(), total_general(), input$pais)
      
      dc <- datos_consolidados()
      mag <- input$magnitud / 100
      tg <- total_general()
      
      total_pais <- sum(dc$valor_12m, na.rm = TRUE)
      perdida <- total_pais * mag
      
      # Calcular magnitudes diferenciadas según alcance
      alcance <- input$alcance
      mag_espana <- input$magnitud
      if(alcance == "global") {
        mag_ue <- mag_espana
      } else if(alcance == "espana") {
        mag_ue <- 0
      } else {
        mag_ue <- input$mag_ue
      }
      
      shock_result(list(
        pais = input$pais,
        magnitud = input$magnitud,
        alcance = alcance,
        mag_espana = mag_espana,
        mag_ue = mag_ue,
        total_pais = total_pais,
        cuota_pais = (total_pais / tg) * 100,
        num_productos = nrow(dc),
        perdida = perdida,
        datos = dc %>% mutate(
          perdida_producto = valor_12m * mag,
          valor_post = valor_12m * (1 - mag)
        )
      ))
    })
    
    # ------------------------------------------------------------------
    # OUTPUTS
    # ------------------------------------------------------------------
    
    output$total_pais <- renderText({
      req(shock_result())
      scales::number(shock_result()$total_pais, scale = 1e-6, suffix = "M€", accuracy = 0.1)
    })
    
    output$cuota_pais <- renderText({
      req(shock_result())
      paste0(round(shock_result()$cuota_pais, 2), "%")
    })
    
    output$num_productos <- renderText({
      req(shock_result())
      scales::number(shock_result()$num_productos, big.mark = ".")
    })
    
    output$perdida_total <- renderText({
      req(shock_result())
      paste0("-", scales::number(shock_result()$perdida, scale = 1e-6, suffix = "M€", accuracy = 0.1))
    })
    
    # Top 20 productos más afectados (barras horizontales)
    output$grafico_top_productos <- renderPlotly({
      req(shock_result())
      
      df <- shock_result()$datos %>%
        head(20) %>%
        mutate(perdida_m = perdida_producto / 1e6)
      
      plot_ly(df, y = ~reorder(codigo, perdida_producto), x = ~perdida_m,
              type = "bar", orientation = "h",
              marker = list(color = "#F44336"),
              text = ~paste0(round(perdida_m, 1), "M€"),
              textposition = "auto") %>%
        layout(
          title = paste0("Pérdida por producto (shock ", shock_result()$magnitud, "% a ", shock_result()$pais, ")"),
          xaxis = list(title = "Pérdida (M€)"),
          yaxis = list(title = "", tickfont = list(size = 10)),
          margin = list(l = 100)
        )
    })
    
    # Dependencia del país por producto
    output$grafico_dependencia <- renderPlotly({
      req(shock_result())
      
      df <- shock_result()$datos %>%
        arrange(desc(dependencia_pct)) %>%
        head(20)
      
      colors <- ifelse(df$dependencia_pct > 50, "#F44336", ifelse(df$dependencia_pct > 25, "#FF9800", "#90CAF9"))
      
      plot_ly(df, y = ~reorder(codigo, dependencia_pct), x = ~dependencia_pct,
              type = "bar", orientation = "h",
              marker = list(color = colors),
              text = ~paste0(round(dependencia_pct, 1), "%"),
              textposition = "auto") %>%
        layout(
          title = paste0("Dependencia de ", shock_result()$pais, " (% import. producto)"),
          xaxis = list(title = "Dependencia (%)", range = c(0, 100)),
          yaxis = list(title = "", tickfont = list(size = 10)),
          margin = list(l = 100)
        )
    })
    
    # Tabla detallada
    output$tabla_productos <- renderDT({
      req(shock_result())
      
      shock_result()$datos %>%
        select(
          Producto = codigo,
          `Import. País (12m)` = valor_12m,
          `Import. Total Producto` = total_producto,
          `Dependencia %` = dependencia_pct,
          `Pérdida Estimada` = perdida_producto
        ) %>%
        datatable(
          options = list(pageLength = 15, dom = 'ftip', order = list(list(1, 'desc'))),
          rownames = FALSE
        ) %>%
        formatCurrency(c(2, 3, 5), "€", digits = 0) %>%
        formatRound(4, 1)
    })
    
    output$descargar_productos <- downloadHandler(
      filename = function() {
        req(shock_result())
        paste0("enfoque_pais_", shock_result()$pais, "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        req(shock_result())
        df <- shock_result()$datos %>%
          select(
            Producto = codigo,
            `Import. País (12m)` = valor_12m,
            `Import. Total Producto` = total_producto,
            `Dependencia %` = dependencia_pct,
            `Pérdida Estimada` = perdida_producto
          )
        writexl::write_xlsx(df, file)
      }
    )
    
  })
}
