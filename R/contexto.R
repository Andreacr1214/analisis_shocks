contexto_producto_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(class = "container-fluid mt-4",
        fluidRow(
          column(12,
                 h2(class = "section-title", "Contexto del Producto"),
                 p(class = "lead", "Seleccione uno o varios productos TARIC (HS2, HS4 o HS6) para analizar su vulnerabilidad.")
          )
        ),
        
        fluidRow(
          column(12,
                 card(
                   card_header(class = "bg-light", "🔍 Seleccionar Producto(s)"),
                   card_body(
                     fluidRow(
                       column(9,
                              selectizeInput(
                                ns("producto"),
                                "Códigos TARIC (cualquier nivel):",
                                choices = NULL,
                                multiple = TRUE,
                                width = "100%",
                                options = list(
                                  placeholder = 'Escriba o seleccione códigos...',
                                  maxOptions = 100
                                )
                              )
                       ),
                       column(3, style = "padding-top: 25px;",
                              actionButton(ns("buscar"), "🔍 Buscar", class = "btn btn-primary btn-lg w-100")
                       )
                     ),
                     uiOutput(ns("overlap_warning")),
                     hr(),
                     uiOutput(ns("producto_info"))
                   )
                 )
          )
        ),
        
        # Métricas (ahora con 5 tarjetas)
        fluidRow(
          column(width = 2, offset = 1,
                 div(class = "metric-card", 
                     shinycssloaders::withSpinner(p(class = "metric-value", textOutput(ns("total_imports")))), 
                     p(class = "metric-label", "Importaciones (12m)"))),
          column(2, div(class = "metric-card", 
                        shinycssloaders::withSpinner(p(class = "metric-value", textOutput(ns("num_proveedores")))), 
                        p(class = "metric-label", "Países Proveedores"))),
          column(2, div(class = "metric-card", 
                        shinycssloaders::withSpinner(p(class = "metric-value", textOutput(ns("top_proveedor")))), 
                        p(class = "metric-label", "Principal Proveedor"))),
          column(2, div(class = "metric-card", 
                        shinycssloaders::withSpinner(p(class = "metric-value", textOutput(ns("hhi_producto")))), 
                        p(class = "metric-label", "Índice HHI"))),
          column(2, div(class = "metric-card", 
                        shinycssloaders::withSpinner(p(class = "metric-value", textOutput(ns("peso_total")))), 
                        p(class = "metric-label", "% sobre Total Importaciones")))
        ),
        
        fluidRow(
          column(12,
                 card(
                   card_header(class = "bg-light", "📋 Detalle de Proveedores"),
                   card_body(
                     div(style = "margin-bottom: 10px;",
                         downloadButton(ns("descargar_proveedores"), "📥 Descargar Excel", class = "btn btn-sm btn-outline-secondary")
                     ),
                     shinycssloaders::withSpinner(DTOutput(ns("tabla_proveedores"))))
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 card(
                   card_header(class = "bg-light", "📈 Evolución Histórica - Top 5 Proveedores (desde 2019)"),
                   card_body(shinycssloaders::withSpinner(plotlyOutput(ns("evolucion_proveedores"), height = "500px")))
                 )
          )
        )
    )
  )
}


contexto_producto_server <- function(id, conexion_db) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --------------------------------------------------------------------------
    # 1. INFO INICIAL - OPTIMIZACIÓN DE ARRANQUE
    # --------------------------------------------------------------------------
    info_inicial <- reactive({
      req(conexion_db)
      
      message("1️⃣ Cargando metadatos iniciales...")
      
      query_codigos <- "
        SELECT codigo 
        FROM datacomex.taric_codigo_iso3a 
        WHERE flujo = 'I' 
        GROUP BY codigo 
        ORDER BY codigo"
      
      query_fecha <- "SELECT MAX(fecha) as f FROM datacomex.taric_codigo_iso3a WHERE flujo = 'I'"
      
      res_codigos <- dbGetQuery(conexion_db, query_codigos)
      res_fecha <- dbGetQuery(conexion_db, query_fecha)
      
      # Generate HS2 and HS4 codes from the raw codes if they don't exist
      raw_codes <- res_codigos$codigo
      hs2 <- unique(substr(raw_codes, 1, 2))
      hs4 <- unique(substr(raw_codes, 1, 4))
      all_codes <- sort(unique(c(hs2, hs4, raw_codes)))
      
      fecha_max <- as.Date(res_fecha$f[1])
      fecha_inicio <- fecha_max %m-% months(11)
      
      list(
        fecha_max = fecha_max,
        desde = fecha_inicio,
        hasta = fecha_max,
        codigos = all_codes,
        label = paste0(format(fecha_inicio, "%b %Y"), " - ", format(fecha_max, "%b %Y"))
      )
    })
    
    # --------------------------------------------------------------------------
    # 2. ACTUALIZAR SELECTOR - SERVER-SIDE SELECTIZE (all codes, any level)
    # --------------------------------------------------------------------------
    observe({
      req(info_inicial())
      
      updateSelectizeInput(
        session, 
        "producto",
        choices = info_inicial()$codigos,
        server = TRUE,
        options = list(
          placeholder = 'Escriba para buscar código TARIC...',
          maxOptions = 100
        )
      )
    })

    # --------------------------------------------------------------------------
    # 2b. OVERLAP VALIDATION — live check, prevent parent/child combos
    # --------------------------------------------------------------------------
    updating_selector <- reactiveVal(FALSE)

    observeEvent(input$producto, {
      if (isTRUE(updating_selector())) return()
      
      sel <- input$producto
      if (is.null(sel) || length(sel) <= 1) {
        output$overlap_warning <- renderUI(NULL)
        return()
      }
      
      # Check for overlaps: code A is prefix of code B → remove the child
      to_remove <- character(0)
      for (i in seq_along(sel)) {
        for (j in seq_along(sel)) {
          if (i != j && startsWith(sel[j], sel[i]) && nchar(sel[j]) > nchar(sel[i])) {
            to_remove <- c(to_remove, sel[j])
          }
        }
      }
      to_remove <- unique(to_remove)
      
      if (length(to_remove) > 0) {
        clean <- setdiff(sel, to_remove)
        
        output$overlap_warning <- renderUI({
          div(class = "alert alert-warning", style = "margin-top: 8px; padding: 8px 12px;",
              icon("triangle-exclamation"),
              sprintf(" Códigos eliminados por solapamiento: %s (ya cubiertos por un código padre seleccionado).",
                      paste(to_remove, collapse = ", ")))
        })
        
        updating_selector(TRUE)
        updateSelectizeInput(session, "producto",
                             choices = info_inicial()$codigos,
                             selected = clean,
                             server = TRUE,
                             options = list(
                               placeholder = 'Escriba para buscar código TARIC...',
                               maxOptions = 100
                             ))
        session$onFlushed(function() { updating_selector(FALSE) }, once = TRUE)
      } else {
        output$overlap_warning <- renderUI(NULL)
      }
    }, ignoreNULL = FALSE)
    
    # --------------------------------------------------------------------------
    # 3. PRODUCTOS VALIDADOS — set on button click
    # --------------------------------------------------------------------------
    productos_validados <- reactiveVal(NULL)
    
    observeEvent(input$buscar, {
      sel <- input$producto
      if (is.null(sel) || length(sel) == 0) {
        showNotification("Seleccione al menos un código TARIC.", type = "warning")
        return()
      }
      productos_validados(sel)
    })
    
    nivel_producto <- reactive({
      req(productos_validados())
      niveles <- unique(nchar(productos_validados()))
      if (length(niveles) == 1) niveles[1] else max(niveles)
    })
    
    # --------------------------------------------------------------------------
    # 4. CARGA DE DATOS - FILTRADO EN SQL (triggers on productos_validados)
    # --------------------------------------------------------------------------
    datos_producto_raw <- reactive({
      req(productos_validados(), info_inicial())
      info <- info_inicial()
      codigos <- productos_validados()
      
      id_notif <- showNotification("Descargando datos...", duration = NULL, type = "message")
      on.exit(removeNotification(id_notif))
      
      req(conexion_db)
      
      # Load data for each selected product and combine
      dfs <- lapply(codigos, function(cod) {
        tryCatch({
          comerciotools::cargar_pg_datacomex(
            dataset = "taric",
            codigo = cod,
            desde = as.Date("2019-01-01"),
            hasta = info$hasta,
            .conexion_db = conexion_db
          )
        }, error = function(e) NULL)
      })
      
      raw_df <- do.call(rbind, Filter(Negate(is.null), dfs))
      if (is.null(raw_df) || nrow(raw_df) == 0) {
        showNotification("No se encontraron datos de importación para estos productos.", type = "warning")
        return(data.frame(iso3a = character(), euros = numeric(), fecha = as.Date(character()), stringsAsFactors = FALSE))
      }
      
      # Deduplicate in case of overlapping sub-codes (belt and suspenders)
      res <- raw_df %>% filter(flujo == "I") %>% distinct()
      
      if(nrow(res) == 0) {
        showNotification("No se encontraron datos de importación para estos productos.", type = "warning")
        return(data.frame(iso3a = character(), euros = numeric(), fecha = as.Date(character()), stringsAsFactors = FALSE))
      }
      return(res)
    })
    
    # --------------------------------------------------------------------------
    # NUEVO: TOTAL IMPORTACIONES (TODOS LOS PRODUCTOS)
    # --------------------------------------------------------------------------
    total_importaciones_general <- reactive({
      req(info_inicial())
      info <- info_inicial()
      
      req(conexion_db)
      
      query_total <- sprintf("
        SELECT SUM(euros) as total
        FROM datacomex.taric_codigo_iso3a
        WHERE flujo = 'I'
        AND fecha >= '%s'
        AND fecha <= '%s'",
                             info$desde, info$hasta
      )
      
      res <- dbGetQuery(conexion_db, query_total)
      return(res$total[1])
    })
    
    # --------------------------------------------------------------------------
    # 5. CONSOLIDACIÓN DE MÉTRICAS (PROCESAMIENTO RÁPIDO)
    # --------------------------------------------------------------------------
    datos_consolidados <- reactive({
      req(datos_producto_raw(), info_inicial())
      
      datos <- datos_producto_raw()
      info <- info_inicial()
      
      # Definimos periodos
      h_actual <- info$hasta
      d_actual <- info$desde
      h_ant <- h_actual %m-% years(1)
      d_ant <- d_actual %m-% years(1)
      
      # NUEVO: Último mes
      ultimo_mes <- datos %>%
        filter(fecha == h_actual) %>%
        group_by(iso3a) %>%
        summarise(valor_ultimo_mes = sum(euros, na.rm = TRUE), .groups = "drop")
      
      # Agregación por país en un solo paso
      actual <- datos %>%
        filter(fecha >= d_actual, fecha <= h_actual) %>%
        group_by(iso3a) %>%
        summarise(valor_12m = sum(euros, na.rm = TRUE), .groups = "drop")
      
      anterior <- datos %>%
        filter(fecha >= d_ant, fecha <= h_ant) %>%
        group_by(iso3a) %>%
        summarise(valor_12m_ant = sum(euros, na.rm = TRUE), .groups = "drop")
      
      # Join y cálculo de cuotas
      resultado <- actual %>%
        left_join(anterior, by = "iso3a") %>%
        left_join(ultimo_mes, by = "iso3a") %>%
        mutate(
          valor_12m_ant = coalesce(valor_12m_ant, 0),
          valor_ultimo_mes = coalesce(valor_ultimo_mes, 0),
          cuota = (valor_12m / sum(valor_12m, na.rm = TRUE)) * 100,
          var_12m = ifelse(valor_12m_ant > 0, ((valor_12m / valor_12m_ant) - 1) * 100, NA)
        ) %>%
        arrange(desc(valor_12m))
      
      return(resultado)
    })
    
    # --------------------------------------------------------------------------
    # 6. HISTÓRICOS TOP 5 (FILTRADO INTELIGENTE)
    # --------------------------------------------------------------------------
    datos_historicos <- reactive({
      req(datos_producto_raw(), datos_consolidados())
      
      top5 <- head(datos_consolidados()$iso3a, 5)
      
      datos_producto_raw() %>%
        filter(iso3a %in% top5) %>%
        group_by(iso3a, fecha) %>%
        summarise(valor = sum(euros, na.rm = TRUE), .groups = "drop") %>%
        arrange(fecha)
    })
    
    # --------------------------------------------------------------------------
    # OUTPUTS (Visualización)
    # --------------------------------------------------------------------------
    
    output$producto_info <- renderUI({
      req(productos_validados(), info_inicial())
      codigos_str <- paste(productos_validados(), collapse = ", ")
      div(class = "info-box",
          p(icon("box"), strong(" Códigos: "), codigos_str, " | ",
            icon("calendar"), strong(" Periodo: "), info_inicial()$label))
    })
    
    output$total_imports <- renderText({
      req(datos_consolidados())
      val <- sum(datos_consolidados()$valor_12m, na.rm = TRUE)
      # Mostrar en millones
      paste0(scales::label_number(accuracy = 0.1, scale = 1e-6, suffix = "M")(val), "€")
    })
    
    output$num_proveedores <- renderText({
      req(datos_consolidados())
      nrow(datos_consolidados())
    })
    
    output$top_proveedor <- renderText({
      req(datos_consolidados())
      d <- datos_consolidados()
      if(nrow(d) > 0) paste0(d$iso3a[1], " (", round(d$cuota[1], 1), "%)") else "N/A"
    })
    
    output$hhi_producto <- renderText({
      req(datos_consolidados())
      round(sum(datos_consolidados()$cuota^2, na.rm = TRUE), 0)
    })
    
    # NUEVO: Peso sobre total importaciones
    output$peso_total <- renderText({
      req(datos_consolidados(), total_importaciones_general())
      val_producto <- sum(datos_consolidados()$valor_12m, na.rm = TRUE)
      val_total <- total_importaciones_general()
      if(!is.na(val_total) && val_total > 0) {
        porcentaje <- (val_producto / val_total) * 100
        paste0(round(porcentaje, 2), "%")
      } else {
        "N/A"
      }
    })
    
    output$tabla_proveedores <- renderDT({
      req(datos_consolidados())
      datatable(
        datos_consolidados() %>% 
          select(País = iso3a, 
                 `Valor Último Mes` = valor_ultimo_mes,
                 `Valor 12m` = valor_12m, 
                 `Cuota % (12m)` = cuota),
        options = list(pageLength = 10, dom = 'ftp'),
        rownames = FALSE
      ) %>% 
        formatCurrency(2:3, "€") %>% 
        formatRound(4, 1)
    })
    
    output$descargar_proveedores <- downloadHandler(
      filename = function() {
        codigos_str <- paste(productos_validados(), collapse = "_")
        paste0("proveedores_", codigos_str, "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        req(datos_consolidados())
        df <- datos_consolidados() %>%
          select(País = iso3a,
                 `Valor Último Mes` = valor_ultimo_mes,
                 `Valor 12m` = valor_12m,
                 `Cuota % (12m)` = cuota)
        writexl::write_xlsx(df, file)
      }
    )
    
    output$evolucion_proveedores <- renderPlotly({
      req(datos_historicos())
      plot_ly(datos_historicos(), x = ~fecha, y = ~valor, color = ~iso3a, 
              type = 'scatter', mode = 'lines+markers') %>%
        layout(yaxis = list(title = "Euros"), xaxis = list(title = ""), hovermode = "x unified")
    })
    
    # Retorno de reactivos para otros módulos
    return(list(
      producto = reactive(productos_validados()),
      nivel = nivel_producto,
      datos = datos_consolidados,
      periodo = reactive(info_inicial()),
      total_general = total_importaciones_general
    ))
  })
}