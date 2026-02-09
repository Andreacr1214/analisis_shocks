
comparativa_eurostat_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(class = "container-fluid mt-4",
        
        fluidRow(
          column(12,
                 h2(class = "section-title", "🇪🇺 Comparativa Europea"),
                 div(class = "info-box",
                     p("Este módulo compara la dependencia de España respecto al país del shock con la de otros socios europeos."),
                     p(style = "margin-bottom: 0;",
                       "Cuando el alcance geográfico del shock no es global, el impacto se diferencia: ",
                       "España, resto de la UE y resto del mundo pueden verse afectados en distinta magnitud. ",
                       "Esto permite evaluar si España pierde o gana competitividad relativa frente a sus socios.")
                 )
          )
        ),
        
        # Estado del análisis
        fluidRow(
          column(12,
                 uiOutput(ns("estado_analisis"))
          )
        ),
        
        # Info del alcance configurado
        fluidRow(
          column(12,
                 uiOutput(ns("info_alcance"))
          )
        ),
        
        # Contenido principal (visible solo si hay datos)
        conditionalPanel(
          condition = sprintf("output['%s']", ns("hay_datos")),
          
          # Métricas resumen
          fluidRow(
            column(3,
                   div(class = "metric-card",
                       p(class = "metric-value", textOutput(ns("rank_espana"))),
                       p(class = "metric-label", "Posición Dependencia España"))
            ),
            column(3,
                   div(class = "metric-card",
                       p(class = "metric-value", textOutput(ns("avg_ue"))),
                       p(class = "metric-label", "Dependencia Media UE"))
            ),
            column(3,
                   div(class = "metric-card",
                       p(class = "metric-value", textOutput(ns("gap_espana"))),
                       p(class = "metric-label", "Diferencial España vs UE"))
            ),
            column(3,
                   div(class = "metric-card",
                       p(class = "metric-value", textOutput(ns("perdida_espana"))),
                       p(class = "metric-label", "Pérdida Estimada España"))
            )
          ),
          
          # Gráfico dependencia
          fluidRow(
            column(6,
                   card(
                     card_header(class = "bg-light", "📊 Dependencia del País del Shock"),
                     card_body(
                       div(class = "text-muted small mb-2",
                           "Porcentaje de las importaciones totales de cada país UE que provienen del país afectado."),
                       shinycssloaders::withSpinner(plotlyOutput(ns("grafico_comparativa"), height = "500px"))
                     )
                   )
            ),
            column(6,
                   card(
                     card_header(class = "bg-light", "📊 Pérdida Estimada por País"),
                     card_body(
                       div(class = "text-muted small mb-2",
                           "Impacto diferenciado según el alcance del shock. ",
                           "Si el shock no es global, cada país se ve afectado en distinta magnitud."),
                       shinycssloaders::withSpinner(plotlyOutput(ns("grafico_perdidas"), height = "500px"))
                     )
                   )
            )
          ),
          
          # Tabla detallada
          fluidRow(
            column(12,
                   card(
                     card_header(class = "bg-light", "📋 Datos Detallados por País"),
                     card_body(
                       div(class = "text-muted small mb-2",
                           tags$strong("Shock aplicado:"), " magnitud que aplica a cada país según el alcance configurado. ",
                           tags$strong("Pérdida:"), " valor estimado de las importaciones perdidas. ",
                           tags$strong("Δ Competitivo:"), " diferencia de pérdida relativa vs media UE (negativo = España pierde más que la media)."),
                       div(style = "margin-bottom: 10px;",
                           downloadButton(ns("descargar_comparativa"), "📥 Descargar Excel", class = "btn btn-sm btn-outline-secondary")
                       ),
                       DTOutput(ns("tabla_comparativa"))
                     )
                   )
            )
          )
        )
    )
  )
}

comparativa_eurostat_server <- function(id, contexto_data, shock_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    paises_ue <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
                   "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
                   "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")
    
    # Reactive value para controlar si mostramos el contenido
    output$hay_datos <- reactive({
      !is.null(datos_eurostat()) && nrow(datos_eurostat()) > 0
    })
    outputOptions(output, "hay_datos", suspendWhenHidden = FALSE)
    
    output$estado_analisis <- renderUI({
      req(contexto_data$producto())
      
      if (is.null(shock_data())) {
        return(div(class = "alert alert-warning", 
                   icon("triangle-exclamation"), 
                   " Primero configure un shock en la pestaña 'Definición del Shock'."))
      }
      
      return(NULL)
    })
    
    # Info del alcance configurado
    output$info_alcance <- renderUI({
      req(shock_data())
      s <- shock_data()
      
      alcance_label <- switch(s$alcance %||% "global",
        "global" = "Igual para toda la UE — todos los importadores de la UE afectados por igual",
        "espana" = "Específico España — solo España afectada, resto de la UE no",
        "custom" = sprintf("Personalizado — España: -%s%%, Resto UE: -%s%%",
                           s$mag_espana, s$mag_ue),
        "Igual para toda la UE"
      )
      
      div(class = "alert alert-info", style = "margin-bottom: 16px;",
          icon("globe"), strong(" Alcance del shock: "), alcance_label)
    })
    
    # Carga de datos Eurostat
    datos_eurostat <- reactive({
      req(contexto_data$producto(), shock_data())
      
      shock <- shock_data()
      prod_codes <- contexto_data$producto()
      
      id_notif <- showNotification("Consultando datos de Eurostat para comparativa UE...", duration = NULL, type = "message")
      on.exit(removeNotification(id_notif))
      
      tryCatch({
        anio_inicio <- as.numeric(format(Sys.Date(), "%Y")) - 2
        
        if (!exists("eurostat", where = asNamespace("comerciotools"), mode = "function")) {
             stop("La función eurostat no está disponible en comerciotools.")
        }
        
        # Query Eurostat for each selected product code and combine
        dfs <- lapply(prod_codes, function(pc) {
          tryCatch({
            comerciotools::eurostat(
              flujo = "I",
              codigo = pc,
              declarante = paises_ue,
              desde = anio_inicio,
              .fechas = TRUE
            )
          }, error = function(e) NULL)
        })
        df_raw <- do.call(rbind, Filter(Negate(is.null), dfs))
        
        if(is.null(df_raw) || nrow(df_raw) == 0) {
             stop("Sin datos de Eurostat.")
        }

        max_fecha <- max(df_raw$fecha, na.rm = TRUE)
        min_fecha <- max_fecha - months(11)
        
        df_filtered <- df_raw %>%
          filter(fecha >= min_fecha, fecha <= max_fecha)
        
        totales <- df_filtered %>%
          group_by(declarante) %>%
          summarise(total_importado = sum(euros, na.rm = TRUE), .groups = "drop")
        
        # Si el shock es a TODOS los proveedores, importado_shock = total_importado
        if (shock$pais == "TODOS") {
          desde_shock <- totales %>%
            mutate(importado_shock = total_importado) %>%
            select(declarante, importado_shock)
        } else {
          desde_shock <- df_filtered %>%
            filter(socio == shock$pais) %>%
            group_by(declarante) %>%
            summarise(importado_shock = sum(euros, na.rm = TRUE), .groups = "drop")
        }
        
        # Determinar magnitud por país según alcance
        mag_espana <- (shock$mag_espana %||% shock$magnitud) / 100
        mag_ue     <- (shock$mag_ue %||% shock$magnitud) / 100
        
        final_df <- totales %>%
          left_join(desde_shock, by = "declarante") %>%
          mutate(
            importado_shock = coalesce(importado_shock, 0),
            dependencia_pct = (importado_shock / total_importado) * 100,
            # Asignar magnitud diferenciada
            magnitud_aplicada = case_when(
              declarante == "ESP" ~ mag_espana,
              TRUE ~ mag_ue
            ),
            perdida = importado_shock * magnitud_aplicada,
            perdida_pct = (perdida / total_importado) * 100
          ) %>%
          arrange(desc(dependencia_pct))
        
        return(final_df)
        
      }, error = function(e) {
        message(paste("Error en análisis UE:", e$message))
        showNotification(paste("Error Eurostat:", e$message), type = "error", duration = 10)
        return(data.frame(declarante = character(), total_importado = numeric(),
                          importado_shock = numeric(), dependencia_pct = numeric(),
                          magnitud_aplicada = numeric(), perdida = numeric(), 
                          perdida_pct = numeric()))
      })
    })
    
    # Outputs
    
    output$rank_espana <- renderText({
      req(datos_eurostat())
      df <- datos_eurostat()
      rank <- which(df$declarante == "ESP")
      if(length(rank) == 0) return("N/A")
      paste0("#", rank, " de ", nrow(df))
    })
    
    output$avg_ue <- renderText({
      req(datos_eurostat())
      df <- datos_eurostat()
      avg <- mean(df$dependencia_pct, na.rm = TRUE)
      paste0(round(avg, 1), "%")
    })
    
    output$gap_espana <- renderText({
      req(datos_eurostat())
      df <- datos_eurostat()
      esp_val <- df$dependencia_pct[df$declarante == "ESP"]
      avg <- mean(df$dependencia_pct, na.rm = TRUE)
      
      if(length(esp_val) == 0) return("N/A")
      
      diff <- esp_val - avg
      signo <- ifelse(diff > 0, "+", "")
      paste0(signo, round(diff, 1), " pp")
    })
    
    output$perdida_espana <- renderText({
      req(datos_eurostat())
      df <- datos_eurostat()
      esp <- df %>% filter(declarante == "ESP")
      if(nrow(esp) == 0) return("N/A")
      paste0("-", scales::number(esp$perdida[1], scale = 1e-6, suffix = "M€", accuracy = 0.1))
    })
    
    output$grafico_comparativa <- renderPlotly({
      req(datos_eurostat(), shock_data())
      df <- datos_eurostat()
      shock <- shock_data()
      
      colors <- ifelse(df$declarante == "ESP", "#F44336", "#90CAF9")
      
      plot_ly(df, x = ~dependencia_pct, y = ~reorder(declarante, dependencia_pct),
              type = "bar",
              text = ~paste0(round(dependencia_pct, 1), "%"),
              textposition = "auto",
              marker = list(color = colors)) %>%
        layout(
          title = paste0("Dependencia de ", shock$pais, " (%)"),
          xaxis = list(title = "Dependencia (%)"),
          yaxis = list(title = ""),
          hovermode = "y unified"
        )
    })
    
    # Nuevo: gráfico de pérdidas diferenciadas
    output$grafico_perdidas <- renderPlotly({
      req(datos_eurostat(), shock_data())
      df <- datos_eurostat() %>% arrange(desc(perdida))
      shock <- shock_data()
      
      # Color por magnitud aplicada
      colors <- case_when(
        df$declarante == "ESP" ~ "#F44336",
        df$magnitud_aplicada == 0 ~ "#E0E0E0",
        TRUE ~ "#FF9800"
      )
      
      plot_ly(df, x = ~perdida / 1e6, y = ~reorder(declarante, perdida),
              type = "bar",
              text = ~paste0(round(perdida / 1e6, 1), "M€ (-", round(magnitud_aplicada * 100), "%)"),
              textposition = "auto",
              marker = list(color = colors),
              hoverinfo = "text") %>%
        layout(
          title = paste0("Pérdida estimada por shock a ", shock$pais),
          xaxis = list(title = "Pérdida (M€)"),
          yaxis = list(title = ""),
          hovermode = "y unified"
        )
    })
    
    output$tabla_comparativa <- renderDT({
      req(datos_eurostat())
      
      avg_perdida_pct <- mean(datos_eurostat()$perdida_pct, na.rm = TRUE)
      
      df <- datos_eurostat() %>%
        mutate(
          delta_competitivo = perdida_pct - avg_perdida_pct
        )
      
      # Si es shock "TODOS", no mostrar "Import. desde Shock" (es igual a Totales)
      if (shock_data()$pais == "TODOS") {
        df <- df %>%
          select(
            País = declarante,
            `Dependencia %` = dependencia_pct,
            `Shock Aplicado %` = magnitud_aplicada,
            `Pérdida (€)` = perdida,
            `Pérdida s/Total %` = perdida_pct,
            `Δ Competitivo` = delta_competitivo,
            `Import. Totales` = total_importado
          )
        col_currency <- c(4, 7)
      } else {
        # Si es país concreto, mostrar ambas columnas
        df <- df %>%
          select(
            País = declarante,
            `Dependencia %` = dependencia_pct,
            `Shock Aplicado %` = magnitud_aplicada,
            `Pérdida (€)` = perdida,
            `Pérdida s/Total %` = perdida_pct,
            `Δ Competitivo` = delta_competitivo,
            `Import. desde Shock` = importado_shock,
            `Import. Totales` = total_importado
          )
        col_currency <- c(4, 7, 8)
      }
      
      df %>%
        datatable(
          options = list(pageLength = 27, dom = 'ft', order = list(list(3, 'desc'))),
          rownames = FALSE
        ) %>%
        formatRound(c(2, 5, 6), 1) %>%
        formatPercentage(3, 0) %>%
        formatCurrency(col_currency, "€", digits = 0)
    })
    
    output$descargar_comparativa <- downloadHandler(
      filename = function() {
        req(shock_data())
        paste0("comparativa_UE_", shock_data()$pais, "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        req(datos_eurostat())
        avg_perdida_pct <- mean(datos_eurostat()$perdida_pct, na.rm = TRUE)
        df <- datos_eurostat() %>%
          mutate(delta_competitivo = perdida_pct - avg_perdida_pct) %>%
          select(
            País = declarante,
            `Dependencia %` = dependencia_pct,
            `Shock Aplicado` = magnitud_aplicada,
            `Pérdida (€)` = perdida,
            `Pérdida s/Total %` = perdida_pct,
            `Δ Competitivo` = delta_competitivo,
            `Import. desde Shock` = importado_shock,
            `Import. Totales` = total_importado
          )
        writexl::write_xlsx(df, file)
      }
    )
    
  })
}
