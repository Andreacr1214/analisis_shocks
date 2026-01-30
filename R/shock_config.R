
configuracion_shock_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(class = "container-fluid mt-4",

        fluidRow(
          column(12,
                 h2(class = "section-title", "⚡ Análisis de Shock e Impacto Directo"),
                 div(class = "info-box", "Configure el escenario y visualice el impacto inmediato sin sustitución.")
          )
        ),
        
        fluidRow(
          column(4,
                 card(
                   card_header(class = "bg-light", "🎯 Parámetros del Shock"),
                   card_body(
                     selectizeInput(ns("pais_shock"), "País afectado:", choices = NULL, width = "100%"),
                     sliderInput(ns("magnitud_shock"), "Caída en importaciones:", 
                                 min = 0, max = 100, value = 40, step = 5, post = "%"),
                     hr(),
                     actionButton(ns("aplicar_shock"), "⚡ Aplicar y Calcular", class = "btn btn-primary btn-lg w-100")
                   )
                 )
          ),
          column(8,
            
                 fluidRow(
                   column(3, div(class = "metric-card", 
                                 p(class = "metric-value", textOutput(ns("perdida_absoluta"))), 
                                 p(class = "metric-label", "Pérdida Absoluta"))),
                   column(3, div(class = "metric-card", 
                                 p(class = "metric-value", textOutput(ns("pct_producto"))), 
                                 p(class = "metric-label", "% s/Producto"))),
                   column(3, div(class = "metric-card", 
                                 p(class = "metric-value", textOutput(ns("pct_total"))), 
                                 p(class = "metric-label", "% s/Import. Totales"))),
                   column(3, div(class = "metric-card", 
                                 p(class = "metric-value", textOutput(ns("cuota_original"))), 
                                 p(class = "metric-label", "Cuota País Afectado")))
                 ),
                 fluidRow(
                   column(4, div(class = "metric-card", 
                                 p(class = "metric-value", textOutput(ns("nuevo_hhi"))), 
                                 p(class = "metric-label", "Nuevo HHI"))),
                   column(4, div(class = "metric-card", 
                                 p(class = "metric-value", textOutput(ns("variacion_hhi"))), 
                                 p(class = "metric-label", "Δ HHI"))),
                   column(4, div(class = "metric-card", 
                                 p(class = "metric-value", textOutput(ns("nuevo_top1"))), 
                                 p(class = "metric-label", "Nuevo Líder")))
                 )
          )
        ),
        
        fluidRow(
          column(6,
                 card(
                   card_header(class = "bg-light", "📊 Magnitud del Impacto Directo"),
                   card_body(plotlyOutput(ns("grafico_impacto"), height = "350px"))
                 )
          ),
          column(6,
                 card(
                   card_header(class = "bg-light", "🔄 Distribución: Antes y Después"),
                   card_body(plotlyOutput(ns("distribucion_comparada"), height = "350px"))
                 )
          )
        )
    )
  )
}

configuracion_shock_server <- function(id, contexto_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive para el estado del shock
    shock_configurado <- reactiveVal(NULL)
    
    # Actualizar países
    observe({
      req(contexto_data$datos())
      datos <- contexto_data$datos()
      paises_choices <- setNames(datos$iso3a, paste0(datos$iso3a, " - ", round(datos$cuota, 1), "%"))
      updateSelectizeInput(session, "pais_shock", choices = c("Seleccione país..." = "", paises_choices))
    })
    
    # Lógica de cálculo al Aplicar
    observeEvent(input$aplicar_shock, {
      req(input$pais_shock, contexto_data$datos(), contexto_data$total_general)
      
      datos_orig <- contexto_data$datos()
      p_data <- datos_orig %>% filter(iso3a == input$pais_shock)
      total_v <- sum(datos_orig$valor_12m, na.rm = TRUE)
      
      # Calcular HHI original
      hhi_original <- sum(datos_orig$cuota^2, na.rm = TRUE)
      
      # Calcular nuevas cuotas post-shock
      valor_perdido <- p_data$valor_12m[1] * (input$magnitud_shock / 100)
      nuevo_total <- total_v - valor_perdido
      
      # Obtener total general y calcular post-shock
      total_gral_original <- contexto_data$total_general()
      total_gral_post_shock <- total_gral_original - valor_perdido
      
      datos_post <- datos_orig %>%
        mutate(
          valor_post = ifelse(iso3a == input$pais_shock, valor_12m - valor_perdido, valor_12m),
          cuota_post = (valor_post / nuevo_total) * 100
        )
      
      # Calcular nuevo HHI
      hhi_nuevo <- sum(datos_post$cuota_post^2, na.rm = TRUE)
      
      # Identificar nuevo líder
      nuevo_lider <- datos_post %>% 
        arrange(desc(valor_post)) %>% 
        slice(1)
      
      shock_configurado(list(
        pais = input$pais_shock,
        magnitud = input$magnitud_shock,
        cuota_original = p_data$cuota[1],
        valor_original = p_data$valor_12m[1],
        valor_perdido = valor_perdido,
        total_antes = total_v,
        total_despues = nuevo_total,
        total_gral_original = total_gral_original,
        total_gral_post_shock = total_gral_post_shock,
        hhi_original = hhi_original,
        hhi_nuevo = hhi_nuevo,
        nuevo_top1 = paste0(nuevo_lider$iso3a, " (", round(nuevo_lider$cuota_post, 1), "%)"),
        datos_full = datos_orig,
        datos_post = datos_post
      ))
    })
    
    # OUTPUTS DE MÉTRICAS
    output$perdida_absoluta <- renderText({
      req(shock_configurado())
      scales::number(shock_configurado()$valor_perdido, scale = 1e-6, suffix = "M€", accuracy = 0.1)
    })
    
    output$pct_producto <- renderText({
      req(shock_configurado())
      paste0("-", round((shock_configurado()$valor_perdido / shock_configurado()$total_antes) * 100, 1), "%")
    })
    
    output$pct_total <- renderText({
      req(shock_configurado())
      s <- shock_configurado()
      
      if(!is.na(s$total_gral_post_shock) && s$total_gral_post_shock > 0) {
        # Producto POST-SHOCK sobre Total POST-SHOCK
        pct <- (s$total_despues / s$total_gral_post_shock) * 100
        paste0(round(pct, 3), "%")
      } else {
        "N/A"
      }
    })
    
    output$cuota_original <- renderText({
      req(shock_configurado())
      paste0(round(shock_configurado()$cuota_original, 1), "%")
    })
    
    output$nuevo_hhi <- renderText({
      req(shock_configurado())
      round(shock_configurado()$hhi_nuevo, 0)
    })
    
    output$variacion_hhi <- renderText({
      req(shock_configurado())
      s <- shock_configurado()
      delta <- s$hhi_nuevo - s$hhi_original
      paste0(ifelse(delta > 0, "+", ""), round(delta, 0))
    })
    
    output$nuevo_top1 <- renderText({
      req(shock_configurado())
      shock_configurado()$nuevo_top1
    })
    
    # GRÁFICO COMPARATIVO (Barras Antes/Después)
    output$grafico_impacto <- renderPlotly({
      req(shock_configurado())
      s <- shock_configurado()
      df <- data.frame(
        Escenario = c("Actual", "Post-Shock"), 
        Valor = c(s$total_antes / 1e6, s$total_despues / 1e6)
      )
      
      plot_ly(df, x = ~Escenario, y = ~Valor, type = "bar", 
              marker = list(color = c("#90CAF9", "#F44336"))) %>%
        layout(title = "Reducción Importación Total del Producto (M€)",
               yaxis = list(title = "Millones €"))
    })
    
    # DISTRIBUCIÓN COMPARADA
    output$distribucion_comparada <- renderPlotly({
      req(shock_configurado())
      s <- shock_configurado()
      
      df_plot <- s$datos_post %>%
        head(10) %>%
        mutate(
          valor_original_m = valor_12m / 1e6,
          valor_post_m = valor_post / 1e6
        )
      
      plot_ly(df_plot, y = ~reorder(iso3a, valor_12m)) %>%
        add_bars(x = ~valor_original_m, name = "Original", marker = list(color = "#90CAF9")) %>%
        add_bars(x = ~valor_post_m, name = "Post-Shock", marker = list(color = "#F44336")) %>%
        layout(barmode = "overlay", title = "Top 10 Proveedores (Impacto Directo)",
               xaxis = list(title = "Millones €"), yaxis = list(title = ""))
    })
    
    return(shock_configurado)
  })
}