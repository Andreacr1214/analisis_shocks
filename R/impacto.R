# ============================================================================
# impacto.R - Módulo de Impacto Directo
# ============================================================================

# Note: library() calls removed — dependencies handled via DESCRIPTION/NAMESPACE
# and the root app.R entry point

# ============================================================================
# UI del Módulo
# ============================================================================

impacto_directo_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(class = "container-fluid mt-4",
        
        # Título y descripción
        fluidRow(
          column(12,
                 h2(class = "section-title", "📉 Impacto Directo del Shock"),
                 div(class = "info-box",
                     "Este módulo calcula el impacto inmediato del shock sin considerar 
            mecanismos de sustitución. Representa el escenario de máximo riesgo."
                 )
          )
        ),
        
        # Estado del shock
        fluidRow(
          column(12,
                 card(
                   card_header(class = "bg-warning text-dark", "⚠️ Parámetros del Shock Aplicado"),
                   card_body(
                     uiOutput(ns("shock_summary"))
                   )
                 )
          )
        ),
        
        # Métricas de impacto
        fluidRow(
          column(4,
                 div(class = "metric-card",
                     p(class = "metric-value", textOutput(ns("perdida_absoluta"))),
                     p(class = "metric-label", "Pérdida Absoluta (€)")
                 )
          ),
          column(4,
                 div(class = "metric-card",
                     p(class = "metric-value", textOutput(ns("pct_producto"))),
                     p(class = "metric-label", "% sobre Import. Producto")
                 )
          ),
          column(4,
                 div(class = "metric-card",
                     p(class = "metric-value", textOutput(ns("pct_total"))),
                     p(class = "metric-label", "% sobre Import. Totales España")
                 )
          )
        ),
        
        # Visualización del impacto
        fluidRow(
          column(12,
                 card(
                   card_header(class = "bg-light", "📊 Magnitud del Impacto"),
                   card_body(
                     plotlyOutput(ns("grafico_impacto"), height = "400px")
                   )
                 )
          )
        ),
        
        # Comparativa antes/después
        fluidRow(
          column(12,
                 card(
                   card_header(class = "bg-light", "🔄 Distribución: Antes y Después del Shock"),
                   card_body(
                     plotlyOutput(ns("distribucion_comparada"), height = "500px")
                   )
                 )
          )
        ),
        
        # Tabla de cambios
        fluidRow(
          column(12,
                 card(
                   card_header(class = "bg-light", "📋 Detalle de Cambios por Proveedor"),
                   card_body(
                     DTOutput(ns("tabla_cambios"))
                   )
                 )
          )
        )
    )
  )
}

# ============================================================================
# SERVER del Módulo
# ============================================================================

impacto_directo_server <- function(id, conexion_db = NULL, contexto_data = NULL, shock_data = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # TODO: Implementar lógica completa del módulo con contexto_data y shock_data
    
    output$shock_summary <- renderUI({
      div(
        style = "padding: 20px;",
        p(strong("Estado:"), " No hay shock configurado"),
        p("Configure el shock en la pestaña de Configuración.")
      )
    })
    
    output$perdida_absoluta <- renderText({ "€0M" })
    output$pct_producto <- renderText({ "0%" })
    output$pct_total <- renderText({ "0%" })
    
    output$grafico_impacto <- renderPlotly({
      plot_ly() %>%
        layout(
          title = "Configure un shock para ver el impacto",
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
    
    output$distribucion_comparada <- renderPlotly({
      plot_ly() %>%
        layout(
          title = "Configure un shock para ver la distribución",
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
    
    output$tabla_cambios <- renderDT({
      datatable(
        data.frame(Mensaje = "Configure un shock para ver los cambios"),
        options = list(dom = 't'),
        rownames = FALSE
      )
    })
    
  })
}
