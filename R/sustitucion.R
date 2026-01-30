sustitucion_proveedores_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(class = "container-fluid mt-4",
        
        fluidRow(
          column(12,
                 h2(class = "section-title", "🔄 Impacto con Sustitución de Proveedores"),
                 div(class = "info-box",
                     "Configure cómo se redistribuye la pérdida de importaciones entre 
            proveedores alternativos según diferentes escenarios económicos."
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 card(
                   card_header(class = "bg-light", "🎯 Selección de Escenario de Sustitución"),
                   card_body(
                     radioButtons(
                       ns("tipo_escenario"),
                       "Escenario de sustitución:",
                       choices = c(
                         "🅰️ Sin sustitución (pérdida total)" = "none",
                         "🅱️ Sustitución proporcional" = "proportional",
                         "🅲️ Sustitución limitada por capacidad" = "capacity",
                         "🅳️ Sustitución 'amigable' (UE/OCDE)" = "friendly",
                         "🅴️ Sustitución por proximidad geográfica" = "proximity"
                       ),
                       selected = "proportional"
                     ),
                     
                     # Panel condicional para sustitución limitada
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'capacity'", ns("tipo_escenario")),
                       hr(),
                       div(
                         style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                         h5("⚙️ Configuración de Capacidad"),
                         sliderInput(
                           ns("limite_capacidad"),
                           "Límite de aumento por país (%):",
                           min = 0.5,
                           max = 20,
                           value = 3,
                           step = 0.5,
                           post = "%",
                           width = "100%"
                         ),
                         helpText("Indica el porcentaje máximo que cada país puede aumentar sus exportaciones respecto a su nivel actual.",
                                  "Valores bajos (1-3%) simulan restricciones de capacidad realistas.")
                       )
                     ),
                     
                     # Panel condicional para sustitución amigable
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'friendly'", ns("tipo_escenario")),
                       hr(),
                       div(
                         style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                         h5("🌍 Grupos 'Amigables'"),
                         checkboxGroupInput(
                           ns("grupos_amigables"),
                           "Seleccione los grupos:",
                           choices = c(
                             "Unión Europea (UE)" = "eu", 
                             "OCDE" = "oecd"
                           ),
                           selected = c("eu", "oecd")
                         )
                       )
                     ),
                     
                     # Panel condicional para sustitución por proximidad
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'proximity'", ns("tipo_escenario")),
                       hr(),
                       div(
                         style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                         h5("🗺️ Configuración de Proximidad"),
                         sliderInput(
                           ns("peso_distancia"),
                           "Peso de la distancia geográfica:",
                           min = 0, 
                           max = 1, 
                           value = 0.5, 
                           step = 0.1
                         ),
                         sliderInput(
                           ns("peso_comercial"),
                           "Peso de la proximidad comercial:",
                           min = 0, 
                           max = 1, 
                           value = 0.5, 
                           step = 0.1
                         ),
                         helpText("La suma de ambos pesos se normaliza automáticamente a 1.0")
                       )
                     ),
                     
                     hr(),
                     actionButton(
                       ns("calcular_sustitucion"),
                       "🧮 Calcular Sustitución",
                       class = "btn btn-primary btn-lg w-100"
                     )
                   )
                 )
          )
        ),

        fluidRow(
          column(3,
                 div(class = "metric-card",
                     p(class = "metric-value", textOutput(ns("pct_absorbido"))),
                     p(class = "metric-label", "% Shock Absorbido")
                 )
          ),
          column(3,
                 div(class = "metric-card",
                     p(class = "metric-value", textOutput(ns("perdida_neta"))),
                     p(class = "metric-label", "Pérdida Neta (€)")
                 )
          ),
          column(3,
                 div(class = "metric-card",
                     p(class = "metric-value", textOutput(ns("hhi_nuevo"))),
                     p(class = "metric-label", "Nuevo HHI")
                 )
          ),
          column(3,
                 div(class = "metric-card",
                     p(class = "metric-value", textOutput(ns("num_beneficiados"))),
                     p(class = "metric-label", "Países Beneficiados")
                 )
          )
        ),
        
        # Gráfico de sustitución
        fluidRow(
          column(12,
                 card(
                   card_header(class = "bg-light", "📊 Redistribución de Importaciones"),
                   card_body(
                     plotlyOutput(ns("grafico_sustitucion"), height = "500px")
                   )
                 )
          )
        ),
        
        # Tabla de redistribución
        fluidRow(
          column(12,
                 card(
                   card_header(class = "bg-light", "📋 Tabla de Redistribución Detallada"),
                   card_body(
                     DTOutput(ns("tabla_redistribucion"))
                   )
                 )
          )
        ),
        
        # NUEVO: Análisis de exportaciones de principales proveedores
        fluidRow(
          column(12,
                 card(
                   card_header(class = "bg-light", "🌐 Análisis de Diversificación de Exportaciones - Top 5 Proveedores"),
                   card_body(
                     div(class = "info-box", style = "margin-bottom: 20px;",
                         "Este análisis muestra a qué países exportan los principales proveedores alternativos, 
                         permitiendo evaluar su capacidad exportadora y el nivel de concentración de sus mercados de destino."),
                     withSpinner(uiOutput(ns("analisis_exportaciones")))
                   )
                 )
          )
        )
    )
  )
}

sustitucion_proveedores_server <- function(id, conexion_db, contexto_data, shock_data) {
  moduleServer(id, function(input, output, session) {

    paises_ue <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
                   "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
                   "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")
    
    paises_oecd <- c("AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CRI", "CZE", "DNK", "EST",
                     "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN",
                     "KOR", "LVA", "LTU", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT",
                     "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA")
    
    # Función auxiliar para calcular score de proximidad
    calcular_proximidad <- function(df, pais_shock, peso_dist, peso_com) {
      suma_pesos <- peso_dist + peso_com
      if(suma_pesos == 0) suma_pesos <- 1
      peso_dist <- peso_dist / suma_pesos
      peso_com <- peso_com / suma_pesos
      
      regiones <- list(
        europa = c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
                   "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
                   "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "GBR", "NOR", "CHE"),
        asia = c("CHN", "JPN", "KOR", "IND", "IDN", "THA", "VNM", "MYS", "SGP", "PHL",
                 "TWN", "HKG", "PAK", "BGD", "LKA", "MMR", "KHM", "LAO", "NPL", "MAC"),
        america = c("USA", "CAN", "MEX", "BRA", "ARG", "CHL", "COL", "PER", "VEN", "ECU",
                    "GTM", "CUB", "BOL", "HTI", "DOM", "HND", "PRY", "NIC", "SLV", "CRI",
                    "PAN", "URY", "JAM", "TTO", "GUY", "SUR", "BLZ", "BHS", "BRB", "GRD"),
        africa = c("ZAF", "EGY", "MAR", "DZA", "TUN", "NGA", "KEN", "ETH", "GHA", "TZA",
                   "UGA", "AGO", "SDN", "MOZ", "CMR", "CIV", "MDG", "MLI", "BFA", "MWI",
                   "ZMB", "SEN", "SOM", "TCD", "ZWE", "GIN", "RWA", "BEN", "BDI", "ERI"),
        oceania = c("AUS", "NZL", "PNG", "FJI", "NCL", "PYF", "SLB", "VUT", "WSM", "KIR")
      )
      
      region_shock <- "otra"
      for(r in names(regiones)) {
        if(pais_shock %in% regiones[[r]]) {
          region_shock <- r
          break
        }
      }
      
      df <- df %>%
        mutate(
          misma_region = case_when(
            region_shock == "europa" & iso3a %in% regiones$europa ~ 1.0,
            region_shock == "asia" & iso3a %in% regiones$asia ~ 1.0,
            region_shock == "america" & iso3a %in% regiones$america ~ 1.0,
            region_shock == "africa" & iso3a %in% regiones$africa ~ 1.0,
            region_shock == "oceania" & iso3a %in% regiones$oceania ~ 1.0,
            TRUE ~ 0.3
          ),
          prox_comercial = cuota / max(cuota, na.rm = TRUE),
          score_proximidad = (peso_dist * misma_region) + (peso_com * prox_comercial)
        )
      
      return(df)
    }
    
    resultado_sustitucion <- eventReactive(input$calcular_sustitucion, {
      req(shock_data(), contexto_data$datos())
      
      tipo <- isolate(input$tipo_escenario)
      limite <- isolate(input$limite_capacidad)
      grupos <- isolate(input$grupos_amigables)
      peso_dist <- isolate(input$peso_distancia)
      peso_com <- isolate(input$peso_comercial)
      
      shock <- shock_data()
      df_alternativos <- contexto_data$datos() %>%
        filter(iso3a != shock$pais) %>%
        mutate(valor_post_shock = valor_12m)
      
      valor_a_repartir <- shock$valor_perdido
      
      message(sprintf("\n========================================"))
      message(sprintf("Calculando sustitución: %s", tipo))
      message(sprintf("Valor a repartir: %.2fM€", valor_a_repartir/1e6))
      if(tipo == "capacity") {
        message(sprintf("Límite de capacidad configurado: %.1f%%", limite))
      }
      message(sprintf("========================================"))
      
      if (tipo == "none") {
        df_alternativos <- df_alternativos %>%
          mutate(incremento = 0)
        message("✓ Sin sustitución aplicada")
        
      } else if (tipo == "proportional") {
        cuota_relativa_sum <- sum(df_alternativos$cuota, na.rm = TRUE)
        df_alternativos <- df_alternativos %>%
          mutate(incremento = (cuota / cuota_relativa_sum) * valor_a_repartir)
        message(sprintf("✓ Sustitución proporcional: 100%% absorbido"))
        
      } else if (tipo == "capacity") {
        limite_pct <- limite / 100
        message(sprintf("Aplicando límite: %.1f%% (%.4f)", limite, limite_pct))
        
        df_alternativos <- df_alternativos %>%
          mutate(
            capacidad_maxima = valor_post_shock * limite_pct,
            incremento = 0,
            capacidad_restante = capacidad_maxima,
            cuota_original = cuota
          )
        
        message(sprintf("Capacidad total disponible: %.2fM€", sum(df_alternativos$capacidad_maxima)/1e6))
        
        valor_pendiente <- valor_a_repartir
        max_iteraciones <- 20
        iteracion <- 0
        
        while(valor_pendiente > 1 && iteracion < max_iteraciones) {
          iteracion <- iteracion + 1
          
          paises_con_capacidad <- df_alternativos %>%
            filter(capacidad_restante > 0.01)
          
          if(nrow(paises_con_capacidad) == 0) {
            message("  ⚠ No quedan países con capacidad disponible")
            break
          }
          
          suma_cuotas_disponibles <- sum(paises_con_capacidad$cuota_original, na.rm = TRUE)
          
          if(suma_cuotas_disponibles == 0) {
            message("  ⚠ Suma de cuotas disponibles es 0")
            break
          }
          
          df_alternativos <- df_alternativos %>%
            mutate(
              proporcion = ifelse(capacidad_restante > 0.01,
                                  cuota_original / suma_cuotas_disponibles,
                                  0),
              incremento_propuesto = proporcion * valor_pendiente,
              incremento_ronda = pmin(incremento_propuesto, capacidad_restante, na.rm = TRUE),
              incremento = incremento + incremento_ronda,
              capacidad_restante = pmax(0, capacidad_restante - incremento_ronda)
            )
          
          absorbido_ronda <- sum(df_alternativos$incremento_ronda, na.rm = TRUE)
          valor_pendiente <- valor_pendiente - absorbido_ronda
          
          message(sprintf("  Iteración %d: absorbido %.2fM€, pendiente %.2fM€, países activos: %d",
                          iteracion, absorbido_ronda/1e6, valor_pendiente/1e6, nrow(paises_con_capacidad)))
          
          if(absorbido_ronda < 1) {
            message("  ⚠ No se pudo absorber más valor")
            break
          }
        }
        
        pct_absorbido <- ((valor_a_repartir - valor_pendiente) / valor_a_repartir) * 100
        message(sprintf("✓ Sustitución capacity completada: %d iteraciones, %.2f%% absorbido",
                        iteracion, pct_absorbido))
        
      } else if (tipo == "friendly") {
        message(sprintf("Grupos seleccionados: %s", paste(grupos, collapse = ", ")))
        
        df_alternativos <- df_alternativos %>%
          mutate(
            es_amigo = (("eu" %in% grupos & iso3a %in% paises_ue) |
                          ("oecd" %in% grupos & iso3a %in% paises_oecd)),
            cuota_amigos = ifelse(es_amigo, cuota, 0),
            incremento = if(sum(cuota_amigos) > 0)
              (cuota_amigos / sum(cuota_amigos)) * valor_a_repartir
            else 0
          )
        
        num_amigos <- sum(df_alternativos$es_amigo)
        pct_absorbido <- (sum(df_alternativos$incremento) / valor_a_repartir) * 100
        message(sprintf("✓ Sustitución friendly: %d países amigables, %.2f%% absorbido",
                        num_amigos, pct_absorbido))
        
      } else if (tipo == "proximity") {
        message(sprintf("Pesos: %.1f%% distancia, %.1f%% comercial",
                        peso_dist*100, peso_com*100))
        
        df_alternativos <- calcular_proximidad(
          df_alternativos,
          shock$pais,
          peso_dist,
          peso_com
        )
        
        suma_scores <- sum(df_alternativos$score_proximidad, na.rm = TRUE)
        
        if(suma_scores > 0) {
          df_alternativos <- df_alternativos %>%
            mutate(incremento = (score_proximidad / suma_scores) * valor_a_repartir)
          message(sprintf("✓ Sustitución proximity: 100%% absorbido"))
        } else {
          df_alternativos <- df_alternativos %>%
            mutate(incremento = 0)
          message("⚠ No se pudo calcular proximidad")
        }
      }
      
      df_alternativos <- df_alternativos %>%
        mutate(
          valor_final = valor_post_shock + incremento,
          cambio_pct = ifelse(valor_post_shock > 0, (incremento / valor_post_shock) * 100, 0)
        )
      
      total_reubicado <- sum(df_alternativos$incremento, na.rm = TRUE)
      num_beneficiados <- sum(df_alternativos$incremento > 0, na.rm = TRUE)
      
      message(sprintf("Total reubicado: %.2fM€", total_reubicado/1e6))
      message(sprintf("Países beneficiados: %d", num_beneficiados))
      message(sprintf("========================================\n"))
      
      list(
        detalle = df_alternativos,
        pct_absorbido = (total_reubicado / valor_a_repartir) * 100,
        perdida_neta = valor_a_repartir - total_reubicado,
        hhi = sum((df_alternativos$valor_final / sum(df_alternativos$valor_final) * 100)^2, na.rm = TRUE),
        num_beneficiados = num_beneficiados
      )
    })
    
    # NUEVO: Análisis de exportaciones de los top 5 proveedores - CORREGIDO
    analisis_exportaciones_data <- eventReactive(input$calcular_sustitucion, {
      req(resultado_sustitucion(), contexto_data$producto())
      
      # Obtener top 5 proveedores post-sustitución
      top5 <- resultado_sustitucion()$detalle %>%
        arrange(desc(valor_final)) %>%
        head(5) %>%
        pull(iso3a)
      
      producto_codigo <- contexto_data$producto()
      
      message(sprintf("Consultando exportaciones de top 5: %s", paste(top5, collapse = ", ")))
      
      # Usar comtradr para obtener datos de exportaciones
      tryCatch({
        # Cargar token si está configurado
        token <- Sys.getenv("COMTRADE_TOKEN", "")
        
        if(token != "") {
          comtradr::set_primary_comtrade_key(token)
        } else {
          return(NULL)
        }
        
        # Consultar datos para cada país del top 5
        lista_resultados <- list()
        
        for(pais in top5) {
          message(sprintf("  Descargando datos de %s...", pais))
          
          datos_pais <- comtradr::ct_get_data(
            type = "goods",
            frequency = "A",
            commodity_classification = "HS",
            commodity_code = producto_codigo,
            flow_direction = "Export",
            reporter = pais,
            partner = "everything",
            start_date = 2024,
            end_date = 2024,
            verbose = FALSE
          )
          
          if(!is.null(datos_pais) && nrow(datos_pais) > 0) {
            
            # PASO 1: Extraer total exportado desde W00
            total_exportado <- datos_pais %>%
              filter(partner_iso == "W00") %>%
              summarise(total = sum(primary_value, na.rm = TRUE)) %>%
              pull(total)
            
            if(length(total_exportado) == 0 || total_exportado == 0) {
              # Si no hay W00, sumar todos excepto W00
              total_exportado <- datos_pais %>%
                filter(partner_iso != "W00") %>%
                summarise(total = sum(primary_value, na.rm = TRUE)) %>%
                pull(total)
            }
            
            # PASO 2: Procesar datos SIN W00 y obtener TODOS los destinos
            todos_destinos <- datos_pais %>%
              filter(partner_iso != "W00") %>%  # ✅ EXCLUIR W00
              group_by(partner_iso) %>%
              summarise(
                valor_exportado = sum(primary_value, na.rm = TRUE),
                .groups = "drop"
              ) %>%
              mutate(
                reporter = pais,
                cuota_exportacion = if(total_exportado > 0) {
                  (valor_exportado / total_exportado) * 100
                } else {
                  NA
                }
              ) %>%
              arrange(desc(valor_exportado))
            
            # PASO 3: Guardar metadatos
            num_destinos_total <- nrow(todos_destinos)
            pos_espana <- which(todos_destinos$partner_iso == "ESP")
            valor_espana <- if(length(pos_espana) > 0) todos_destinos$valor_exportado[pos_espana] else NA
            
            # PASO 4: TOMAR SOLO TOP 5 PARA GRAFICAR
            top5_destinos <- todos_destinos %>%
              head(5)  # ✅ SOLO 5 DESTINOS
            
            # Agregar metadatos como atributos
            attr(top5_destinos, "total_exportado") <- total_exportado
            attr(top5_destinos, "num_destinos_total") <- num_destinos_total
            attr(top5_destinos, "pos_espana") <- if(length(pos_espana) > 0) pos_espana else NA
            attr(top5_destinos, "valor_espana") <- valor_espana
            
            lista_resultados[[pais]] <- top5_destinos
            
            message(sprintf("    ✓ Total: %.1fM$, %d destinos totales, top 5 guardado", 
                            total_exportado/1e6, num_destinos_total))
          }
          
          Sys.sleep(0.15) # Rate limiting
        }
        
        # Combinar todos los resultados
        if(length(lista_resultados) > 0) {
          message(sprintf("✓ Datos de exportación descargados para %d países", length(lista_resultados)))
          return(lista_resultados)
        } else {
          message("⚠ No se pudieron obtener datos de exportación")
          return(NULL)
        }
        
      }, error = function(e) {
        message(sprintf("✗ Error descargando datos: %s", e$message))
        showNotification(
          paste("No se pudieron obtener datos de exportación de Comtrade:", e$message),
          type = "warning",
          duration = 10
        )
        return(NULL)
      })
    })
    
    # Outputs de métricas
    output$pct_absorbido <- renderText({
      req(resultado_sustitucion())
      paste0(round(resultado_sustitucion()$pct_absorbido, 1), "%")
    })
    
    output$perdida_neta <- renderText({
      req(resultado_sustitucion())
      scales::number(resultado_sustitucion()$perdida_neta, scale = 1e-6, suffix = " M€", accuracy = 0.1)
    })
    
    output$hhi_nuevo <- renderText({
      req(resultado_sustitucion())
      round(resultado_sustitucion()$hhi, 0)
    })
    
    output$num_beneficiados <- renderText({
      req(resultado_sustitucion())
      resultado_sustitucion()$num_beneficiados
    })
    
    output$grafico_sustitucion <- renderPlotly({
      req(resultado_sustitucion())
      df_plot <- resultado_sustitucion()$detalle %>%
        arrange(desc(valor_final)) %>%
        head(15)
      
      plot_ly(df_plot, y = ~reorder(iso3a, valor_final)) %>%
        add_bars(x = ~valor_12m / 1e6, name = "Original", marker = list(color = "#90CAF9")) %>%
        add_bars(x = ~incremento / 1e6, name = "Sustitución", marker = list(color = "#4CAF50")) %>%
        layout(
          barmode = "stack",
          xaxis = list(title = "Millones €"),
          yaxis = list(title = ""),
          title = "Top 15 Proveedores Post-Sustitución"
        )
    })
    
    output$tabla_redistribucion <- renderDT({
      req(resultado_sustitucion())
      resultado_sustitucion()$detalle %>%
        arrange(desc(incremento)) %>%
        select(País = iso3a,
               `Valor Original` = valor_12m,
               `Incremento` = incremento,
               `Valor Final` = valor_final,
               `Cambio %` = cambio_pct) %>%
        datatable(
          options = list(pageLength = 15, dom = 'ftp'),
          rownames = FALSE
        ) %>%
        formatCurrency(c('Valor Original', 'Incremento', 'Valor Final'), "€") %>%
        formatRound('Cambio %', 1)
    })
    
    # NUEVO: Renderizado del análisis de exportaciones - USANDO TOP 5
    output$analisis_exportaciones <- renderUI({
      datos_lista <- analisis_exportaciones_data()
      
      if(is.null(datos_lista)) {
        return(div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          " No se pudieron obtener datos de exportación desde Comtrade. ",
          "Asegúrese de tener configurado un token válido con ",
          tags$code("Sys.setenv(COMTRADE_TOKEN = 'su_token')")
        ))
      }
      
      # Obtener top 5 actuales
      top5 <- resultado_sustitucion()$detalle %>%
        arrange(desc(valor_final)) %>%
        head(5) %>%
        pull(iso3a)
      
      # Crear visualizaciones para cada país
      lapply(top5, function(pais) {
        top5_data <- datos_lista[[pais]]
        
        if(is.null(top5_data) || nrow(top5_data) == 0) {
          return(NULL)
        }
        
        # Obtener metadatos
        total_exportado <- attr(top5_data, "total_exportado")
        num_destinos <- attr(top5_data, "num_destinos_total")
        pos_espana <- attr(top5_data, "pos_espana")
        valor_espana <- attr(top5_data, "valor_espana")
        
        # Calcular HHI con los datos del top 5
        hhi_exportaciones <- sum(top5_data$cuota_exportacion^2, na.rm = TRUE)
        
        # Información de España
        info_espana <- if(!is.na(pos_espana) && !is.na(valor_espana)) {
          cuota_esp <- (valor_espana / total_exportado) * 100
          paste0("#", pos_espana, " - ",
                 scales::number(valor_espana, scale = 1e-6, suffix = "M$"),
                 " (", round(cuota_esp, 1), "%)")
        } else {
          "No exporta este producto a España"
        }
        
        div(
          style = "margin-bottom: 30px; border-left: 4px solid #3b82f6; padding-left: 15px;",
          
          h4(style = "color: #1e3a8a; margin-bottom: 15px;",
             icon("flag"), " ", pais),
          
          fluidRow(
            column(3, div(
              style = "background: #f8f9fa; padding: 10px; border-radius: 8px; text-align: center;",
              p(style = "margin: 0; font-size: 0.8rem; color: #64748b;", "Total Exportado (2024)"),
              p(style = "margin: 5px 0 0 0; font-size: 1.2rem; font-weight: 600; color: #1e3a8a;",
                scales::number(total_exportado, scale = 1e-6, suffix = "M$", accuracy = 0.1))
            )),
            column(3, div(
              style = "background: #f8f9fa; padding: 10px; border-radius: 8px; text-align: center;",
              p(style = "margin: 0; font-size: 0.8rem; color: #64748b;", "Nº Destinos"),
              p(style = "margin: 5px 0 0 0; font-size: 1.2rem; font-weight: 600; color: #1e3a8a;",
                num_destinos)
            )),
            column(3, div(
              style = "background: #f8f9fa; padding: 10px; border-radius: 8px; text-align: center;",
              p(style = "margin: 0; font-size: 0.8rem; color: #64748b;", "HHI Exportaciones"),
              p(style = "margin: 5px 0 0 0; font-size: 1.2rem; font-weight: 600; color: #1e3a8a;",
                round(hhi_exportaciones, 0))
            )),
            column(3, div(
              style = "background: #dbeafe; padding: 10px; border-radius: 8px; text-align: center;",
              p(style = "margin: 0; font-size: 0.8rem; color: #1e40af;", "🇪🇸 España"),
              p(style = "margin: 5px 0 0 0; font-size: 0.9rem; font-weight: 600; color: #1e40af;",
                info_espana)
            ))
          ),
          
          div(style = "margin-top: 15px;",
              renderPlotly({
                # top5_data ya contiene solo 5 países sin W00
                plot_ly(
                  data = top5_data,
                  x = ~valor_exportado / 1e6,
                  y = ~reorder(partner_iso, valor_exportado),
                  type = "bar",
                  marker = list(
                    color = ifelse(top5_data$partner_iso == "ESP", "#3b82f6", "#90CAF9")
                  ),
                  text = ~paste0(partner_iso, ": ", round(cuota_exportacion, 1), "%"),
                  hovertemplate = "%{text}<br>$%{x:.1f}M<extra></extra>"
                ) %>%
                  layout(
                    title = list(text = "Top 5 Destinos de Exportación (2024)", font = list(size = 12)),
                    xaxis = list(title = "Millones $"),
                    yaxis = list(title = ""),
                    height = 280,
                    margin = list(l = 50, r = 20, t = 40, b = 40)
                  )
              })
          ),
          
          hr()
        )
      })
    })
  })
}