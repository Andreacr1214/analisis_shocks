get_css_custom <- function() {
  "
  @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');

  body {
    font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif;
    background: #f8f9fa;
    color: #1a1a1a;
  }

  .navbar {
    background: linear-gradient(135deg, #1e3a8a 0%, #3b82f6 100%) !important;
    box-shadow: 0 2px 12px rgba(0,0,0,0.08);
  }

  .navbar-brand {
    font-weight: 700;
    font-size: 1.3rem;
    letter-spacing: -0.5px;
    color: #ffffff !important;  /* ← TÍTULO BLANCO */
  }

  /* ← PESTAÑAS BLANCAS */
  .navbar .nav-link {
    color: rgba(255, 255, 255, 0.85) !important;
    font-weight: 500;
  }

  .navbar .nav-link:hover {
    color: #ffffff !important;
  }

  .navbar .nav-link.active {
    color: #ffffff !important;
    font-weight: 600;
  }

  /* Dropdown items del menú */
  .navbar .dropdown-menu {
    background-color: #ffffff;
  }

  .navbar .dropdown-item {
    color: #1e3a8a !important;
  }

  .navbar .dropdown-item:hover {
    background-color: #dbeafe;
    color: #1e3a8a !important;
  }

  .card {
    border: none;
    border-radius: 12px;
    box-shadow: 0 1px 3px rgba(0,0,0,0.08);
    transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
    background: white;
  }

  .card:hover {
    box-shadow: 0 8px 16px rgba(0,0,0,0.12);
    transform: translateY(-2px);
  }

  .card-header {
    background: linear-gradient(135deg, #f8fafc 0%, #ffffff 100%) !important;
    border-bottom: 1px solid #e2e8f0 !important;
    font-weight: 600;
    color: #334155;
    padding: 1rem 1.5rem;
  }

  .metric-card {
    background: white;
    border: 1px solid #e2e8f0;
    border-radius: 12px;
    padding: 24px;
    margin: 10px 0;
    height: 130px;
    display: flex;
    flex-direction: column;
    justify-content: center;
    transition: all 0.3s ease;
    position: relative;
    overflow: hidden;
  }

  .metric-card::before {
    content: '';
    position: absolute;
    top: 0;
    left: 0;
    width: 4px;
    height: 100%;
    background: linear-gradient(180deg, #3b82f6 0%, #1e3a8a 100%);
  }

  .metric-card:hover {
    border-color: #3b82f6;
    box-shadow: 0 4px 12px rgba(59, 130, 246, 0.15);
  }

  .metric-value {
    font-size: 2rem;
    font-weight: 700;
    color: #1e3a8a;
    margin: 0;
    line-height: 1.2;
    letter-spacing: -0.5px;
  }

  .metric-label {
    font-size: 0.75rem;
    color: #64748b;
    text-transform: uppercase;
    letter-spacing: 0.5px;
    margin-top: 8px;
    font-weight: 500;
  }

  .section-title {
    color: #1e293b;
    font-weight: 700;
    font-size: 1.75rem;
    margin-bottom: 24px;
    padding-bottom: 12px;
    border-bottom: 2px solid #e2e8f0;
    letter-spacing: -0.5px;
  }

  .info-box {
    background: linear-gradient(135deg, #eff6ff 0%, #dbeafe 100%);
    border-left: 4px solid #3b82f6;
    padding: 18px 20px;
    margin: 20px 0;
    border-radius: 8px;
    color: #1e40af;
    font-size: 0.95rem;
    line-height: 1.6;
  }

  .btn-primary {
    background: linear-gradient(135deg, #3b82f6 0%, #2563eb 100%);
    border: none;
    border-radius: 8px;
    font-weight: 600;
    padding: 12px 24px;
    transition: all 0.3s ease;
    box-shadow: 0 2px 4px rgba(59, 130, 246, 0.2);
  }

  .btn-primary:hover {
    background: linear-gradient(135deg, #2563eb 0%, #1e40af 100%);
    transform: translateY(-2px);
    box-shadow: 0 6px 12px rgba(59, 130, 246, 0.3);
  }

  .nav-underline .nav-link.active {
    color: #1e3a8a;
    border-bottom-color: #3b82f6;
    font-weight: 600;
  }

  .nav-underline .nav-link {
    color: #64748b;
    font-weight: 500;
    transition: all 0.2s ease;
  }

  .nav-underline .nav-link:hover {
    color: #3b82f6;
  }

  h2, h3, h4 {
    color: #1e293b;
    font-weight: 600;
  }

  /* Home page styles */
  .hero-section {
    background: linear-gradient(135deg, #1e3a8a 0%, #3b82f6 100%);
    color: white;
    padding: 60px 40px;
    border-radius: 16px;
    margin-bottom: 40px;
    box-shadow: 0 10px 30px rgba(30, 58, 138, 0.2);
  }

  .hero-title {
    font-size: 2.5rem;
    font-weight: 700;
    margin-bottom: 20px;
    letter-spacing: -1px;
  }

  .hero-subtitle {
    font-size: 1.2rem;
    opacity: 0.95;
    line-height: 1.6;
    font-weight: 300;
  }

  .feature-card {
    background: white;
    border-radius: 12px;
    padding: 32px;
    margin: 16px 0;
    border: 1px solid #e2e8f0;
    transition: all 0.3s ease;
  }

  .feature-card:hover {
    border-color: #3b82f6;
    box-shadow: 0 8px 24px rgba(59, 130, 246, 0.12);
    transform: translateY(-4px);
  }

  .feature-icon {
    font-size: 2.5rem;
    margin-bottom: 16px;
  }

  .feature-title {
    font-size: 1.3rem;
    font-weight: 700;
    color: #1e293b;
    margin-bottom: 12px;
  }

  .feature-description {
    color: #64748b;
    line-height: 1.7;
    font-size: 0.95rem;
  }

  .approach-badge {
    display: inline-block;
    background: linear-gradient(135deg, #3b82f6 0%, #2563eb 100%);
    color: white;
    padding: 6px 14px;
    border-radius: 20px;
    font-size: 0.8rem;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.5px;
    margin-bottom: 8px;
  }
  "
}

#' @param id 
#' @export
home_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::div(class = "container-fluid mt-4",
             
               shiny::div(class = "hero-section",
                          shiny::div(class = "hero-title", "Simulador de Shocks Comerciales"),
                          shiny::div(class = "hero-subtitle",
                                     "Herramienta de análisis para evaluar el impacto de disrupciones en las cadenas de suministro y la vulnerabilidad comercial de España")
               ),
               
              
               shiny::fluidRow(
                 shiny::column(12,
                               shiny::div(class = "info-box",
                                          shiny::p(style = "margin-bottom: 12px; font-weight: 600; font-size: 1.05rem;",
                                                   "¿Qué hace esta herramienta?"),
                                          shiny::p("Este simulador permite analizar cómo las disrupciones en el comercio internacional afectan a la economía española.
                       Mediante dos enfoques complementarios, se puede evaluar el impacto de shocks comerciales, identificar vulnerabilidades
                       y explorar escenarios de sustitución de proveedores."))
                 )
               ),
               
            
               shiny::fluidRow(
                 shiny::column(6,
                               shiny::div(class = "feature-card",
                                          shiny::div(class = "feature-icon", "🎯"),
                                          shiny::div(class = "approach-badge", "Enfoque Producto"),
                                          shiny::div(class = "feature-title", "Análisis por Producto"),
                                          shiny::div(class = "feature-description",
                                                     shiny::tags$ul(style = "margin-top: 12px;",
                                                                    shiny::tags$li("Seleccione un producto específico (código TARIC)"),
                                                                    shiny::tags$li("Analice la concentración de proveedores"),
                                                                    shiny::tags$li("Simule shocks en países proveedores específicos"),
                                                                    shiny::tags$li("Evalúe el impacto directo y con sustitución"),
                                                                    shiny::tags$li(shiny::tags$strong("Ideal para:"), " análisis de vulnerabilidad de productos estratégicos o críticos")
                                                     )
                                          )
                               )
                 ),
                 shiny::column(6,
                               shiny::div(class = "feature-card",
                                          shiny::div(class = "feature-icon", "🌍"),
                                          shiny::div(class = "approach-badge", "Enfoque País"),
                                          shiny::div(class = "feature-title", "Análisis por País"),
                                          shiny::div(class = "feature-description",
                                                     shiny::tags$ul(style = "margin-top: 12px;",
                                                                    shiny::tags$li("Seleccione un país proveedor"),
                                                                    shiny::tags$li("Identifique qué productos importa España de ese país"),
                                                                    shiny::tags$li("Analice la dependencia agregada"),
                                                                    shiny::tags$li("Simule el impacto de un shock generalizado"),
                                                                    shiny::tags$li(shiny::tags$strong("Ideal para:"), " análisis de riesgo geopolítico o disrupciones país-específicas")
                                                     )
                                          )
                               )
                 )
               ),
               
               # Capacidades clave
               shiny::fluidRow(
                 shiny::column(4,
                               shiny::div(class = "feature-card",
                                          shiny::div(class = "feature-icon", "📈"),
                                          shiny::div(class = "feature-title", "Métricas de Concentración"),
                                          shiny::div(class = "feature-description",
                                                     "Índice HHI, cuotas de mercado, número de proveedores y análisis temporal de la evolución de las importaciones."
                                          )
                               )
                 ),
                 shiny::column(4,
                               shiny::div(class = "feature-card",
                                          shiny::div(class = "feature-icon", "⚡"),
                                          shiny::div(class = "feature-title", "Simulación de Shocks"),
                                          shiny::div(class = "feature-description",
                                                     "Configure la magnitud del shock (0-100%) y visualice el impacto inmediato en las importaciones y la concentración del mercado."
                                          )
                               )
                 ),
                 shiny::column(4,
                               shiny::div(class = "feature-card",
                                          shiny::div(class = "feature-icon", "🔄"),
                                          shiny::div(class = "feature-title", "Análisis de Sustitución"),
                                          shiny::div(class = "feature-description",
                                                     "Explore escenarios de sustitución basados en capacidad exportadora, proximidad geográfica y similitud comercial."
                                          )
                               )
                 )
               ),
               
         
               shiny::fluidRow(
                 shiny::column(12, style = "text-align: center; margin: 40px 0;",
                               shiny::h4("Seleccione un enfoque en el menú superior para comenzar",
                                         style = "color: #64748b; font-weight: 400;")
                 )
               )
    )
  )
}



#' @param ... argumentos adicionales
#' @export
vulnerabilidad_comercial_app <- function(...) {
  
  message(getwd())
  
  ui <- bslib::page_navbar(
    title = shiny::HTML("<span style='font-weight: 700;'>Simulador de Shocks Comerciales</span>"),
    theme = bslib::bs_theme(
      version = 5,
      bg = "#ffffff",
      fg = "#1a1a1a",
      primary = "#3b82f6",
      secondary = "#64748b",
      base_font = bslib::font_google("Inter")
    ),
    header = shiny::tags$head(
      shiny::tags$style(shiny::HTML(get_css_custom()))
    ),
    
   
    bslib::nav_panel(
      title = "Inicio",
      icon = shiny::icon("home"),
      home_ui("home")
    ),
    

    bslib::nav_menu(
      title = "Enfoque: Producto",
      icon = shiny::icon("box"),
      bslib::nav_panel(
        title = "Contexto del Producto",
        icon = shiny::icon("chart-line"),
        contexto_producto_ui("contexto")
      ),
      bslib::nav_panel(
        title = "Definición del Shock",
        icon = shiny::icon("bolt"),
        configuracion_shock_ui("shock_config")
      ),
      bslib::nav_panel(
        title = "Impacto con Sustitución",
        icon = shiny::icon("arrows-rotate"),
        sustitucion_proveedores_ui("sustitucion")
      ),
      bslib::nav_panel(
        title = "Comparativa UE",
        icon = shiny::icon("map-location-dot"),
        comparativa_eurostat_ui("comparativa")
      )
    ),

    bslib::nav_panel(
      title = "Enfoque: País",
      icon = shiny::icon("globe"),
      enfoque_pais_ui("enfoque_pais")
    )
  )
  
  server <- function(input, output, session) {

    # Check for DB connection requirements
    required_vars <- c("POSTGRESQL_SGEYPC_USER", "POSTGRESQL_SGEYPC_PASSWORD", "SERVIDOR_SGEYPC")
    missing_vars <- required_vars[sapply(required_vars, function(v) nchar(Sys.getenv(v)) == 0)]
    
    if(length(missing_vars) > 0) {
      msg <- paste("Faltan variables de entorno para BD:", paste(missing_vars, collapse=", "))
      showNotification(msg, type = "error", duration = NULL)
      message("⚠ ", msg)
    }

    # Connect to database — required for real data
    conexion_db <- tryCatch({
      conn <- comerciotools::crear_conexion_pg()
      message("✓ Conexión a BD establecida correctamente.")
      conn
    }, error = function(e) {
      showNotification(
        paste("Error de conexión a BD:", e$message), 
        type = "error", 
        duration = NULL
      )
      message("✗ Fallo conexión BD: ", e$message)
      NULL
    })
    
    shiny::onStop(function() {
      if(!is.null(conexion_db)) {
        message("Cerrando conexión a BD")
        DBI::dbDisconnect(conexion_db)
      }
    })
    
    contexto <- contexto_producto_server("contexto", conexion_db = conexion_db)
    
    shock_configurado <- configuracion_shock_server(
      "shock_config",
      contexto_data = contexto
    )
    
    tryCatch({
      impacto_directo_server(
        "impacto",
        conexion_db = conexion_db,
        contexto_data = contexto,
        shock_data = shock_configurado
      )
    }, error = function(e) message("Módulo impacto error: ", e$message))
    
    tryCatch({
      sustitucion_proveedores_server(
        "sustitucion",
        conexion_db = conexion_db,
        contexto_data = contexto,
        shock_data = shock_configurado
      )
    }, error = function(e) message("Módulo sustitucion error: ", e$message))
    
    tryCatch({
      comparativa_eurostat_server(
        "comparativa",
        contexto_data = contexto,
        shock_data = shock_configurado
      )
    }, error = function(e) message("Módulo comparativa error: ", e$message))
    
    tryCatch({
      enfoque_pais_server("enfoque_pais", conexion_db = conexion_db)
    }, error = function(e) message("Módulo enfoque_pais error: ", e$message))
  }
  
  port <- as.numeric(Sys.getenv("SHINY_PORT", "3838"))
  
  shiny::shinyApp(
    ui = ui,
    server = server,
    options = list(port = port)
  )
}

#devulnerabilidad_comercial_app()
