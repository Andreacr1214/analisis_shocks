# ============================================================================
# ayuda.R - Módulo de Ayuda / Documentación
# ============================================================================

ayuda_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(class = "container-fluid mt-4",
        
        # Cabecera
        fluidRow(
          column(12,
                 h2(class = "section-title", "❓ Ayuda y Documentación"),
                 div(class = "info-box",
                     "Guía completa del Simulador de Shocks Comerciales: objetivo, datos, metodología e interpretación de resultados.")
          )
        ),
        
        # --- Objetivo ---
        fluidRow(
          column(12,
                 card(
                   card_header(class = "bg-light", "🎯 Objetivo de la Herramienta"),
                   card_body(
                     p("El ", tags$strong("Simulador de Shocks Comerciales"), " permite analizar cómo las disrupciones
                       en el comercio internacional afectan a la economía española. La herramienta ofrece dos enfoques complementarios:"),
                     tags$ul(
                       tags$li(tags$strong("Enfoque Producto:"),
                               " Seleccione un producto específico (código TARIC) y simule un shock en uno o todos sus proveedores.
                               Se analiza la concentración de proveedores, el impacto directo, los posibles escenarios de sustitución,
                               y se compara la vulnerabilidad de España con otros países de la UE."),
                       tags$li(tags$strong("Enfoque País:"),
                               " Seleccione un país proveedor y simule una reducción generalizada de las importaciones desde ese origen.
                               Se identifican los productos más afectados y los de mayor dependencia a nivel HS6.")
                     )
                   )
                 )
          )
        ),
        
        # --- Fuentes de datos ---
        fluidRow(
          column(12,
                 card(
                   card_header(class = "bg-light", "📂 Fuentes de Datos"),
                   card_body(
                     tags$ul(
                       tags$li(tags$strong("Datacomex (Agencia Tributaria / AEAT):"),
                               " Datos de comercio exterior de España a nivel de producto TARIC y país de origen.
                               Se utilizan los últimos 12 meses disponibles. La base de datos se actualiza mensualmente
                               con un desfase de aproximadamente 2 meses.",
                               tags$br(),
                               tags$em("Se usa en: Contexto del Producto, Definición del Shock, Impacto con Sustitución y Enfoque País.")),
                       tags$li(tags$strong("Eurostat (Comext):"),
                               " Datos de comercio intra y extracomunitario de todos los países miembros de la UE.
                               Se utilizan para comparar la dependencia de España con sus socios europeos.
                               Los datos de Eurostat se cargan a través del paquete ", tags$code("comerciotools"), ".",
                               tags$br(),
                               tags$em("Se usa exclusivamente en: Comparativa UE."))
                     ),
                     div(class = "alert alert-warning", style = "margin-top: 12px;",
                         icon("triangle-exclamation"),
                         " Los datos de Eurostat solo cubren países de la UE. Por lo tanto, la comparativa internacional
                         y el análisis de alcance diferenciado se limitan a países europeos. No es posible cuantificar el
                         impacto en terceros países fuera de la UE con esta herramienta.")
                   )
                 )
          )
        ),
        
        # --- Flujo de trabajo (Producto) ---
        fluidRow(
          column(12,
                 card(
                   card_header(class = "bg-light", "📋 Flujo de Trabajo — Enfoque Producto"),
                   card_body(
                     tags$ol(
                       tags$li(tags$strong("Contexto del Producto:"),
                               " Seleccione un código TARIC. Se cargan automáticamente los proveedores, sus cuotas de mercado,
                               el índice HHI de concentración, y la evolución histórica de importaciones desde 2019."),
                       tags$li(tags$strong("Definición del Shock:"),
                               tags$ul(
                                 tags$li("Elija el ", tags$strong("tipo de shock"), ": un país proveedor específico o todos los proveedores a la vez."),
                                 tags$li("Configure el ", tags$strong("porcentaje de caída"), " de las importaciones."),
                                 tags$li("Defina el ", tags$strong("alcance geográfico"), ": ¿afecta igual a todos los países de la UE, solo a España, o de forma personalizada?"),
                                 tags$li("Pulse ", tags$strong("'Aplicar y Calcular'"), " para ver el impacto directo.")
                               )),
                       tags$li(tags$strong("Impacto con Sustitución:"),
                               " Analiza 5 escenarios de redistribución de las importaciones perdidas entre proveedores alternativos:
                               sin sustitución, proporcional, limitada por capacidad, países amigos (UE/OCDE) y por proximidad geográfica."),
                       tags$li(tags$strong("Comparativa UE:"),
                               " Compara la dependencia de España frente al país del shock con la de otros 26 miembros de la UE (datos Eurostat).
                               Cuando el alcance no es uniforme, se visualiza el impacto diferenciado por país.")
                     )
                   )
                 )
          )
        ),
        
        # --- Alcance geográfico ---
        fluidRow(
          column(12,
                 card(
                   card_header(class = "bg-light", "🌐 Alcance Geográfico del Shock"),
                   card_body(
                     p("No todos los shocks comerciales afectan a todos los países por igual. El simulador permite configurar
                       cuatro modos de alcance. ", tags$strong("Importante:"), " dado que los datos comparativos provienen de Eurostat,
                       el análisis se limita a la UE-27."),
                     tags$table(class = "table table-bordered",
                       tags$thead(
                         tags$tr(tags$th("Modo"), tags$th("España"), tags$th("Resto UE"), tags$th("Ejemplo"))
                       ),
                       tags$tbody(
                         tags$tr(
                           tags$td(tags$strong("Igual para toda la UE")),
                           tags$td("X%"), tags$td("X%"),
                           tags$td("Disrupción en origen (guerra, catástrofe) que reduce la oferta global del país exportador.")
                         ),
                         tags$tr(
                           tags$td(tags$strong("Específico España")),
                           tags$td("X%"), tags$td("0%"),
                           tags$td("Represalia bilateral contra España, arancel específico, conflicto diplomático.")
                         ),
                         tags$tr(
                           tags$td(tags$strong("Personalizado")),
                           tags$td("X%"), tags$td("Y%"),
                           tags$td("España sujeta a arancel mayor que el resto de la UE, o shock asimétrico por contratos preferentes.")
                         )
                       )
                     ),
                     p(class = "text-muted small",
                       "En el modo personalizado, se pueden definir porcentajes diferentes para España y el resto de la UE.")
                   )
                 )
          )
        ),
        
        # --- Métricas ---
        fluidRow(
          column(12,
                 card(
                   card_header(class = "bg-light", "📊 Interpretación de Métricas"),
                   card_body(
                     tags$dl(
                       tags$dt("Índice HHI (Herfindahl-Hirschman)"),
                       tags$dd("Mide la concentración de proveedores. Rango de 0 a 10.000. ",
                               "Un HHI < 1.500 indica baja concentración (mercado diversificado). ",
                               "Entre 1.500 y 2.500, concentración moderada. ",
                               "Por encima de 2.500, alta concentración (pocos proveedores dominantes)."),
                       
                       tags$dt("Cuota de mercado (%)"),
                       tags$dd("Porcentaje del valor total importado de un producto que proviene de un país determinado."),
                       
                       tags$dt("Dependencia (%)"),
                       tags$dd("En la comparativa UE, porcentaje de las importaciones totales de cada país miembro que provienen ",
                               "del país afectado por el shock. Una dependencia alta implica mayor vulnerabilidad."),
                       
                       tags$dt("Pérdida estimada"),
                       tags$dd("Valor de las importaciones que se perderían según el porcentaje de shock configurado. ",
                               "Es un cálculo de impacto directo, sin considerar sustitución ni redireccionamiento comercial."),
                       
                       tags$dt("Δ HHI"),
                       tags$dd("Variación del índice HHI tras el shock. Si el país afectado era un proveedor importante, ",
                               "el nuevo HHI puede subir (más concentración en los restantes) o bajar (si se diversifica)."),
                       
                       tags$dt("Δ Competitivo"),
                       tags$dd("En la comparativa UE, diferencia entre la pérdida relativa de España y la media de la UE. ",
                               "Un valor negativo indica que España pierde más que la media; positivo, que pierde menos.")
                     )
                   )
                 )
          )
        ),
        
        # --- Nivel HS6 ---
        fluidRow(
          column(12,
                 card(
                   card_header(class = "bg-light", "🏷️ Clasificación de Productos"),
                   card_body(
                     tags$ul(
                       tags$li(tags$strong("TARIC:"), " Nomenclatura arancelaria de la UE. Los códigos van de 2 a 10 dígitos.
                               En el 'Enfoque Producto' se puede seleccionar cualquier nivel de agregación."),
                       tags$li(tags$strong("HS6 (Sistema Armonizado, 6 dígitos):"), " Máximo nivel de desagregación armonizado
                               internacionalmente. En el 'Enfoque País' se muestran resultados exclusivamente a nivel HS6
                               para garantizar comparabilidad internacional y evitar mezclar niveles de agregación."),
                       tags$li(tags$strong("Secciones y capítulos:"), " Los 2 primeros dígitos identifican el capítulo
                               (p.ej. 27 = combustibles, 87 = vehículos). Los 4 primeros identifican la partida.")
                     )
                   )
                 )
          )
        ),
        
        # --- Limitaciones ---
        fluidRow(
          column(12,
                 card(
                   card_header(class = "bg-warning text-dark", "⚠️ Limitaciones y Caveats"),
                   card_body(
                     tags$ul(
                       tags$li(tags$strong("Impacto directo:"), " El simulador calcula el impacto mecánico directo de la caída
                               en importaciones. No modela efectos de segunda ronda (sustitución real de proveedores, ajustes de precios,
                               relocalización industrial, acumulación de inventarios, etc.)."),
                       tags$li(tags$strong("Escenarios de sustitución simplificados:"), " Los 5 escenarios de sustitución son heurísticos
                               basados en capacidad exportadora y proximidad, no en modelos de equilibrio general o elasticidades estimadas."),
                       tags$li(tags$strong("Datos retrospectivos:"), " Se utilizan datos de los últimos 12 meses disponibles.
                               No se proyecta el crecimiento futuro del comercio ni se ajusta por estacionalidad."),
                       tags$li(tags$strong("Solo UE en comparativa:"), " La comparativa internacional se limita a los 27 miembros de la UE
                               dado que se basa en datos de Eurostat. No se incluyen terceros países."),
                       tags$li(tags$strong("Valores nominales:"), " Todos los valores están en euros corrientes, sin ajuste por inflación
                               ni tipo de cambio."),
                       tags$li(tags$strong("Comercio de bienes:"), " Solo se analizan importaciones de bienes (mercancías). No se incluyen
                               servicios ni inversión extranjera directa."),
                       tags$li(tags$strong("Latencia de datos:"), " Los datos de Datacomex se publican con aproximadamente 2 meses de desfase.
                               Los datos de Eurostat pueden tener un desfase mayor (hasta 3-4 meses).")
                     )
                   )
                 )
          )
        ),
        
        # --- Créditos ---
        fluidRow(
          column(12, style = "text-align: center; margin: 30px 0; color: #94a3b8;",
                 p("Simulador de Shocks Comerciales — Subdirección General de Estudios y Evaluación de Política Comercial"),
                 p(style = "font-size: 0.85rem;", "Desarrollado con R, Shiny y comerciotools. Datos: Datacomex (AEAT), Eurostat (Comext).")
          )
        )
    )
  )
}
