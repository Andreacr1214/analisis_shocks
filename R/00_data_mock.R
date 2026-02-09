# R/00_data_mock.R
# Funciones para generar datos simulados cuando no hay conexión a BD

#' Generar metadatos iniciales simulados
#' @export
mock_info_inicial <- function() {
  fecha_max <- Sys.Date()
  # Use base R date arithmetic to avoid lubridate dependency issues
  fecha_inicio <- seq(fecha_max, by = "-11 months", length.out = 2)[2]
  
  list(
    fecha_max = fecha_max,
    desde = fecha_inicio,
    hasta = fecha_max,
    codigos = c("01012100", "87032319", "27090090", "30049000", "85171200"),
    label = paste0(format(fecha_inicio, "%b %Y"), " - ", format(fecha_max, "%b %Y"))
  )
}

#' Generar datos de importación simulados
#' @export
mock_cargar_datacomex <- function(codigo, desde, hasta) {
  fechas <- seq(from = desde, to = hasta, by = "month")
  paises <- c("CHN", "DEU", "FRA", "ITA", "USA", "TUR", "MAR", "NLD", "PRT", "GBR")
  
  df_list <- list()
  
  for(p in paises) {
    # Tendencia aleatoria
    base <- runif(1, 1e6, 50e6)
    trend <- runif(length(fechas), 0.8, 1.2)
    
    df_list[[p]] <- data.frame(
      iso3a = p,
      fecha = fechas,
      euros = base * trend,
      flujo = "I",
      codigo = codigo
    )
  }
  
  do.call(rbind, df_list)
}

#' Generar datos simulados de Eurostat
#' @export
mock_eurostat <- function(codigo, paises_ue) {
  fecha_inicio <- seq(Sys.Date(), by = "-11 months", length.out = 2)[2]
  fechas <- seq(fecha_inicio, Sys.Date(), by = "month")
  
  df_list <- list()
  # País del shock simulado (el primero de la lista mock si no se especifica)
  pais_shock <- "CHN" 
  
  for(p in paises_ue) {
    base_total <- runif(1, 10e6, 500e6)
    share_shock <- runif(1, 0.05, 0.4)
    
    df_list[[p]] <- data.frame(
      declarante = p,
      fecha = rep(fechas, each = 1),
      socio = c(rep(pais_shock, length(fechas)), rep("WORLD", length(fechas))),
      euros = c(
        rnorm(length(fechas), base_total * share_shock / 12, (base_total * share_shock / 12)*0.1),
        rnorm(length(fechas), base_total / 12, (base_total / 12)*0.1)
      )
    )
  }
  
  do.call(rbind, df_list)
}
