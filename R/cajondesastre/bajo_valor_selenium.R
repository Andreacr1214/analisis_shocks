# bajo_valor_selenium.R - Scraping avanzado con RSelenium para productos bajo vigilancia
# Fuente: http://ec.europa.eu/taxation_customs/dds2/surv/surv_consultation.jsp?Lang=en

# Librerías necesarias
library(RSelenium)
library(dplyr)
library(readr)
library(stringr)

# Paso 1: Iniciar el servidor Selenium (requiere Docker o Selenium Server)
# Si tienes Docker instalado, puedes lanzar Selenium con:
# docker run -d -p 4445:4444 selenium/standalone-chrome

# Paso 2: Conectar con el navegador
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "chrome")
remDr$open()

# Paso 3: Navegar a la página
url <- "http://ec.europa.eu/taxation_customs/dds2/surv/surv_consultation.jsp?Lang=en"
remDr$navigate(url)
Sys.sleep(5) # Esperar a que cargue la página

# Paso 4: Extraer el HTML de la página cargada
doc <- remDr$getPageSource()[[1]]
library(rvest)
pagina <- read_html(doc)

# Paso 5: Buscar tablas en la página
tablas <- pagina %>% html_table(fill = TRUE)

# Paso 6: Guardar la primera tabla encontrada (si existe)
if (length(tablas) > 0) {
  print(names(tablas))
  print(head(tablas[[1]]))
  write_csv(tablas[[1]], "bajo_valor_tabla_selenium.csv")
} else {
  print("No se encontraron tablas en la página. Puede requerir interacción adicional.")
}

# Paso 7: Cerrar el navegador
remDr$close()

# NOTA: Si la tabla requiere interacción (selección de año, producto, etc.), se pueden automatizar clicks y formularios con RSelenium.
