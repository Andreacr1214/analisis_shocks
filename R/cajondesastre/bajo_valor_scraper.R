# bajo_valor_scraper.R - Descarga y análisis de productos bajo vigilancia (bajo valor)
# Fuente: http://ec.europa.eu/taxation_customs/dds2/surv/surv_consultation.jsp?Lang=en

# Librerías necesarias
library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(readr)

# Paso 1: Definir URL de la página de consulta
url <- "http://ec.europa.eu/taxation_customs/dds2/surv/surv_consultation.jsp?Lang=en"

# Paso 2: Leer la página y buscar tablas de productos bajo vigilancia
pagina <- read_html(url)

# Paso 3: Extraer tablas (puede requerir inspección manual de la estructura)
tablas <- pagina %>% html_table(fill = TRUE)

# Paso 4: Mostrar las tablas encontradas y guardar la primera como ejemplo
if (length(tablas) > 0) {
  print(names(tablas))
  print(head(tablas[[1]]))
  write_csv(tablas[[1]], "bajo_valor_tabla_ejemplo.csv")
} else {
  print("No se encontraron tablas en la página. Puede requerir scraping avanzado o interacción manual.")
}

# NOTA: Si la tabla no aparece, puede que la web use formularios dinámicos o JavaScript. En ese caso, se requerirá RSelenium o descarga manual.
