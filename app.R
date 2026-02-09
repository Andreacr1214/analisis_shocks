# app.R - Main entry point

# 1. Load required libraries
library(shiny)
library(bslib)
library(tidyverse)
library(lubridate)
library(DBI)
library(plotly)
library(DT)
library(shinycssloaders)
library(scales)
library(devtools)

# 2. Load the project code
# We use devtools to simulate loading the package structure
# This sources everything in R/ and handles dependencies
if (requireNamespace("devtools", quietly = TRUE)) {
  # Try to load the sibling project 'comerciotools' if it exists
  if (dir.exists("../CVG/comerciotools")) {
    tryCatch({
      devtools::load_all("../CVG/comerciotools", quiet = TRUE)
      message("Loaded sibling project: comerciotools")
    }, error = function(e) {
      warning("Failed to load comerciotools: ", e$message)
    })
  }
  
  # Load the current project
  devtools::load_all(".", quiet = TRUE)
} else {
  stop("devtools package is required to run this app in development mode.")
}

# 3. Launch the application
message("Launching Analysis Shocks App...")

# Construct the app object
app <- vulnerabilidad_comercial_app()

# Explicitly run the app if this file is sourced
if (interactive()) {
  shiny::runApp(app)
} else {
  # If run from command line Rscript app.R
  shiny::runApp(app, launch.browser = TRUE, port = 3838)
}
