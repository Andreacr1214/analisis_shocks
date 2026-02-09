# Script to launch the application explicitly
library(shiny)

# Ensure the app runs on a specific port and host if needed
# You can change port=3838 to any open port
runApp("app.R", launch.browser = TRUE, port = 3838, host = "0.0.0.0")
