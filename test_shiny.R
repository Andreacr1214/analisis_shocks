library(shiny)
message("Testing Shiny Launch...")
app <- shinyApp(
  ui = fluidPage(h1("Hello World")),
  server = function(input, output) {}
)
print(app)
message("Shiny object created. Attempting run...")
# We won't actually block with runApp in a script unless intended, 
# but printing 'app' should show the object structure.
