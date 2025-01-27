library(surveydown)

db <- sd_database(
  host   = "",
  dbname = "",
  port   = "",
  user   = "",
  table  = "",
  ignore = TRUE
)

server <- function(input, output, session) {

  # Define conditional skip logic here (skip to page if a condition is true)
  sd_skip_if()

  # Define  conditional display logic here (show a question if a condition is true)
  sd_show_if()

  # Main server to control the app
  sd_server()
}

shiny::shinyApp(ui = sd_ui(), server = server)
