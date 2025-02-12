# remotes::install_github("surveydown-dev/surveydown", force = TRUE)
library(surveydown)
#setwd('/home/sagesteppe/Documents/assoRted/EstimatingSeedMarketSize/survey')

# Database setup
db <- sd_db_connect(
  gssencmode = NULL
)

# Server setup
server <- function(input, output, session) {

  # Define any conditional skip logic here (skip to page if a condition is true)
#  sd_skip_if(
   # input$respondent_DOI_region == 1 ~ "NAA_lifeform_area",
  #  input$respondent_DOI_region == 2 ~ "SAG_lifeform_area",
  #  input$respondent_DOI_region == 3 ~ "GL_lifeform_area",
  #  input$respondent_DOI_region == 4 ~ "Mississippi_lifeform_area",
  #  input$respondent_DOI_region == 5 ~ "Missouri_lifeform_area",
  #  input$respondent_DOI_region == 6 ~ "ARGTG_lifeform_area",
  #  input$respondent_DOI_region == 7 ~ "UCB_lifeform_area",
  #  input$respondent_DOI_region == 8 ~ "LCB_lifeform_area",
  #  input$respondent_DOI_region == 9 ~ "CPN_lifeform_area",
  #  input$respondent_DOI_region == 10 ~ "CGB_lifeform_area",
#    input$respondent_DOI_region == 11 ~ "Alaska_lifeform_area"#,
  #  input$respondent_DOI_region == 12 ~ "Pacific_lifeform_area"
#  )

  # Define any conditional display logic here (show a question if a condition is true)
  sd_show_if(
    input$respondentAgency == "other" ~ "respondentAgency_other",
    input$email_followup == 'Yes' ~ "respondent_email_address"
  )

  # Database designation and other settings
  sd_server(
    db = db, 
    use_cookies = FALSE, 
    rate_survey = TRUE
  )
}

# shinyApp() initiates your app - don't change it
shiny::shinyApp(ui = sd_ui(), server = server)
